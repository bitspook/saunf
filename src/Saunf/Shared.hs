{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Saunf.Shared where

import Data.List (intersect)
import Data.Text (toUpper)
import Relude
import Saunf.Types
import qualified Data.Map as M
import qualified Text.Pandoc as P hiding (Reader)
import qualified Text.Pandoc.Shared as P
import qualified Text.Parsec as Parsec

hasId :: Text -> P.Block -> Bool
hasId cid b = case b of
  (P.Header _ (cid', _, _) _) -> cid' == cid
  _ -> False

-- TODO: Write tests for me please
-- TODO: Take "category" as an argument perhaps, make this hasProperty instead.
-- There is a good chance that such a function already exists in P.Pandoc
hasCategory :: Text -> P.Block -> Bool
hasCategory cat block = case block of
  (P.Header _ (_, _, props) _) -> any (\p -> fst p == "category" && snd p == cat) props
  _ -> True

-- TODO: Write tests for me please
isHeaderWithLevel :: Int -> P.Block -> Bool
isHeaderWithLevel lvl block = case block of
  (P.Header lvl' _ _) -> lvl == lvl'
  _ -> False

filterSections :: (MonadReader e m, HasSaunfDoc e) => (P.Block -> Bool) -> m [Section]
filterSections headerMatcher = do
  P.Pandoc _ bs <- asks getSaunfDoc
  case dropWhile isNotMatch bs of
    h@(P.Header level _ _) : xs -> do
      let content = takeWhile (isSectionContent level) xs
      let leftover = P.Pandoc mempty $ dropWhile (isSectionContent level) xs
      remainingContent <- local (setSaunfDoc leftover) $ filterSections headerMatcher
      return $ Section h content : remainingContent
    _ -> return []
  where
    isNotMatch x = case x of
      P.Header {} -> not . headerMatcher $ x
      _ -> True
    isNotHeaderOfLevel lvl x = case x of
      (P.Header lvl' _ _) -> lvl < lvl'
      _ -> False
    isSectionContent level x = (not . P.isHeaderBlock) x || isNotHeaderOfLevel level x

sectionsWithProperties :: (MonadReader e m, HasSaunfDoc e) => [(Text, Text)] -> m [Section]
sectionsWithProperties [] = return []
sectionsWithProperties props = filterSections byProps
  where
    byProps h = case h of
      (P.Header _ (_, _, props') _) -> props `intersect` props' == props
      _ -> False

-- | Adjust the level of a section to given level. This will make the first
-- P.Header in section of the given level, and every sub-Header's level
-- will be shifted accordingly
setSectionHeaderLevel :: Int -> Section -> Section
setSectionHeaderLevel n (Section h xs) = case h of
  (P.Header sectionLvl attr inner) -> Section (P.Header n attr inner) (shiftHeader <$> xs)
    where
      delta = n - sectionLvl
      shiftHeader b = case b of
        P.Header lvl a i -> P.Header (lvl + delta) a i
        _ -> b
  _ -> Section h xs

-- Nasty little helper to quickly render shit to markdown
writeMd ::
  ( P.PandocMonad m,
    MonadReader e m,
    HasSaunfDoc e
  ) =>
  [P.Block] ->
  m Text
writeMd bollocks = do
  (P.Pandoc meta _) <- asks getSaunfDoc
  P.writeMarkdown P.def $ P.Pandoc meta bollocks

-- | Move following org-mode directives from P.Header text to attrs:
-- * Todo markers
-- * Tags
-- We rely on P.Pandoc for which Todo directives are respected. P.Pandoc parses and
-- labels these correctly, but for some reason put them with the P.Header text
-- (i.e [Inline]) instead of shoving them in the attrs.
-- FIXME: Loada fuckin' crap is what this function is. Clean it up wise one
sterileSectionHeader :: P.Block -> P.Block
sterileSectionHeader = \case
  (P.Header lvl (cid, cls, props) txt) -> P.Header lvl (cid, cls, props') txt'
    where
      mark :: ((Text, [Text]), [P.Inline]) -> P.Inline -> ((Text, [Text]), [P.Inline])
      mark ((todo, tags), is) i = case i of
        (P.Span (_, cls, props) _)
        -- FIXME Any Span whose class value is all upper is considered to be a
        -- Todo status. A task has been created to change this to be a
        -- configurable list of todo statuses. Right now we are relying on
        -- Pandoc to identify todo status and present it in a `Span` class attr
        -- as a fully upper-case value, which is a very weird thing to do
          | isJust $ find (\x -> toUpper x == x) cls ->
            let todo = fromMaybe mempty $ find (\x -> toUpper x == x) cls
             in ((todo, tags), is)
          | isJust $ find (\x -> fst x == "tag-name") props ->
            let tags' = map snd $ filter (\x -> fst x == "tag-name") props
             in ((todo, tags ++ tags'), is)
        P.Space
          | null is -> ((todo, tags), is)
        i -> ((todo, tags), is ++ [i])

      exploded :: ((Text, [Text]), [P.Inline])
      exploded = foldl' mark ((mempty, mempty), mempty) txt

      txt' = snd exploded

      props' =
        props
          ++ [("todo", fst . fst $ exploded)]
          ++ (("tag",) <$> (snd . fst) exploded)
  xs -> xs

-- | Try to parse a given string as org-mode meta value. It expects strings of
-- form "#+KEY: val", e.g obtained from RawBlock Blocks returned by readOrg of
-- Pandoc
-- FIXME: This function returns all the meta-values as Text. It is possible to
-- provide better values, e.g t/nil can be converted to True/Falsje
metaValParser :: Parsec.Parsec Text () (Map Text P.MetaValue)
metaValParser = do
  _ <- Parsec.string "#+"
  name <- Parsec.manyTill Parsec.anyChar (Parsec.try $ Parsec.char ':')
  _ <- Parsec.spaces
  val <- Parsec.many Parsec.anyChar

  return $ M.singleton (toText name) (P.MetaString $ toText val)

-- | Read the SaunfDoc from given org content. It differs from Pandoc's readOrg
-- in following ways:
--
-- * Additional meta values (#+KEY: Val) in org header are loaded in Pandoc Meta
--   instead of adding them as RawText Blocks in Pandoc body. Please note that
--   all additional meta values are read as-is as Text as of now
readSaunfDoc :: (P.PandocMonad m) => Text -> m P.Pandoc
readSaunfDoc content = do
  (P.Pandoc meta body) <- P.readOrg P.def content

  let extraMeta = mconcat $ metaEls <$> takeWhile isMetaBlock body
  let newMeta = meta <> P.Meta extraMeta
  let newBody = dropWhile isMetaBlock body

  return $ P.Pandoc newMeta newBody
  where
    isMetaBlock :: P.Block -> Bool
    isMetaBlock (P.RawBlock (P.Format "org") _) = True
    isMetaBlock _ = False

    metaEls :: P.Block -> M.Map Text P.MetaValue
    metaEls (P.RawBlock (P.Format "org") val) =
      fromRight M.empty $ Parsec.parse metaValParser (toString val) val
    metaEls _ = mempty
