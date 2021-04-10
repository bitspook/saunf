{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Saunf.Shared where

import Data.List (intersect)
import Data.Text (toUpper)
import Relude
import Saunf.Types
import qualified Text.Pandoc as P hiding (Reader)
import qualified Text.Pandoc.Shared as P

isHeaderWithId :: Text -> P.Block -> Bool
isHeaderWithId cid b = case b of
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
