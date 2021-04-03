{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Saunf.Shared where

import Data.List (intersect)
import Relude
import Saunf.Types
import Text.Pandoc hiding (Reader)
import Text.Pandoc.Shared

isHeaderWithId :: Text -> Block -> Bool
isHeaderWithId id b = case b of
  (Header _ (id', _, _) _) -> id' == id
  _ -> False

-- TODO: Write tests for me please
-- TODO: Take "category" as an argument perhaps, make this hasProperty instead.
-- There is a good chance that such a function already exists in Pandoc
hasCategory :: Text -> Block -> Bool
hasCategory cat block = case block of
  (Header _ (_, _, props) _) -> any (\p -> fst p == "category" && snd p == cat) props
  _ -> True

-- TODO: Write tests for me please
isHeaderWithLevel :: Int -> Block -> Bool
isHeaderWithLevel lvl block = case block of
  (Header lvl' _ _) -> lvl == lvl'
  _ -> False

filterSections :: (MonadReader e m, HasSaunfDoc e) => (Block -> Bool) -> m [Section]
filterSections headerMatcher = do
  Pandoc _ bs <- asks getSaunfDoc
  case dropWhile isNotMatch bs of
    h@(Header level _ _) : xs -> do
      let content = takeWhile (isSectionContent level) xs
      let leftover = Pandoc mempty $ dropWhile (isSectionContent level) xs
      remainingContent <- local (setSaunfDoc leftover) $ filterSections headerMatcher
      return $ Section h content : remainingContent
    _ -> return []
  where
    isNotMatch x = case x of
      Header {} -> not . headerMatcher $ x
      _ -> True
    isNotHeaderOfLevel lvl x = case x of
      (Header lvl' _ _) -> lvl < lvl'
      _ -> False
    isSectionContent level x = (not . isHeaderBlock) x || isNotHeaderOfLevel level x

sectionsWithProperties :: (MonadReader e m, HasSaunfDoc e) => [(Text, Text)] -> m [Section]
sectionsWithProperties [] = return []
sectionsWithProperties props = filterSections filterByProps
  where
    filterByProps h = case h of
      (Header _ (_, _, props') _) -> props `intersect` props' == props
      _ -> False

-- | Adjust the level of a section to given level. This will make the first
-- Header in section of the given level, and every sub-Header's level
-- will be shifted accordingly
setSectionHeaderLevel :: Int -> Section -> Section
setSectionHeaderLevel n (Section h xs) = case h of
  (Header sectionLvl attr inner) -> Section (Header n attr inner) (shiftHeader <$> xs)
    where
      delta = n - sectionLvl
      shiftHeader b = case b of
        Header lvl a i -> Header (lvl + delta) a i
        _ -> b
  _ -> Section h xs

-- Nasty little helper to quickly render shit to markdown
writeMd ::
  ( PandocMonad m,
    MonadReader e m,
    HasSaunfDoc e,
    MonadIO m
  ) =>
  [Block] ->
  m Text
writeMd bollocks = do
  (Pandoc meta _) <- asks getSaunfDoc
  liftIO $ handleError =<< runIO (writeMarkdown def $ Pandoc meta bollocks)
