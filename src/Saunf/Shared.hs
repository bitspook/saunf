module Saunf.Shared where

import Control.Monad.Reader
import Data.List (intersect)
import Data.Text (Text)
import Saunf.Types
import Text.Pandoc hiding (Reader)
import Text.Pandoc.Shared

isHeaderWithId :: Text -> Block -> Bool
isHeaderWithId id b = case b of
  (Header _ (id', _, _) _) -> id' == id
  _ -> False

filterSections :: (Block -> Bool) -> Reader SaunfEnv [Section]
filterSections headerMatcher = do
  Pandoc _ bs <- asks saunfDoc
  return $ case dropWhile isNotMatch bs of
    h@(Header level _ _) : xs ->
      Section (h : takeWhile (sectionContent level) xs) :
      runReader
        (filterSections headerMatcher)
        (SaunfEnv (Pandoc mempty (dropWhile (sectionContent level) xs)) mempty)
    _ -> []
  where
    isNotMatch x = case x of
      Header {} -> not . headerMatcher $ x
      _ -> True
    isNotHeaderOfLevel lvl x = case x of
      (Header lvl' _ _) -> lvl < lvl'
      _ -> False
    sectionContent level x = (not . isHeaderBlock) x || isNotHeaderOfLevel level x

sectionsWithProperties :: [(Text, Text)] -> Reader SaunfEnv [Section]
sectionsWithProperties [] = return []
sectionsWithProperties props = filterSections filterByProps
  where
    filterByProps h = case h of
      (Header _ (_, _, props') _) -> props `intersect` props' == props
      _ -> False
