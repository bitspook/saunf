module Saunf.Shared where

import Control.Monad.Reader
import Data.Text (Text)
import Saunf.Types
import Text.Pandoc hiding (Reader)
import Text.Pandoc.Shared

isHeaderWithId :: Text -> Block -> Bool
isHeaderWithId id b = case b of
  (Header _ (id', _, _) _) -> id' == id
  _ -> False

findSections :: (Block -> Bool) -> Reader SaunfEnv [Section]
findSections headerMatcher = do
  Pandoc _ bs <- asks saunfDoc
  return $ case dropWhile isNotMatch bs of
    h@(Header level _ _) : xs -> [Section (h : takeWhile (\x -> (not . isHeaderBlock) x || isNotHeaderOfLevel level x) xs)]
    _ -> []
  where
    isNotMatch x = case x of
      Header{} -> not . headerMatcher $ x
      _ -> True
    isNotHeaderOfLevel lvl x = case x of
      (Header lvl' _ _) -> lvl < lvl'
      _ -> False

sectionsWithProperties :: [(Text, Text)] -> Reader SaunfEnv [Section]
sectionsWithProperties props = do
  Pandoc _ bs <- asks saunfDoc
  return []
