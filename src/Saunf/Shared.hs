module Saunf.Shared where

import Data.Text (Text)
import Text.Pandoc
import Text.Pandoc.Shared

findSection :: Text -> [Block] -> Maybe [Block]
findSection id bs = case dropWhile isNotMatch bs of
  [] -> Nothing
  h@(Header level _ _) : xs -> Just $ h : takeWhile (\x -> (not . isHeaderBlock) x || isNotHeaderOfLevel level x) xs
  _ -> Nothing
  where
    isNotMatch x = case x of
      Header _ (id', _, _) _ -> id /= id'
      _ -> True
    isNotHeaderOfLevel lvl x = case x of
      (Header lvl' _ _) -> lvl < lvl'
      _ -> False
