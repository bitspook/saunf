{-# LANGUAGE OverloadedStrings #-}

module Saunf.Readme
  ( findTitle,
    findDescription,
    findSection,
    buildReadme,
    getReadmeTemplate,
    getConfig,
    SaunfConfig(..),
    Readme (..),
  )
where

import Data.List
import Data.Text (Text)
import Text.Pandoc

-- Create a readme doc, and push it to readme.md
pushReadmeFile :: IO ()
pushReadmeFile = putStrLn "Implementation in progress"

data Readme = Readme {readmeTitle :: Maybe [Inline], readmeBlocks :: [Block]}
  deriving (Show)

buildReadme :: Pandoc -> Readme
buildReadme file@(Pandoc _ bs) = Readme (findTitle file) bs

findTitle :: Pandoc -> Maybe [Inline]
findTitle (Pandoc meta _) = metaTitle
  where
    getInlines x = case x of
      MetaInlines x -> Just x
      _ -> Nothing
    metaTitle = getInlines =<< lookupMeta "title" meta

findDescription :: Pandoc -> Maybe [Block]
findDescription (Pandoc _ bs) = case takeWhile isNotHeader bs of
  [] -> Nothing
  xs -> Just xs
  where
    isNotHeader Header {} = False
    isNotHeader _ = True

findSection :: Text -> [Block] -> Maybe [Block]
findSection id bs = case dropWhile isNotMatch bs of
  [] -> Nothing
  h@(Header level _ _) : xs -> Just $ h : takeWhile (\x -> isNotHeader x || isNotHeaderOfLevel level x) xs
  _ -> Nothing
  where
    isNotMatch x = case x of
      Header _ (id', _, _) _ -> id /= id'
      _ -> True
    isNotHeader x = case x of
      Header {} -> False
      _ -> True
    isNotHeaderOfLevel lvl x = case x of
      (Header lvl' _ _) -> lvl /= lvl'
      _ -> False

newtype SaunfConfig
  = SaunfConfig [Block]
  deriving (Show, Eq)

getConfig :: Pandoc -> Maybe SaunfConfig
getConfig (Pandoc _ bs) = SaunfConfig <$> findSection "saunf-config" bs

getReadmeTemplate :: SaunfConfig -> Maybe Text
getReadmeTemplate (SaunfConfig bs) = case findSection "readme" bs of
  Just xs -> firstCodeBlock xs
  _ -> Nothing
  where
    isCodeBlock x = case x of
      (CodeBlock _ _) -> True
      _ -> False
    firstCodeBlock xs = case find isCodeBlock xs of
      Just (CodeBlock _ t) -> Just t
      _ -> Nothing
