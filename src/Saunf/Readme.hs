{-# LANGUAGE OverloadedStrings #-}

module Saunf.Readme
  ( grabReadmeTitle,
    buildReadme,
    Readme (..),
  )
where

import Text.Pandoc
import Control.Applicative

-- Create a readme doc, and push it to readme.md
pushReadmeFile :: IO ()
pushReadmeFile = putStrLn "Implementation in progress"

data Readme = Readme {readmeTitle :: Maybe [Inline], readmeBlocks :: [Block]}
  deriving (Show)

buildReadme :: Pandoc -> Readme
buildReadme file@(Pandoc _ bs) = Readme (grabReadmeTitle file) bs

grabReadmeTitle :: Pandoc -> Maybe [Inline]
grabReadmeTitle (Pandoc meta bs) = metaTitle <|> firstBlockTitle
  where
    getInlines x = case x of
      MetaInlines x -> Just x
      _ -> Nothing
    isHeader Header{} = True
    isHeader _ = False
    metaTitle = getInlines =<< lookupMeta "title" meta
    firstBlockTitle = case take 1 . filter isHeader $ bs of
      [Header _ _ xs] -> Just xs
      _ -> Nothing
