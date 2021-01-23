{-# LANGUAGE OverloadedStrings #-}

module Saunf where

import Control.Applicative
import qualified Data.Map as Map
import Data.Org
import Data.Text (Text)
import Data.List.NonEmpty (intersperse)

-- Create a readme doc, and push it to readme.md
pushReadmeFile :: IO ()
pushReadmeFile = putStrLn "Implementation in progress"

data Readme = Readme { readmeTitle :: Maybe Text, readmeDoc :: OrgDoc }
            deriving (Show)

buildReadme :: OrgFile -> Readme
buildReadme orgFile = Readme { readmeTitle = grabReadmeTitle orgFile, readmeDoc = emptyDoc }

grabReadmeTitle :: OrgFile -> Maybe Text
grabReadmeTitle (OrgFile meta (OrgDoc _ scs)) = metaTitle <|> firstSectionTitle
  where
    metaTitle = Map.lookup "title" meta
    firstSectionTitle = case scs of
      [] -> Nothing
      _ -> Just $ foldr (<>) "" (intersperse " " (prettyWords <$> (sectionHeading . head $ scs)))
