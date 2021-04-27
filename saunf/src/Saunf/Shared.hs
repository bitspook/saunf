{-# LANGUAGE OverloadedStrings #-}

module Saunf.Shared where

import qualified Data.Map as M
import Data.Org (OrgDoc (..), OrgFile, Section (..), org)
import Relude

readSaunfDoc :: Text -> Maybe OrgFile
readSaunfDoc = org

-- | Filter sections of given $OrgDoc$. Perform the given function on sections
-- of all depths in the OrgDoc
filterSections :: (Section -> Bool) -> OrgDoc -> [Section]
filterSections f (OrgDoc _ sections) = filter f sections ++ children
  where
    children = concat $ filterSections f . sectionDoc <$> sections

-- | Checks if a section has a category
hasCategory :: Text -> Section -> Bool
hasCategory cat Section {sectionProps = props} =
  isJust $ (==) cat <$> M.lookup "Category" props
