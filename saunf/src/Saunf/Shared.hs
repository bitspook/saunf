{-# LANGUAGE OverloadedStrings #-}

module Saunf.Shared where

import qualified Data.Map as M
import Data.Org (Block (..), OrgDoc (..), OrgFile (..), Section (..), org)
import Relude
import Control.Monad.Catch
import Saunf.Error (SaunfError)

readSaunfDoc :: Text -> Maybe OrgFile
readSaunfDoc = org

-- | Filter sections of given $OrgDoc$. Perform the given function on sections
-- of all depths in the OrgDoc
filterSections :: (Section -> Bool) -> OrgDoc -> [Section]
filterSections f (OrgDoc _ sections) = filter f sections ++ children
  where
    children = concat $ filterSections f . sectionDoc <$> sections

findSection :: OrgFile -> Text -> Maybe Section
findSection OrgFile {orgDoc = doc} sid = listToMaybe $ filterSections (hasCId sid) doc

-- | Check if given section has a property with given value
hasProperty :: Text -> Text -> Section -> Bool
hasProperty prop val Section {sectionProps = props} =
  isJust $ (==) val <$> M.lookup prop props

-- | Checks if a section has a category
hasCategory :: Text -> Section -> Bool
hasCategory = hasProperty "CATEGORY"

-- | Checks if section has given CUSTOM_ID
hasCId :: Text -> Section -> Bool
hasCId = hasProperty "CUSTOM_ID"

-- | Perform given transformations on every section and block of the OrgFile
walk :: (Section -> Section) -> (Block -> Block) -> OrgFile -> OrgFile
walk f b (OrgFile meta root) = OrgFile meta (processDoc root)
  where
    processSection sec@Section {sectionDoc = doc} = sec {sectionDoc = processDoc doc}
    processDoc (OrgDoc blocks sections) = OrgDoc (b <$> blocks) (f . processSection <$> sections)

-- | Transform every section of the OrgFile
walkSections :: (Section -> Section) -> OrgFile -> OrgFile
walkSections f = walk f id

-- | Transform every block of the OrgFile
walkBlocks :: (Block -> Block) -> OrgFile -> OrgFile
walkBlocks = walk id

-- | Short-circuit helper to avoid nested error-handling
orDie :: (MonadThrow m) => Maybe a -> SaunfError -> m a
Just a  `orDie` _      = return a
Nothing `orDie` e = throwM e
