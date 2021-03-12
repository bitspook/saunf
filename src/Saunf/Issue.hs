{-# LANGUAGE NoImplicitPrelude #-}

module Saunf.Issue where

import Control.Monad.Reader
import Relude
import Saunf.Types
import Saunf.Shared (filterSections)

hasCategory cat block | trace ("HAS CATEGORY: " ++ show block) False = undefined
hasCategory cat block = True

issues :: Reader SaunfEnv [Issue]
issues = do
  sections <- filterSections (hasCategory "issue")
  return $ Issue <$> sections
