{-# LANGUAGE OverloadedStrings #-}

module Saunf
  ( pushReadmeFile,
    getConfig,
  )
where

import Control.Monad.Reader
import Saunf.Readme (pushReadmeFile)
import Saunf.Shared
import Saunf.Types

getConfig :: Reader SaunfEnv (Maybe SaunfConf)
getConfig = do
  sections <- findSections (isHeaderWithId "saunf-conf")
  return $
    SaunfConf <$> case sections of
      (Section x : _) -> Just x
      _ -> Nothing
