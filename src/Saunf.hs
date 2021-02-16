{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Saunf
  ( pushReadmeFile,
    getConfig,
  )
where

import Control.Monad.Reader
import Saunf.Readme (getReadmeTemplate, pushReadmeFile)
import Saunf.Shared
import Saunf.Types

getConfig :: Reader SaunfEnv (Either SaunfConfError SaunfConf)
getConfig = do
  sections <- filterSections (isHeaderWithId "saunf-conf")
  return $ case sections of
    [x] -> Right $ SaunfConf (getReadmeTemplate x)
    [] -> Left ConfNotFound
    _ -> Left ConflictingConf
