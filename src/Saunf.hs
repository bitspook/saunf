{-# LANGUAGE OverloadedStrings #-}

module Saunf
  ( pushReadmeFile,
    getConfig,
  )
where

import Saunf.Config
import Saunf.Readme (pushReadmeFile)
import Saunf.Shared
import Text.Pandoc

getConfig :: Pandoc -> Maybe SaunfConfig
getConfig (Pandoc _ bs) = SaunfConfig <$> findSection "saunf-config" bs
