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
import Text.Pandoc hiding (Reader)

getConfig :: Pandoc -> Maybe SaunfConf
getConfig doc =
  SaunfConf <$> case runReader (findSections (isHeaderWithId "saunf-conf")) (SaunfEnv doc mempty) of
    (Section x:_) -> Just x
    _ -> Nothing
