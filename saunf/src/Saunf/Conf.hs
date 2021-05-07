{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Saunf.Conf where

import GitHub.Data
import Dhall (FromDhall)
import Relude

data GithubConf = GithubConf
  { user :: Name Owner,
    repo :: Name Repo,
    token :: Text
  }
  deriving (Generic, Show)

data LogVerbosity = Error | Warning | Info | Debug deriving (Show, Generic)

instance FromDhall (Name Owner)
instance FromDhall (Name Repo)
instance FromDhall GithubConf
instance FromDhall LogVerbosity

-- | Saunf configuration
data SaunfConf = SaunfConf
  { readmePath :: FilePath,
    saunfDocPath :: FilePath,
    readmeTemplate :: Maybe Text,
    github :: Maybe GithubConf,
    verbosity :: LogVerbosity
  }
  deriving (Show, Generic)

instance FromDhall SaunfConf

emptyConf :: SaunfConf
emptyConf = SaunfConf "" "" Nothing Nothing Info
