{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Saunf.Conf where

import GitHub.Data
import Data.Text (Text)
import Dhall (FromDhall, Generic)

data GithubConf = GithubConf
  { user :: Name Owner,
    repo :: Name Repo,
    token :: Text
  }
  deriving (Generic, Show)

instance FromDhall (Name Owner)
instance FromDhall (Name Repo)
instance FromDhall GithubConf

data SaunfConf = SaunfConf
  { readmeTemplate :: Maybe Text,
    github :: Maybe GithubConf
  }
  deriving (Show, Generic)

instance FromDhall SaunfConf

emptyConf :: SaunfConf
emptyConf = SaunfConf Nothing Nothing
