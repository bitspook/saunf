{-# LANGUAGE NoImplicitPrelude #-}

module Saunf.Error where

import Relude
import qualified GitHub.Data.Definitions as GH

data SaunfError
  = GithubError GH.Error
  | SaunfConfError Text
  | ReadmeError Text
  | UnknownSaunfError Text
  deriving (Show)

instance Exception SaunfError
