{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Saunf.CLI.Types where

import Relude
import Saunf.Types

newtype CLI a = CLI
  { unCLI :: ReaderT (SaunfEnv CLI) IO a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (SaunfEnv CLI)
    )

data CLIOptions = CLIOptions
  { cliCommand :: CLICommands,
    debug :: Bool
  }

data CLICommands
  = Init
  | Format
  | Readme ReadmeOptions
  | GithubIssues GithubIssuesOptions
  deriving (Show)


data ReadmeOptions
  = PushReadme -- Push changes to readme
  | PullReadme -- Pull changes back from readme
  deriving (Show)

data GithubIssuesOptions
  = PushGHIssues -- Push new issues to Github
  | PullGHIssues -- Pull new issues from Github
  deriving (Show)
