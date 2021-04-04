{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Saunf.CLI where

import Control.Monad.Reader
import qualified Data.Text.IO as T
import qualified Dhall (auto, input)
import qualified Saunf.CLI.Commands as Commands
import Saunf.Types
import Saunf.Conf
import Options.Applicative
import Text.Pandoc as P
import Colog (richMessageAction)

data CLIOptions
  = Init
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

readmeOptions :: Parser ReadmeOptions
readmeOptions =
  subparser
    ( command "push" (info (pure PushReadme) (progDesc "Push changes to readme.md"))
        <> command "pull" (info (pure PullReadme) (progDesc "Pull changes made to readme.md back"))
    )

githubIssuesOptions :: Parser GithubIssuesOptions
githubIssuesOptions =
  subparser
    ( command "push" (info (pure PushGHIssues) (progDesc "Push new issues to Github issues"))
        <> command "pull" (info (pure PullGHIssues) (progDesc "Pull new Github issues"))
    )

cliOptions :: Parser CLIOptions
cliOptions =
  subparser $
    command "init" (info (pure Init) (progDesc "Initialize a saunf in a project with default conf"))
      <> command "readme" (info (Readme <$> readmeOptions <**> helper) (progDesc "Manage the readme file"))
      <> command "gh-issues" (info (GithubIssues <$> githubIssuesOptions <**> helper) (progDesc "Manage Github issues"))

cliParser :: ParserInfo CLIOptions
cliParser = info (cliOptions <**> helper) (fullDesc <> header "Tasty project management" <> progDesc "Manage software projects with plain-text")

buildSaunfEnv :: IO (SaunfEnv CLI)
buildSaunfEnv = do
  conf <- Dhall.input Dhall.auto "./saunf.dhall"
  pmpText <- T.readFile $ saunfDocPath conf
  saunfDoc <- P.handleError =<< P.runIO (readOrg def pmpText)

  return $ SaunfEnv saunfDoc conf richMessageAction

run :: IO ()
run = do
  val <- execParser cliParser
  case val of
    Init -> Commands.init
    _ -> do
      env <- buildSaunfEnv

      case val of
        Readme PushReadme -> P.runIOorExplode $ runReaderT (unCLI Commands.pushReadmeFile) env
        -- GithubIssues PushGHIssues -> runReaderT (unCLI $ SaunfIssues.push) env
        _ -> putStrLn "Not implemented yet 😢"
