{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.Reader
import qualified Data.Text.IO as T
import qualified Dhall (auto, input)
import Options.Applicative
import Saunf.Readme
import Saunf.Conf
import Saunf.Types
import Text.Pandoc as P
import qualified Saunf.Issue as SaunfIssues

data CliOptions
  = Readme ReadmeOptions
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

cliOptions :: Parser CliOptions
cliOptions =
  subparser $
    command "readme" (info (Readme <$> readmeOptions <**> helper) (progDesc "Manage the readme file"))
      <> command "gh-issues" (info (GithubIssues <$> githubIssuesOptions <**> helper) (progDesc "Manage Github issues"))

cliParser :: ParserInfo CliOptions
cliParser = info (cliOptions <**> helper) (fullDesc <> header "Tasty project management" <> progDesc "Manage software projects with plain-text")

main :: IO ()
main = do
  let readmeDest = "./readme.md"
  conf :: SaunfConf <- Dhall.input Dhall.auto "./saunf.dhall"
  pmpText <- T.readFile "./saunf/saunf.org"
  saunfDoc <- P.handleError =<< P.runIO (readOrg def pmpText)
  val <- execParser cliParser

  let env = SaunfEnv saunfDoc conf
  case val of
    Readme PushReadme -> runReaderT (pushReadmeFile readmeDest) env
    GithubIssues PushGHIssues -> runReaderT SaunfIssues.push env
    _ -> putStrLn "Not implemented yet :-("
