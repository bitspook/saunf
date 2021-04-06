{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Saunf.CLI where

import qualified Colog as CL
  ( LogAction,
    Message,
    Msg (..),
    Severity (..),
    cfilter,
    cmap,
    logByteStringStdout,
    richMessageAction,
    showSeverity,
  )
import Control.Monad.Reader
import qualified Data.Text.IO as T
import qualified Dhall (auto, input)
import Options.Applicative
import Relude
import qualified Saunf.CLI.Commands as Commands
import Saunf.Conf
import Saunf.Types
import Saunf.CLI.Types
import Text.Pandoc as P

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

cliCommands :: Parser CLICommands
cliCommands =
  subparser $
    command "init" (info (pure Init) (progDesc "Initialize a saunf in a project with default conf"))
      <> command "readme" (info (Readme <$> readmeOptions <**> helper) (progDesc "Manage the readme file"))
      <> command "gh-issues" (info (GithubIssues <$> githubIssuesOptions <**> helper) (progDesc "Manage Github issues"))

cliOptions :: Parser CLIOptions
cliOptions =
  CLIOptions
    <$> cliCommands
    <*> switch (long "debug" <> help "Show very verbose logs for debugging")

cliParser :: ParserInfo CLIOptions
cliParser =
  info
    (cliOptions <**> helper)
    (fullDesc <> header "Tasty project management" <> progDesc "Manage software projects with plain-text")

getLogger :: (MonadIO m) => LogVerbosity -> CL.LogAction m CL.Message
getLogger v = case v of
  Debug -> CL.richMessageAction
  _ -> soberMessageAction
  where
    filterByVerbosity = case v of
      Error -> CL.cfilter (\(CL.Msg sev _ _) -> sev >= CL.Error)
      Warning -> CL.cfilter (\(CL.Msg sev _ _) -> sev >= CL.Warning)
      Info -> CL.cfilter (\(CL.Msg sev _ _) -> sev >= CL.Info)
      Debug -> CL.cfilter (\(CL.Msg sev _ _) -> sev >= CL.Debug)
    soberFmtMessage CL.Msg {..} = CL.showSeverity msgSeverity <> msgText
    soberMessageAction =
      filterByVerbosity $
        CL.cmap (encodeUtf8 . soberFmtMessage) CL.logByteStringStdout

buildSaunfEnv :: Bool -> IO (SaunfEnv CLI)
buildSaunfEnv isDebugEnabled = do
  conf <- Dhall.input Dhall.auto "./saunf.dhall"
  pmpText <- T.readFile $ saunfDocPath conf
  saunfDoc <- P.handleError =<< P.runIO (readOrg def pmpText)
  let logVerbosity = if isDebugEnabled then Debug else verbosity conf

  return $ SaunfEnv saunfDoc conf (getLogger logVerbosity)

run :: IO ()
run = do
  (CLIOptions cmds isDebugEnabled) <- execParser cliParser
  case cmds of
    Init -> Commands.init
    _ -> do
      env <- buildSaunfEnv isDebugEnabled

      case cmds of
        Readme PushReadme -> P.runIOorExplode $ runReaderT (unCLI Commands.pushReadmeFile) env
        GithubIssues PushGHIssues -> P.runIOorExplode $ runReaderT (unCLI Commands.pushGithubIssues) env
        _ -> putStrLn "Not implemented yet 😢"
