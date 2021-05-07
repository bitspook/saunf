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
-- import Control.Monad.Reader
import qualified Dhall (auto, input)
import Options.Applicative
import Relude
import qualified Saunf.CLI.Commands as Commands
import Saunf.CLI.Types
import Saunf.Conf
import Saunf.Types
import Saunf.Shared (readSaunfDoc, orDie)
import Control.Monad.Catch (MonadThrow)

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
      <> command "fmt" (info (pure Format) (progDesc "Format the saunf doc in optimal formatting for minimal diffs when saunf need to update saunf-doc"))
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

buildSaunfEnv :: (MonadThrow m, MonadIO m) => Bool -> m (SaunfEnv CLI)
buildSaunfEnv isDebugEnabled = do
  conf <- liftIO $ Dhall.input Dhall.auto "./saunf.dhall"
  pmpText <- liftIO $ readFileText $ saunfDocPath conf
  saunfDoc <- liftIO $ readSaunfDoc pmpText `orDie` UnknownSaunfError "Failed to read saunf doc"

  let logVerbosity = if isDebugEnabled then Debug else verbosity conf

  return $ SaunfEnv saunfDoc conf (getLogger logVerbosity)

run :: (MonadThrow m, MonadIO m) => m ()
run = do
  (CLIOptions cmd isDebugEnabled) <- liftIO $ execParser cliParser
  case cmd of
    Init -> liftIO Commands.init
    _ -> do
      env <- liftIO $ buildSaunfEnv isDebugEnabled

      let cmd' = case cmd of
            Format -> Commands.format
              -- Readme PushReadme -> Commands.pushReadmeFile
              -- GithubIssues PushGHIssues -> Commands.pushGithubIssues
            _ -> liftIO $ putStrLn "Not implemented yet 😢"

      liftIO $ runReaderT (unCLI cmd') env
