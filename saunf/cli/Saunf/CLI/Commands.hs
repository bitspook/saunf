{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Saunf.CLI.Commands where

import Colog
  ( Message,
    WithLog,
    -- log,
    -- pattern D,
    -- pattern E,
    -- pattern I,
  )
-- import qualified GitHub.Data.Issues as GH
import Relude
import Saunf.Conf
-- import Saunf.Issue
-- import qualified Saunf.Readme as Saunf
import Saunf.Types
import System.Directory
import Data.Org (prettyOrgFile)

init :: IO ()
init = do
  let confFileName = "./saunf.dhall"
  let defaultTemplate =
        unlines
          [ "let VerbosityLevel = < Error | Warning | Info | Debug >",
            "let Github = { repo : Text, user : Text, token : Text }",
            "let readmePath = \"./readme.md\"",
            "let saunfDocPath = \"./saunf.org\"",
            "let verbosity = VerbosityLevel.Debug",
            "",
            "in  { readmePath",
            "    , saunfDocPath",
            "    , github = None Github",
            "    , readmeTemplate = Some",
            "        ''",
            "        # $title$",
            "        $description$",
            "        ''",
            "    , verbosity",
            "    }"
          ]

  hasExistingSaunfConf <- doesFileExist confFileName
  when
    hasExistingSaunfConf
    $ die
      "Found existing saunf conf (saunf.dhall). \n\
      \Please delete it if you want to overwrite the saunf conf"
  writeFileText confFileName defaultTemplate

-- Create a readme doc, and push it to readme.md
-- pushReadmeFile ::
--   ( WithLog e Message m,
--     HasSaunfDoc e,
--     HasSaunfConf e,
--     MonadIO m
--   ) =>
--   m ()
-- pushReadmeFile = do
--   dest <- asks $ readmePath . getSaunfConf

--   readme <- Saunf.readme

--   log D "Writing readme file"
--   liftIO $ writeFile dest readme
--   log I $ "readme written successfully to: " <> toText dest

-- pushGithubIssues ::
--   ( WithLog e Message m,
--     HasSaunfDoc e,
--     HasSaunfConf e,
--     MonadIO m
--   ) =>
--   m ()
-- pushGithubIssues = do
--   allIssues <- issues
--   log D $ "Found " <> show (length allIssues) <> " total issues"

--   let newIssues = filter (isNothing . issueId) allIssues
--   log I $ "Found " <> show (length newIssues) <> " new issues"

--   mapM_ createGhIssue' newIssues
--   where
--     createGhIssue' i = do
--       response <- createGithubIssue i
--       case response of
--         Left err -> case err of
--           GithubError e -> log E $ "[Github error] " <> show e
--           _ -> log E $ show err
--         Right issue -> log I $ "Successfully created Github issue: " <> show (GH.issueUrl issue)

format ::
  ( WithLog e Message m,
    HasSaunfDoc e,
    HasSaunfConf e,
    MonadIO m
  ) =>
  m ()
format = do
  dest <- asks $ saunfDocPath . getSaunfConf
  doc <- asks getSaunfDoc

  let formattedDoc = prettyOrgFile doc

  liftIO $ writeFile dest (toString formattedDoc)

  return ()
