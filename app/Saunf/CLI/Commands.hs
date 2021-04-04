{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Saunf.CLI.Commands where

import Colog
  ( Message,
    WithLog,
    log,
    pattern D,
    pattern I,
  )
import qualified Data.Text.IO as T
import Relude
import Saunf.Conf
import qualified Saunf.Readme as Saunf
import Saunf.Types
import System.Directory
import Text.Pandoc as P

init :: IO ()
init = do
  let confFileName = "./saunf.dhall"
  let defaultTemplate =
        "\
        \let Github = { repo : Text, user : Text, token : Text }\n\
        \\n\
        \in  { readmePath = \"./readme.md\"\n\
        \    , saunfDocPath = \"./saunf/saunf.org\"\n\
        \    , github = None Github\n\
        \    , readmeTemplate = Some\n\
        \        ''\n\
        \        # $title$\n\
        \        $description$\n\
        \        ''\n\
        \    }"

  hasExistingSaunfConf <- doesFileExist confFileName
  when
    hasExistingSaunfConf
    $ die
      "Found existing saunf conf (saunf.dhall). \n\
      \Please delete it if you want to overwrite the saunf conf"
  writeFileText confFileName defaultTemplate

-- Create a readme doc, and push it to readme.md
pushReadmeFile ::
  ( WithLog e Message m,
    HasSaunfDoc e,
    HasSaunfConf e,
    PandocMonad m,
    MonadIO m
  ) =>
  m ()
pushReadmeFile = do
  dest <- asks $ readmePath . getSaunfConf

  readme <- Saunf.readme

  log D "Writing readme file"
  liftIO $ T.writeFile dest readme
  log I $ "readme written successfully to: " <> toText dest
