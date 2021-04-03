{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Saunf.CLI.Commands where

import Relude
import System.Directory
import Saunf.Types
import Saunf.Conf
import qualified Saunf.Readme as Saunf
import Text.Pandoc as P
import qualified Data.Text.IO as T

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
  if hasExistingSaunfConf
    then
      die
        "Found existing saunf conf (saunf.dhall). \n\
        \Please delete it if you want to overwrite the saunf conf"
    else return ()
  writeFileText confFileName defaultTemplate

-- Create a readme doc, and push it to readme.md
pushReadmeFile ::
  ( MonadIO m,
    MonadReader e m,
    HasSaunfDoc e,
    HasSaunfConf e
  ) =>
  m ()
pushReadmeFile = do
  dest <- asks $ readmePath . getSaunfConf
  env <- ask

  readme' <- liftIO $ P.runIO $ runReaderT Saunf.readme env
  readme <- liftIO $ P.handleError readme'

  liftIO $ T.writeFile dest readme
