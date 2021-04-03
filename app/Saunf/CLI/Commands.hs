{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Saunf.CLI.Commands where

import Relude
import System.Directory

init :: IO ()
init = do
  let confFileName = "./saunf.dhall"
  let defaultTemplate =
        "\
        \let Github = { repo : Text, user : Text, token : Text }\n\
        \\n\
        \in  { github = None Github\n\
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
