{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SharedSpec where

import Data.Org (orgDoc)
import Relude
import Saunf.Shared
import Saunf.Types
import Test.Hspec
import TestAssist

spec :: Spec
spec = do
  describe "filterSections" $ do
    it "returns Nothing if section with given matcher is not found" $ do
      doc <-
        readSaunfDoc' $
          unlines
            [ "#+title: Title *from* Meta",
              "This is the description",
              "",
              "* First Section"
            ]
      let sections = filterSections (hasCId "my-section") (orgDoc doc)

      sections `shouldBe` []

    it "returns all matched sections" $ do
      doc <-
        readSaunfDoc' $
          unlines
            [ "#+title: Title *from* Meta",
              "This is the description",
              "",
              "* Original Section",
              ":PROPERTIES:",
              ":CUSTOM_ID: my-section",
              ":END:",
              "Original section text",
              "",
              "* Duplicate Section",
              ":PROPERTIES:",
              ":CUSTOM_ID: my-section",
              ":END:\n"
            ]
      let sections = filterSections (hasCId "my-section") (orgDoc doc)

      length sections `shouldBe` 2
