{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module SharedSpec where

import Control.Monad.Reader
import Saunf.Shared
import Saunf.Types
import Shared
import Test.Hspec
import Text.Pandoc as P hiding (Reader)

spec :: Spec
spec = do
  describe "filterSections" $ do
    it "returns Nothing if section with given matcher is not found" $ do
      orgFile' <- P.runIO (readOrg def "#+title: Title *from* Meta\n\nThis is the description\n\n* First Section ")
      doc <- P.handleError orgFile'
      let sections = runReader (filterSections (isHeaderWithId "my-section")) (SaunfEnv doc mempty)

      sections `shouldBe` []

    it "returns all blocks b/w matched heading and next heading of same or higher level" $ do
      doc <-
        readOrg'
          "** First Section\n:PROPERTIES:\n\
          \:CUSTOM_ID: my-section\n\
          \:END:\n\
          \First section text\n\
          \* Second Section"
      let sections = runReader (filterSections (isHeaderWithId "my-section")) (SaunfEnv doc mempty)

      (Pandoc _ expectedSection) <-
        readOrg'
          "** First Section\n\
          \:PROPERTIES:\n\
          \:CUSTOM_ID: my-section\n\
          \:END:\n\
          \First section text"

      sections `shouldBe` [Section expectedSection]

    it "returns all matched sections" $ do
      doc <-
        readOrg'
          "#+title: Title *from* Meta\n\n\
          \This is the description\n\n\
          \* Original Section\n\
          \:PROPERTIES:\n\
          \:CUSTOM_ID: my-section\n\
          \:END:\n\
          \Original section text\n\
          \* Duplicate Section\n\
          \:PROPERTIES:\n\
          \:CUSTOM_ID: my-section\n\
          \:END:\n"
      let section = runReader (filterSections (isHeaderWithId "my-section")) (SaunfEnv doc mempty)

      (Pandoc _ expectedSection1) <-
        P.handleError
          =<< P.runIO
            ( readOrg
                def
                "* Original Section\n\
                \:PROPERTIES:\n\
                \:CUSTOM_ID: my-section\n\
                \:END:\n\
                \Original section text\n"
            )
      (Pandoc _ expectedSection2) <-
        P.handleError
          =<< P.runIO
            ( readOrg
                def
                "* Duplicate Section\n\
                \:PROPERTIES:\n\
                \:CUSTOM_ID: my-section\n\
                \:END:"
            )

      section `shouldBe` [Section expectedSection1, Section expectedSection2]

    describe "sectionsWithProperties" $ do
      it "returns all the sections which has provided properties" $ do
        doc <-
          readOrg'
            "* Section 1\n\
            \:PROPERTIES:\n\
            \:KEY1: val1\n\
            \:END:\n\
            \* Section 1+2\n\
            \:PROPERTIES:\n\
            \:KEY1: val1\n\
            \:KEY2: val2\n\
            \:END:\n\
            \* Section 2\n\
            \:PROPERTIES:\n\
            \:KEY2: val2\n\
            \:END:\n\
            \* Additional Section\n"

        let sections1 = runReader (sectionsWithProperties [("key1", "val1")]) (SaunfEnv doc mempty)
        let sections1n2 = runReader (sectionsWithProperties [("key1", "val1"), ("key2", "val2")]) (SaunfEnv doc mempty)
        let sections2 = runReader (sectionsWithProperties [("key2", "val2")]) (SaunfEnv doc mempty)
        let nonSections = runReader (sectionsWithProperties [("key3", "val3")]) (SaunfEnv doc mempty)

        nonSections `shouldBe` []
        sections1 `shouldBe` [Section [Header 1 ("", [], [("key1", "val1")]) [Str "Section", Space, Str "1"]], Section [Header 1 ("", [], [("key1", "val1"), ("key2", "val2")]) [Str "Section", Space, Str "1+2"]]]
        sections1n2 `shouldBe` [Section [Header 1 ("", [], [("key1", "val1"), ("key2", "val2")]) [Str "Section", Space, Str "1+2"]]]
        sections2 `shouldBe` [Section [Header 1 ("", [], [("key1", "val1"), ("key2", "val2")]) [Str "Section", Space, Str "1+2"]], Section [Header 1 ("", [], [("key2", "val2")]) [Str "Section", Space, Str "2"]]]
