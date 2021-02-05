{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module SharedSpec where

import Control.Monad.Reader
import Saunf.Shared
import Saunf.Types
import Test.Hspec
import Text.Pandoc as P hiding (Reader)

inlines :: Block -> [Inline]
inlines = \case
  (Plain xs) -> xs
  (Para xs) -> xs
  _ -> []

spec :: Spec
spec = do
  describe "findSections" $ do
    it "returns Nothing if section with given matcher is not found" $ do
      orgFile' <- P.runIO (readOrg def "#+title: Title *from* Meta\n\nThis is the description\n\n* First Section ")
      let doc = either mempty id orgFile'
      let sections = runReader (findSections (isHeaderWithId "my-section")) (SaunfEnv doc mempty)

      sections `shouldBe` []

    it "returns all blocks b/w matched heading and next heading of same or higher level" $ do
      orgFile' <-
        P.runIO
          ( readOrg
              def
              "** First Section\n:PROPERTIES:\n\
              \:CUSTOM_ID: my-section\n\
              \:END:\n\
              \First section text\n\
              \* Second Section"
          )
      let doc = either mempty id orgFile'
      let sections = runReader (findSections (isHeaderWithId "my-section")) (SaunfEnv doc mempty)

      expectedDoc' <-
        P.runIO
          ( readOrg
              def
              "** First Section\n\
              \:PROPERTIES:\n\
              \:CUSTOM_ID: my-section\n\
              \:END:\n\
              \First section text"
          )
      let (Pandoc _ expectedSection) = either mempty id expectedDoc'

      sections `shouldBe` [Section expectedSection]

    it "returns first matched section if multiple sections with same ID are present" $ do
      orgFile' <-
        P.runIO
          ( readOrg
              def
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
          )
      let doc = either mempty id orgFile'
      let section = runReader (findSections (isHeaderWithId "my-section")) (SaunfEnv doc mempty)

      expectedDoc' <-
        P.runIO
          ( readOrg
              def
              "* Original Section\n\
              \:PROPERTIES:\n\
              \:CUSTOM_ID: my-section\n\
              \:END:\n\
              \Original section text"
          )
      let (Pandoc _ expectedSection) = either mempty id expectedDoc'

      section `shouldBe` [Section expectedSection]

    describe "sectionsWithProperties" $ do
      it "returns all the sections which has provided properties" $ do
        orgFile' <-
          P.runIO
            ( readOrg
                def
                "* Original Section\n\
                \:PROPERTIES:\n\
                \:KEY1: val1\n\
                \:END:\n\
                \* Duplicate Section\n"
            )

        let doc = either mempty id orgFile'
        let sections = runReader (sectionsWithProperties []) (SaunfEnv doc mempty)

        pending
