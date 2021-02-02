{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module SharedSpec where

import Saunf.Shared
import Test.Hspec
import Text.Pandoc as P

inlines :: Block -> [Inline]
inlines = \case
  (Plain xs) -> xs
  (Para xs) -> xs
  _ -> []

spec :: Spec
spec = do
  describe "findSection" $ do
    it "returns Nothing if section with given id is not found" $ do
      orgFile' <- P.runIO (readOrg def "#+title: Title *from* Meta\n\nThis is the description\n\n* First Section ")
      let (Pandoc _ orgFile) = either mempty id orgFile'
      let section = findSection "my-section" orgFile

      section `shouldBe` Nothing

    it "returns all blocks b/w matched heading and next heading of same or higher level" $ do
      orgFile' <-
        P.runIO
          ( readOrg
              def
              "#+title: Title *from* Meta\n\n\
              \This is the description\n\n\
              \** First Section\n:PROPERTIES:\n\
              \:CUSTOM_ID: my-section\n\
              \:END:\n\
              \First section text\n\
              \* Second Section"
          )
      let (Pandoc _ orgFile) = either mempty id orgFile'
      let section = findSection "my-section" orgFile

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

      section `shouldBe` Just expectedSection

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
      let (Pandoc _ orgFile) = either mempty id orgFile'
      let section = findSection "my-section" orgFile

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

      section `shouldBe` Just expectedSection
