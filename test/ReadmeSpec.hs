{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ReadmeSpec where

import Saunf.Readme
import Test.Hspec
import Text.Pandoc as P
import Text.Pandoc.Walk

inlines :: Block -> [Inline]
inlines = \case
  (Plain xs) -> xs
  (Para xs) -> xs
  _ -> []

spec :: Spec
spec = do
  describe "findTitle" $ do
    it "returns Nothing if title is not present in metadata" $ do
      orgFile' <- P.runIO (readOrg def "\n\n* Yo yo yo\n")
      let orgFile = either mempty id orgFile'
      let title = findTitle orgFile

      title `shouldBe` Nothing

    it "returns title from metadata if present" $ do
      orgFile' <- P.runIO (readOrg def "#+title: Title *from* Meta\n")
      let orgFile = either mempty id orgFile'
      let title = findTitle orgFile

      expectedDoc <- P.runIO (readOrg def "Title *from* Meta")
      let expectedTitle = query inlines expectedDoc

      title `shouldBe` Just expectedTitle

  describe "findDescription" $ do
    it "returns Nothing if there is no text preceding first section" $ do
      orgFile' <- P.runIO (readOrg def "#+title: Title *from* Meta\n\n* First section heading")
      let orgFile = either mempty id orgFile'
      let description = findDescription orgFile

      description `shouldBe` Nothing

    it "returns all text till first section" $ do
      orgFile' <- P.runIO (readOrg def "#+title: Title *from* Meta\n\nThis is the description\n\n* First Section ")
      let orgFile = either mempty id orgFile'
      let description = findDescription orgFile

      expectedDoc' <- P.runIO (readOrg def "This is the description")
      let (Pandoc _ expectedDescription) = either mempty id expectedDoc'

      description `shouldBe` Just expectedDescription

  describe "findSection" $ do
    it "returns Nothing if section with given id is not found" $ do
      orgFile' <- P.runIO (readOrg def "#+title: Title *from* Meta\n\nThis is the description\n\n* First Section ")
      let (Pandoc _ orgFile) = either mempty id orgFile'
      let section = findSection "my-section" orgFile

      section `shouldBe` Nothing

    it "returns all blocks b/w matched heading and next heading of same level" $ do
      orgFile' <-
        P.runIO
          ( readOrg
              def
              "#+title: Title *from* Meta\n\n\
              \This is the description\n\n\
              \* First Section\n:PROPERTIES:\n\
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
              "* First Section\n\
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

  describe "getConfig" $ do
    it "returns Nothing if a section with CUSTOM_ID='saunf-config' is not present" $ do
      orgFile' <-
        P.runIO
          ( readOrg
              def
              "#+title: Title *from* Meta\n\n\
              \This is the description\n\n\
              \:END:\n"
          )
      let orgFile = either mempty id orgFile'
      let template = getConfig orgFile

      template `shouldBe` Nothing

    it "returns SaunfConfig if a section with CUSTOM_ID='saunf-config' is present" $ do
      orgFile' <-
        P.runIO
          ( readOrg
              def
              "#+title: Title *from* Meta\n\n\
              \This is the description\n\n\
              \* Any random text\n\
              \:PROPERTIES:\n\
              \:CUSTOM_ID: saunf-config\n\
              \:END:\n\
              \** Subsection"
          )
      let orgFile = either mempty id orgFile'
      let config = getConfig orgFile

      expectedDoc' <-
        P.runIO
          ( readOrg
              def
              "* Any random text\n\
              \:PROPERTIES:\n\
              \:CUSTOM_ID: saunf-config\n\
              \:END:\n\
              \** Subsection"
          )
      let (Pandoc _ expectedBlocks) = either mempty id expectedDoc'

      config `shouldBe` Just (SaunfConfig expectedBlocks)

  describe "getReadmeTemplate" $ do
    it "returns Nothing if section with CUSTOM_ID='readme' section is not present in Configuration" $ do
      orgFile' <-
        P.runIO
          ( readOrg
              def
              "#+title: Title *from* Meta\n\n\
              \This is the description\n\n\
              \* Any random text\n\
              \:PROPERTIES:\n\
              \:CUSTOM_ID: saunf-config\n\
              \:END:\n\
              \** Readme\n\n\
              \No CUSTOM_ID though"
          )
      let orgFile = either mempty id orgFile'
      let config = getConfig orgFile
      let readmeTemplate = getReadmeTemplate =<< config

      readmeTemplate `shouldBe` Nothing

    it "returns Nothing if a code block is not present in readme section" $ do
      orgFile' <-
        P.runIO
          ( readOrg
              def
              "#+title: Title *from* Meta\n\n\
              \This is the description\n\n\
              \* Any random text\n\
              \:PROPERTIES:\n\
              \:CUSTOM_ID: saunf-config\n\
              \:END:\n\
              \** Readme\n\n\
              \:PROPERTIES:\n\
              \:CUSTOM_ID: readme\n\
              \:END:\n\
              \No code block for template"
          )
      let orgFile = either mempty id orgFile'
      let config = getConfig orgFile
      let readmeTemplate = getReadmeTemplate =<< config

      readmeTemplate `shouldBe` Nothing

    it "returns template text from first code block found in readme section" $ do
      orgFile' <-
        P.runIO
          ( readOrg
              def
              "#+title: Title *from* Meta\n\n\
              \This is the description\n\n\
              \* Any random text\n\
              \:PROPERTIES:\n\
              \:CUSTOM_ID: saunf-config\n\
              \:END:\n\
              \** Readme\n\
              \:PROPERTIES:\n\
              \:CUSTOM_ID: readme\n\
              \:END:\n\
              \#+begin_src markdown\n\
              \# $$title$$\n\
              \$$description$$\n\
              \## $$#features$$\n\
              \#+end_src\n"
          )
      let orgFile = either mempty id orgFile'
      let config = getConfig orgFile
      let readmeTemplate = getReadmeTemplate =<< config

      let expectedDoc =
            "# $$title$$\n\
            \$$description$$\n\
            \## $$#features$$\n"

      readmeTemplate `shouldBe` Just expectedDoc

  describe "buildReadme" $ do
    it "populates title and description variables if present" $ do
      pending

    it "populates title and description with empty strings if they are not present" $ do
      pending

    it "removes the section if it is not found in pmp doc" $ do
      pending

    it "replaces the section with entire section if it is found in pmp doc" $ do
      pending
