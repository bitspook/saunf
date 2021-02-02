{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ReadmeSpec where

import Data.Maybe
import Saunf
import Saunf.Readme
import Test.Hspec
import Text.Pandoc as P

inlines :: Block -> [Inline]
inlines = \case
  (Plain xs) -> xs
  (Para xs) -> xs
  _ -> []

spec :: Spec
spec = do
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

  describe "parseInjectedSectionName" $ do
    it "gives Nothing if string is not a section-injection variable" $ do
      parseInjectedSectionName "$title$" `shouldBe` Nothing

    it "gives Just section-name if string is a valid section-injection variable" $ do
      parseInjectedSectionName "$#section$" `shouldBe` Just "section"
      parseInjectedSectionName "$#yoyo$" `shouldBe` Just "yoyo"

  describe "adjustSectionLevel" $ do
    it "sets the section Header's level to given level" $ do
      orgFile' <-
        P.runIO
        (readOrg def "** Features\nAwesome features.")
      let (Pandoc _ section) = either mempty id orgFile'
      let (header1:_) = setSectionHeaderLevel 3 section
      let (header2:_) = setSectionHeaderLevel 1 section
      let (header3:_) = setSectionHeaderLevel 7 section

      case header1 of
        Header lvl _ _ -> lvl `shouldBe` 3
        x -> x `shouldBe` Null -- just to shut up the non-exhaustive pattern warning

      case header2 of
        Header lvl _ _ -> lvl `shouldBe` 1
        x -> x `shouldBe` Null -- just to shut up the non-exhaustive pattern warning

      case header3 of
        Header lvl _ _ -> lvl `shouldBe` 7
        x -> x `shouldBe` Null -- just to shut up the non-exhaustive pattern warning

    it "shifts every sub-Header's level properly" $ do
      orgFile' <-
        P.runIO
        (readMarkdown def "## Header\n\
                          \### Sub header\n\
                          \#### Sub Sub header\n")
      let (Pandoc _ section) = either mempty id orgFile'
      let (header:sHeader:ssHeader:_) = setSectionHeaderLevel 4 section

      case header of
        Header lvl _ _ -> lvl `shouldBe` 4
        x -> x `shouldBe` Null -- just to shut up the non-exhaustive pattern warning

      case sHeader of
        Header lvl _ _ -> lvl `shouldBe` 5
        x -> x `shouldBe` Null -- just to shut up the non-exhaustive pattern warning

      case ssHeader of
        Header lvl _ _ -> lvl `shouldBe` 6
        x -> x `shouldBe` Null -- just to shut up the non-exhaustive pattern warning

  describe "soberizeReadmeTemplate" $ do
    it "removes the section if it is not found in pmp-doc" $ do
      orgFile' <-
        P.runIO
          ( readOrg
              def
              "\
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
              \## $#features$ \n\
              \#+end_src\n"
          )
      let orgFile@(Pandoc _ bs) = either mempty id orgFile'
      let config = getConfig orgFile
      let readmeTemplate = fromMaybe mempty $ getReadmeTemplate =<< config
      soberTemplate' <- P.runIO $ soberizeReadmeTemplate readmeTemplate bs
      let soberTemplate = either mempty id soberTemplate'

      expected' <-
        P.runIO $ do
          x <-
            readMarkdown
              def
              "# $$title$$\n\
              \$$description$$"
          writeMarkdown def x

      let expected = either mempty id expected'

      soberTemplate `shouldBe` expected

    it "replaces the section with entire section content if it is found in pmp-doc" $ do
      orgFile' <-
        P.runIO
          ( readOrg
              def
              "\
              \** Features\n\
              \:PROPERTIES:\n\
              \:CUSTOM_ID: features\n\
              \:END:\n\
              \Awesome features.\n\
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
              \## $#features$ \n\
              \#+end_src\n"
          )
      let orgFile@(Pandoc _ bs) = either mempty id orgFile'
      let config = getConfig orgFile
      let readmeTemplate = fromMaybe mempty $ getReadmeTemplate =<< config
      soberTemplate' <- P.runIO $ soberizeReadmeTemplate readmeTemplate bs
      let soberTemplate = either mempty id soberTemplate'

      expected' <-
        P.runIO $ do
          x <-
            readMarkdown
              def
              "# $$title$$\n\
              \$$description$$\n\
              \## Features\n\
              \Awesome features.\n"
          writeMarkdown def x

      let expected = either mempty id expected'

      soberTemplate `shouldBe` expected

    it "adjust header levels of the injected section as per provided level in readme template" $ do
      orgFile' <-
        P.runIO
          ( readOrg
              def
              "\
              \* Features\n\
              \:PROPERTIES:\n\
              \:CUSTOM_ID: features\n\
              \:END:\n\
              \Awesome features.\n\
              \** Level two\n\
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
              \## $#features$ \n\
              \#+end_src\n"
          )
      let orgFile@(Pandoc _ bs) = either mempty id orgFile'
      let config = getConfig orgFile
      let readmeTemplate = fromMaybe mempty $ getReadmeTemplate =<< config
      soberTemplate' <- P.runIO $ soberizeReadmeTemplate readmeTemplate bs
      let soberTemplate = either mempty id soberTemplate'

      expected' <-
        P.runIO $ do
          x <-
            readMarkdown
              def
              "# $$title$$\n\
              \$$description$$\n\
              \## Features\n\
              \Awesome features.\n\
              \### Level two"
          writeMarkdown def x

      let expected = either mempty id expected'

      soberTemplate `shouldBe` expected
