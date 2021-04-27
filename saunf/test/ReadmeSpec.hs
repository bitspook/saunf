{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ReadmeSpec where

import Control.Monad.Reader
import Saunf.Conf
import Saunf.Readme
import Saunf.Types
import Test.Hspec
import TestAssist
import Text.Pandoc as P hiding (Reader)

spec :: Spec
spec = do
  describe "findDescription" $ do
    it "returns Nothing if there is no text preceding first section" $ do
      orgFile <- readOrg' "#+title: Title *from* Meta\n\n* First section heading"
      let description = findDescription orgFile

      description `shouldBe` Nothing

    it "returns all text till first section" $ do
      orgFile <- readOrg' "#+title: Title *from* Meta\n\nThis is the description\n\n* First Section "
      let description = findDescription orgFile

      (Pandoc _ expectedDescription) <- readOrg' "This is the description"

      description `shouldBe` Just expectedDescription

  describe "parseInjectedSectionName" $ do
    it "gives Nothing if string is not a section-injection variable" $ do
      parseInjectedSectionName "$title$" `shouldBe` Nothing

    it "gives Just section-name if string is a valid section-injection variable" $ do
      parseInjectedSectionName "$#section$" `shouldBe` Just "section"
      parseInjectedSectionName "$#yoyo$" `shouldBe` Just "yoyo"

  describe "soberReadmeTemplate" $ do
    it "removes the section if it is not found in pmp-doc" $ do
      orgFile <- readOrg' ""
      let conf =
            SaunfConf
              ""
              ""
              ( Just
                  "# $$title$$\n\
                  \$$description$$\n\
                  \## $#features$ \n"
              )
              Nothing
              Info
      soberTemplate' <- P.runIO $ runReaderT soberReadmeTemplate (env {saunfDoc = orgFile, saunfConf = conf})
      soberTemplate <- P.handleError soberTemplate'

      expected' <-
        P.runIO $ do
          x <-
            readMarkdown
              def
              "# $$title$$\n\
              \$$description$$"
          writeMarkdown def x

      expected <- P.handleError expected'

      soberTemplate `shouldBe` expected

    it "replaces the section with entire section content if it is found in pmp-doc" $ do
      orgFile <-
        readOrg'
          "\
          \** Features\n\
          \:PROPERTIES:\n\
          \:CUSTOM_ID: features\n\
          \:END:\n\
          \Awesome features.\n"
      let conf =
            SaunfConf
              ""
              ""
              ( Just
                  "# $$title$$\n\
                  \$$description$$\n\
                  \## $#features$ \n"
              )
              Nothing
              Info
      soberTemplate' <- P.runIO $ runReaderT soberReadmeTemplate (env {saunfDoc = orgFile, saunfConf = conf})
      soberTemplate <- P.handleError soberTemplate'

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

      expected <- P.handleError expected'

      soberTemplate `shouldBe` expected

    it "adjust header levels of the injected section as per provided level in readme template" $ do
      orgFile <-
        readOrg'
          "\
          \* Features\n\
          \:PROPERTIES:\n\
          \:CUSTOM_ID: features\n\
          \:END:\n\
          \Awesome features.\n\
          \** Level two\n"
      let conf =
            SaunfConf
              ""
              ""
              ( Just
                  "# $$title$$\n\
                  \$$description$$\n\
                  \## $#features$ \n"
              )
              Nothing
              Info
      soberTemplate' <- P.runIO $ runReaderT soberReadmeTemplate (env {saunfDoc = orgFile, saunfConf = conf})
      soberTemplate <- P.handleError soberTemplate'

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

      expected <- P.handleError expected'

      soberTemplate `shouldBe` expected
