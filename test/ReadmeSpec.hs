{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ReadmeSpec where

import Control.Monad.Reader
import Saunf.Readme
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
  describe "findDescription" $ do
    it "returns Nothing if there is no text preceding first section" $ do
      orgFile' <- P.runIO (readOrg def "#+title: Title *from* Meta\n\n* First section heading")
      orgFile <- P.handleError orgFile'
      let description = findDescription orgFile

      description `shouldBe` Nothing

    it "returns all text till first section" $ do
      orgFile' <- P.runIO (readOrg def "#+title: Title *from* Meta\n\nThis is the description\n\n* First Section ")
      orgFile <- P.handleError orgFile'
      let description = findDescription orgFile

      expectedDoc' <- P.runIO (readOrg def "This is the description")
      (Pandoc _ expectedDescription) <- P.handleError expectedDoc'

      description `shouldBe` Just expectedDescription

  describe "parseInjectedSectionName" $ do
    it "gives Nothing if string is not a section-injection variable" $ do
      parseInjectedSectionName "$title$" `shouldBe` Nothing

    it "gives Just section-name if string is a valid section-injection variable" $ do
      parseInjectedSectionName "$#section$" `shouldBe` Just "section"
      parseInjectedSectionName "$#yoyo$" `shouldBe` Just "yoyo"

  describe "soberReadmeTemplate" $ do
    it "removes the section if it is not found in pmp-doc" $ do
      orgFile' <-
        P.runIO
          ( readOrg
              def
              "* Any random text\n\
              \:PROPERTIES:\n\
              \:CUSTOM_ID: saunf-conf\n\
              \:END:\n"
          )
      orgFile <- P.handleError orgFile'
      let conf =
            SaunfConf
              ( Just
                  "# $$title$$\n\
                  \$$description$$\n\
                  \## $#features$ \n"
              )
              Nothing
      soberTemplate' <- P.runIO $ runReaderT soberReadmeTemplate (SaunfEnv orgFile conf)
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
              \:CUSTOM_ID: saunf-conf\n\
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
      orgFile <- P.handleError orgFile'
      let conf =
            SaunfConf
              ( Just
                  "# $$title$$\n\
                  \$$description$$\n\
                  \## $#features$ \n"
              )
              Nothing
      soberTemplate' <- P.runIO $ runReaderT soberReadmeTemplate (SaunfEnv orgFile conf)
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
              \:CUSTOM_ID: saunf-conf\n\
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
      orgFile <- P.handleError orgFile'
      let conf =
            SaunfConf
              ( Just
                  "# $$title$$\n\
                  \$$description$$\n\
                  \## $#features$ \n"
              )
              Nothing
      soberTemplate' <- P.runIO $ runReaderT soberReadmeTemplate (SaunfEnv orgFile conf)
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
