{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module IssueSpec where

import Relude
import Saunf.Issue
import Saunf.Conf
import Saunf.Shared
import Saunf.Types
import Test.Hspec
import Text.Pandoc as P

spec :: Spec
spec = do
  describe "issues" $ do
    it "returns empty list if no issues are present" $ do
      orgFile <- P.handleError =<< P.runIO (readOrg def "")
      let result = runReader issues (SaunfEnv orgFile emptyConf)

      result `shouldBe` []

    it "returns all sections which have property CATEGORY=issue" $ do
      orgFile <- P.handleError =<< P.runIO (readOrg def "")
      let env = SaunfEnv orgFile emptyConf
      let result = runReader issues env
      let expected =
            (\(Section title body) -> Issue Nothing title body)
              <$> runReader (filterSections (const True)) env

      result `shouldBe` expected

    it "returns all sub-sections of sections which have property CATEGORY=issues" $ do
      orgFile <-
        P.handleError
          =<< P.runIO
            ( readOrg
                def
                "* Issues\n\
                \:PROPERTIES:\n\
                \:CATEGORY: issues\n\
                \:END:\n\
                \** Issue 1\n\
                \** Issue 2\n"
            )
      let env = SaunfEnv orgFile emptyConf
      let result = runReader issues env

      let expected =
            [ Issue Nothing (Header 2 ("", [], []) [Str "Issue", Space, Str "1"]) [],
              Issue Nothing (Header 2 ("", [], []) [Str "Issue", Space, Str "2"]) []
            ]

      result `shouldBe` expected

    it "adds issue-id for issues which have ISSUE_ID property" $ do
      orgFile <-
        P.handleError
          =<< P.runIO
            ( readOrg
                def
                "* Issues\n\
                \:PROPERTIES:\n\
                \:CATEGORY: issues\n\
                \:END:\n\
                \** Issue 1\n\
                \:PROPERTIES:\n\
                \:ISSUE_ID: 23\n\
                \:END:\n\
                \** Issue 2\n"
            )
      let env = SaunfEnv orgFile emptyConf
      let result = runReader issues env

      let expected =
            [ Issue (Just "23") (Header 2 ("", [], [("issue_id", "23")]) [Str "Issue", Space, Str "1"]) [],
              Issue Nothing (Header 2 ("", [], []) [Str "Issue", Space, Str "2"]) []
            ]

      result `shouldBe` expected
