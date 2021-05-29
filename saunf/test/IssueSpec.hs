{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module IssueSpec where

import Relude
import Saunf.Issue
import Saunf.Shared
import Saunf.Types
import Test.Hspec
import TestAssist

spec :: Spec
spec = do
  describe "issues" $ do
    it "returns empty list if no issues are present" $ do
      orgFile <- readSaunfDoc' ""
      let result = runReader issues (env {saunfDoc = orgFile})

      result `shouldBe` []

    -- it "returns all sections which have property CATEGORY=issue" $ do
    --   orgFile <- readOrg' ""
    --   let result = runReader issues (env {saunfDoc = orgFile})
    --   let expected =
    --         (\(Section title body) -> Issue Nothing (inlines title) body [])
    --           <$> runReader (filterSections (const True)) env

    --   result `shouldBe` expected

    -- it "returns all sub-sections of sections which have property CATEGORY=issues" $ do
    --   orgFile <-
    --     readOrg'
    --       "* Issues\n\
    --       \:PROPERTIES:\n\
    --       \:CATEGORY: issues\n\
    --       \:END:\n\
    --       \** Issue 1\n\
    --       \** Issue 2\n"
    --   let result = runReader issues (env {saunfDoc = orgFile})

    --   let expected =
    --         [ Issue Nothing [Str "Issue", Space, Str "1"] [] [("todo", "")],
    --           Issue Nothing [Str "Issue", Space, Str "2"] [] [("todo", "")]
    --         ]

    --   result `shouldBe` expected

    -- it "adds issue-id for issues which have ISSUE_ID property" $ do
    --   orgFile <-
    --     readOrg'
    --       "* Issues\n\
    --       \:PROPERTIES:\n\
    --       \:CATEGORY: issues\n\
    --       \:END:\n\
    --       \** Issue 1\n\
    --       \:PROPERTIES:\n\
    --       \:ISSUE_ID: 23\n\
    --       \:END:\n\
    --       \** Issue 2\n"
    --   let result = runReader issues (env {saunfDoc = orgFile})

    --   let expected =
    --         [ Issue (Just "23") [Str "Issue", Space, Str "1"] [] [("issue_id", "23"), ("todo", "")],
    --           Issue Nothing [Str "Issue", Space, Str "2"] [] [("todo", "")]
    --         ]

    --   result `shouldBe` expected
