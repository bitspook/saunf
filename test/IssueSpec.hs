{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module IssueSpec where

import Saunf.Issue
import Test.Hspec
import Text.Pandoc as P

spec :: Spec
spec = do
  describe "epics" $ do
    it "returns empty list of no epics are present" $ do
      pending

    it "returns all sections with have property CATEGORY=epic" $ do
      pending

    it "returns all direct-children of sections which have property CATEGORY=epics" $ do
      pending

  describe "issues" $ do
    it "returns empty list if no issues are present" $ do
      pending

    it "returns all child-sections of all epics" $ do
      pending
