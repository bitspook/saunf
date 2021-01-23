{-# LANGUAGE OverloadedStrings #-}
module SaunfSpec where

import Test.Hspec
import Saunf
import Data.Org (org, orgDoc)

spec :: Spec
spec = do
  describe "grabReadmeTitle" $ do
    it "should pick title from metadata if present" $ do
      let orgFile = org "#+title: Title From Meta\n"
      let title = grabReadmeTitle <$> orgFile

      title `shouldBe` Just (Just "Title From Meta")

    it "should pick first section's heading if it is not present in metadata" $ do
      -- TODO This might be a bug in org-mode. It needs 2 new-lines to consider next heading a
      -- new section. Check it out.
      let orgFile = org "Some random text\n\n* Yo Yo Yo"
      let title = grabReadmeTitle <$> orgFile
      let orgDoc2 = org "Some random text\n\n** Level two Yo"
      let title2 = grabReadmeTitle <$> orgDoc2

      title `shouldBe` Just (Just "Yo Yo Yo")
      title2 `shouldBe` Just (Just "Level two Yo")

    it "should return Nothing if neither meta has a title, nor there is any section" $ do
      let orgFile = org "Some random text"
      let title = grabReadmeTitle <$> orgFile

      title `shouldBe` Just Nothing

  describe "buildReadme" $ do
    it "should take entire org document for rendering" $ do
      let orgFile = org "#+title: The title bro\n\nSome text\n\n* First section\n\nFirst section content"
      let readme = buildReadme <$> orgFile

      readmeDoc <$> readme `shouldBe` orgDoc <$> orgFile
