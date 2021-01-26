{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module ReadmeSpec where

import Test.Hspec
import Saunf.Readme
import Text.Pandoc as P
import Text.Pandoc.Walk

inlines :: Block -> [Inline]
inlines = \case
  (Plain xs) -> xs
  (Para xs) -> xs
  _ -> []

spec :: Spec
spec = do
  describe "grabReadmeTitle" $ do
    context "title is present in metadata" $ do
      it "should pick title from metadata if present" $ do
        orgFile' <- P.runIO (readOrg def "#+title: Title *from* Meta\n")
        let orgFile = either mempty id orgFile'
        let title = grabReadmeTitle orgFile

        expectedDoc <- P.runIO (readOrg def "Title *from* Meta")
        let expectedTitle = query inlines expectedDoc

        title `shouldBe` Just expectedTitle

    context "title is not present in metadata" $ do
      it "should pick first available section's heading" $ do
        orgFile' <-  P.runIO $ readOrg def "Some random text\n\n* Yo Yo Yo"
        let orgFile = either mempty id orgFile'
        let title = grabReadmeTitle orgFile

        expectedDoc <- P.runIO (readOrg def "Yo Yo Yo")
        let expectedTitle = query inlines expectedDoc

        title `shouldBe` Just expectedTitle

      it "should pick first available section's heading regardless of its level" $ do
        orgFile' <-  P.runIO $ readOrg def "Some random text\n\n** Level two Yo"
        let orgFile = either mempty id orgFile'
        let title2 = grabReadmeTitle orgFile

        expectedDoc2 <- P.runIO $ readOrg def "Level two Yo"
        let expectedTitle2 = query inlines expectedDoc2

        title2 `shouldBe` Just expectedTitle2

    it "should return Nothing if neither meta has a title, nor there is any section" $ do
      orgFile' <-  P.runIO $ readOrg def "Some random text"
      let orgFile = either mempty id orgFile'
      let title = grabReadmeTitle orgFile

      title `shouldBe` Nothing

  describe "buildReadme" $ do
    it "should take entire org document for rendering" $ do
      orgFile' <- P.runIO $ readOrg def "#+title: The title bro\n\nSome text\n\n* First section\n\nFirst section content"
      let orgFile@(Pandoc _ bs) = either mempty id orgFile'
      let (Readme _ expectedBs) = buildReadme orgFile

      expectedBs `shouldBe` bs
