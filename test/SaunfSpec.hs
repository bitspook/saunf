{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module SaunfSpec where

import Saunf
import Saunf.Config
import Test.Hspec
import Text.Pandoc as P

inlines :: Block -> [Inline]
inlines = \case
  (Plain xs) -> xs
  (Para xs) -> xs
  _ -> []

spec :: Spec
spec = do
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
