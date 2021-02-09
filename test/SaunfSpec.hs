{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module SaunfSpec where

import Saunf
import Saunf.Types
import Test.Hspec
import Text.Pandoc as P hiding (Reader)
import Control.Monad.Reader

inlines :: Block -> [Inline]
inlines = \case
  (Plain xs) -> xs
  (Para xs) -> xs
  _ -> []

spec :: Spec
spec = do
  describe "getConfig" $ do
    it "gives $Left$ $ConfNotFound$ if a section with CUSTOM_ID='saunf-conf' is not present" $ do
      orgFile' <-
        P.runIO
          ( readOrg
              def
              "#+title: Title *from* Meta\n\n\
              \This is the description\n\n\
              \:END:\n"
          )
      orgFile <- P.handleError orgFile'
      let conf = runReader getConfig (SaunfEnv orgFile mempty)

      conf `shouldBe` Left ConfNotFound

    it "returns $Right$ $SaunfConf$ if a section with CUSTOM_ID='saunf-conf' is present" $ do
      orgFile' <-
        P.runIO
          ( readOrg
              def
              "#+title: Title *from* Meta\n\n\
              \This is the description\n\n\
              \* Any random text\n\
              \:PROPERTIES:\n\
              \:CUSTOM_ID: saunf-conf\n\
              \:END:\n\
              \** Subsection"
          )
      orgFile <- P.handleError orgFile'
      let conf = runReader getConfig (SaunfEnv orgFile mempty)

      conf `shouldBe` Right (SaunfConf Nothing)
