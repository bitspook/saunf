{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TestAssist where

import Data.Text
import Text.Pandoc as P
import Relude
import Saunf.Types
import Saunf.Conf
import Colog (simpleMessageAction)

readOrg' :: Text -> IO Pandoc
readOrg' str = P.handleError =<< P.runIO (readOrg def str)

newtype Test a = Test
  { unTest :: ReaderT (SaunfEnv Test) IO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (SaunfEnv Test))

inlines :: Block -> [Inline]
inlines = \case
  (Plain xs) -> xs
  (Para xs) -> xs
  _ -> []

env :: SaunfEnv Test
env = SaunfEnv mempty emptyConf simpleMessageAction
