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

import Relude
import Saunf.Types
import Saunf.Conf
import Saunf.Shared
import Control.Monad.Catch (MonadThrow)
import Data.Org (emptyOrgFile, OrgFile)
import Colog (simpleMessageAction)

newtype Test a = Test
  { unTest :: ReaderT (SaunfEnv Test) IO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (SaunfEnv Test))

readSaunfDoc' :: (MonadIO m, MonadThrow m) => Text -> m OrgFile
readSaunfDoc' xs = readSaunfDoc xs `orDie` UnknownSaunfError "Invalid SaunfDoc"

env :: SaunfEnv Test
env = SaunfEnv emptyOrgFile emptyConf simpleMessageAction
