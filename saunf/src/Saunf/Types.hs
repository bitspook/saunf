{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Saunf.Types
  ( SaunfError (..),
    Issue (..),
    SaunfEnv (..),
    HasSaunfDoc (..),
    HasSaunfConf (..),
  )
where

import Colog
  ( HasLog (..),
    LogAction,
    Message,
  )
import qualified Data.Map as M
import Data.Org (OrgDoc, OrgFile, Words)
import Relude
import Saunf.Conf
import Saunf.Error

data Issue = Issue
  { issueId :: Maybe Text,
    issueTitle :: NonEmpty Words,
    issueBody :: OrgDoc,
    issueMeta :: M.Map Text Text
  }
  deriving (Show, Eq)

-- | An $Reader$ environment for very saunf-specific utilities
data SaunfEnv m = SaunfEnv
  { -- | The work document. For now Saunf supports just a single org file
    saunfDoc :: OrgFile,
    -- | Saunf Configuration
    saunfConf :: SaunfConf,
    saunfLogAction :: !(LogAction m Message)
  }

instance HasLog (SaunfEnv m) Message m where
  getLogAction :: SaunfEnv m -> LogAction m Message
  getLogAction = saunfLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> SaunfEnv m -> SaunfEnv m
  setLogAction newLogAction env = env {saunfLogAction = newLogAction}
  {-# INLINE setLogAction #-}

class HasSaunfDoc a where
  getSaunfDoc :: a -> OrgFile
  setSaunfDoc :: OrgFile -> a -> a

class HasSaunfConf a where
  getSaunfConf :: a -> SaunfConf

instance HasSaunfDoc (SaunfEnv m) where
  getSaunfDoc (SaunfEnv doc _ _) = doc
  setSaunfDoc doc env = env {saunfDoc = doc}

instance HasSaunfConf (SaunfEnv m) where
  getSaunfConf (SaunfEnv _ conf _) = conf
