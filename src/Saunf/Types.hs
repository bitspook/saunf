{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Saunf.Types where

import Colog
import Relude
import Saunf.Conf
import Control.Monad.Error.Class (MonadError)
import Text.Pandoc (Block, Pandoc, PandocMonad, PandocError, PandocIO)

-- | Pandoc don't have a concept of sections, but org-mode do. A section is
-- | essentially everything that follows a header (inclusive), until another
-- | header of same or higher level
data Section = Section {sectionTitle :: Block, sectionBody :: [Block]} deriving (Show, Eq)

data Issue = Issue
  { issueId :: Maybe Text,
    issueTitle :: Block,
    issueBody :: [Block]
  }
  deriving (Show, Eq)

-- | An $Reader$ environment for very saunf-specific utilities
data SaunfEnv m = SaunfEnv
  { -- | The work document. For now Saunf supports just a single org file
    saunfDoc :: Pandoc,
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

newtype CLI a = CLI
  { unCLI :: ReaderT (SaunfEnv CLI) PandocIO a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (SaunfEnv CLI),
      MonadError PandocError,
      PandocMonad
    )

class HasSaunfDoc a where
  getSaunfDoc :: a -> Pandoc
  setSaunfDoc :: Pandoc -> a -> a

class HasSaunfConf a where
  getSaunfConf :: a -> SaunfConf

instance HasSaunfDoc (SaunfEnv m) where
  getSaunfDoc (SaunfEnv doc _ _) = doc
  setSaunfDoc doc env = env {saunfDoc = doc}

instance HasSaunfConf (SaunfEnv m) where
  getSaunfConf (SaunfEnv _ conf _) = conf
