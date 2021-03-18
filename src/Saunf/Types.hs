{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Saunf.Types where

import Data.Text (Text)
import Saunf.Conf
import Text.Pandoc (Block, Pandoc)

-- | Pandoc don't have a concept of sections, but org-mode do. A section is
-- | essentially everything that follows a header (inclusive), until another
-- | header of same or higher level
data Section = Section {sectionTitle :: Block, sectionBody :: [Block]} deriving (Show, Eq)

-- | An $Reader$ environment for very saunf-specific utilities
data SaunfEnv = SaunfEnv
  { -- | The work document. For now Saunf supports just a single org file
    saunfDoc :: Pandoc,
    -- | Saunf Configuration
    saunfConf :: SaunfConf
  }
  deriving (Show)

data Issue = Issue
  { issueId :: Maybe Text,
    issueTitle :: Block,
    issueBody :: [Block]
  }
  deriving (Show, Eq)
