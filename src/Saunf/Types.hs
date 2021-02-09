{-# LANGUAGE OverloadedStrings #-}

module Saunf.Types where

import Data.Text (Text)
import Text.Pandoc (Block, Pandoc)

data SaunfConfError = ConfNotFound | ConflictingConf deriving (Show, Eq)

newtype SaunfConf = SaunfConf {readmeTemplate :: Maybe Text}
  deriving (Show, Eq)

instance Semigroup SaunfConf where
  (SaunfConf as) <> (SaunfConf bs) = SaunfConf ((<>) <$> as <*> bs)

instance Monoid SaunfConf where
  mempty = SaunfConf Nothing

-- | Pandoc don't have a concept of sections, but org-mode do. A section is
-- | essentially everything that follows a header (inclusive), until another
-- | header of same or higher level
newtype Section = Section [Block] deriving (Show, Eq)

data SaunfEnv = SaunfEnv {saunfDoc :: Pandoc, saunfConf :: SaunfConf} deriving (Show)
