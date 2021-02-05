{-# LANGUAGE OverloadedStrings #-}

module Saunf.Types where

import Text.Pandoc (Pandoc, Block)

newtype SaunfConf
  = SaunfConf [Block]
  deriving (Show, Eq)

instance Semigroup SaunfConf where
  (SaunfConf as) <> (SaunfConf bs) = SaunfConf (as ++ bs)

instance Monoid SaunfConf where
  mempty = SaunfConf []

-- | Pandoc don't have a concept of sections, but org-mode do. A section is
-- | essentially everything that follows a header (inclusive), until another
-- | header of same or higher level
newtype Section = Section [Block] deriving (Show, Eq)

data SaunfEnv = SaunfEnv { saunfDoc :: Pandoc, saunfConf :: SaunfConf } deriving (Show)
