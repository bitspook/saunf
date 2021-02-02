{-# LANGUAGE OverloadedStrings #-}

module Saunf.Config where

import Text.Pandoc

newtype SaunfConfig
  = SaunfConfig [Block]
  deriving (Show, Eq)
