{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Saunf.Issue where

import Control.Monad.Reader
import Relude
import Saunf.Shared (filterSections)
import Saunf.Types
import qualified Text.Pandoc as P

hasCategory :: Text -> P.Block -> Bool
hasCategory cat block = case block of
  (P.Header _ (_, _, props) _) -> any (\p -> fst p == "category" && snd p == cat) props
  _ -> True

isHeaderWithLevel :: Int -> P.Block -> Bool
isHeaderWithLevel lvl block = case block of
  (P.Header lvl' _ _) -> lvl == lvl'
  _ -> False

issues :: Reader SaunfEnv [Issue]
issues = do
  env <- ask
  containers <- filterSections (hasCategory "issues")
  let containerSections = concat $ flip runReader env . issuesFromContainer <$> containers
  sections <- filterSections (hasCategory "issue")
  return $ Issue <$> sections ++ containerSections
  where
    issuesFromContainer :: Section -> Reader SaunfEnv [Section]
    issuesFromContainer = \case
      (Section (P.Header lvl _ _) body) ->
        local (\env -> env {saunfDoc = P.Pandoc mempty body}) $
          filterSections (isHeaderWithLevel (lvl + 1))
      _ -> return []
