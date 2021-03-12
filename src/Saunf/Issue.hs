{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Saunf.Issue where

import Control.Monad.Reader
import Relude
import Saunf.Shared
import Saunf.Types
import qualified Text.Pandoc as P

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
