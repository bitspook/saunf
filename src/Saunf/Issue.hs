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
  return $ (\(Section title body) -> Issue (issueId title) title body) <$> sections ++ containerSections
  where
    issueId :: P.Block -> Maybe Text
    issueId ( P.Header _ (_, _, props) _) = snd <$> find (\(name, _) -> name == "issue_id") props
    issueId _ = Nothing

    issuesFromContainer :: Section -> Reader SaunfEnv [Section]
    issuesFromContainer = \case
      (Section (P.Header lvl _ _) body) ->
        local (\env -> env {saunfDoc = P.Pandoc mempty body}) $
          filterSections (isHeaderWithLevel (lvl + 1))
      _ -> return []
