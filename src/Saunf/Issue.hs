{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Saunf.Issue where

import Colog
  ( Message,
    WithLog,
    log,
    pattern D,
    pattern E,
    pattern I,
  )
import Control.Monad.Reader
import qualified GitHub.Auth as GH
import qualified GitHub.Data.Issues as GH
import qualified GitHub.Endpoints.Issues as GH
import qualified GitHub.Request as GH
import Relude
import Saunf.Conf
import Saunf.Shared
import Saunf.Types
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Shared as P

issues :: (MonadReader e m, HasSaunfDoc e) => m [Issue]
issues = do
  env <- ask
  containers <- filterSections (hasCategory "issues")
  let containerSections = concat $ flip runReader env . issuesFromContainer <$> containers
  sections <- filterSections (hasCategory "issue")
  return $
    ( \(Section h body) -> do
        let header = sterileSectionHeader h
        Issue
          (issueId header)
          (title header)
          body
          (issueMeta header)
    )
      <$> sections
      ++ containerSections
  where
    issueId = \case
      (P.Header _ (_, _, props) _) -> snd <$> find (\(name, _) -> name == "issue_id") props
      _ -> Nothing

    title = \case
      (P.Header _ _ xs) -> xs
      _ -> mempty

    issueMeta a = case a of
      (P.Header _ (_, _, props) _) -> props
      _ -> []

    issuesFromContainer :: (MonadReader e m, HasSaunfDoc e) => Section -> m [Section]
    issuesFromContainer = \case
      (Section (P.Header lvl _ _) body) ->
        local (setSaunfDoc $ P.Pandoc mempty body) $
          filterSections (isHeaderWithLevel (lvl + 1))
      _ -> return []

createGithubIssue ::
  ( HasSaunfDoc e,
    HasSaunfConf e,
    MonadIO m,
    WithLog e Message m,
    P.PandocMonad m
  ) =>
  Issue ->
  m (Either SaunfError GH.Issue)
createGithubIssue (Issue _ title body _) = do
  ghConf' <- asks $ github . getSaunfConf
  case ghConf' of
    Nothing -> return $ Left (SaunfConfError "Missing github configuration")
    Just (GithubConf user repo token) -> do
      let auth = GH.OAuth (encodeUtf8 token)

      let titleText = P.stringify title
      bodyText <- writeMd body

      let newGhIssue = (GH.newIssue titleText) {GH.newIssueBody = Just bodyText}
      let issueReq = GH.createIssueR user repo newGhIssue

      log D $ "Creating Github issue: " <> "\n\t[Title] " <> titleText <> "\n\t[Body] " <> bodyText
      result <- liftIO $ GH.github auth issueReq

      return $ first GithubError result

filterNewIssues :: [Issue] -> [Issue]
filterNewIssues =
  filter
    ( \x ->
        (isNothing . issueId $ x)
        -- FIXME When we have configurable todo statuses
          || fmap snd (find (\x -> fst x == "todo") (issueMeta x)) == Just "DONE"
    )
