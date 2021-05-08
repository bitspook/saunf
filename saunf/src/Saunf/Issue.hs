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
import qualified Data.Map as M
import Data.Org (OrgDoc (..), Section (..), orgDoc, prettyWords, prettyOrg)
import qualified GitHub.Auth as GH
import qualified GitHub.Data.Issues as GH
import qualified GitHub.Endpoints.Issues as GH
import qualified GitHub.Request as GH
import Relude
import Saunf.Conf
import Saunf.Shared
import Saunf.Types

issues :: (MonadReader e m, HasSaunfDoc e) => m [Issue]
issues = do
  doc <- asks $ orgDoc . getSaunfDoc
  let containerSections = concat $ docSections . sectionDoc <$> filterSections (hasCategory "issues") doc
  let individualSections = filterSections (hasCategory "issue") doc
  return $
    ( \Section {sectionHeading = header, sectionProps = props, sectionDoc = body} -> do
        Issue
          (M.lookup "issue_id" props)
          header
          body
          props
    )
      <$> (individualSections ++ containerSections)

filterNewIssues :: [Issue] -> [Issue]
filterNewIssues =
  filter
    ( \x ->
        (isNothing . issueId $ x)
        -- FIXME When we have configurable todo statuses
        || M.lookup "todo" (issueMeta x) == Just "DONE"
    )

createGithubIssue ::
  ( HasSaunfDoc e,
    HasSaunfConf e,
    MonadIO m,
    WithLog e Message m
  ) =>
  Issue ->
  m (Either SaunfError GH.Issue)
createGithubIssue (Issue _ title body _) = do
  ghConf' <- asks $ github . getSaunfConf
  case ghConf' of
    Nothing -> return $ Left (SaunfConfError "Missing github configuration")
    Just (GithubConf user repo token) -> do
      let auth = GH.OAuth (encodeUtf8 token)

      let titleText = unwords . toList $ prettyWords <$> title
      let bodyText = prettyOrg body

      let newGhIssue = (GH.newIssue titleText) {GH.newIssueBody = Just bodyText}
      let issueReq = GH.createIssueR user repo newGhIssue

      log D $ "Creating Github issue: " <> "\n\t[Title] " <> titleText <> "\n\t[Body] " <> bodyText
      result <- liftIO $ GH.github auth issueReq

      return $ first GithubError result
