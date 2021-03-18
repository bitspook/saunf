{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Saunf.Issue where

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

issues :: Reader SaunfEnv [Issue]
issues = do
  env <- ask
  containers <- filterSections (hasCategory "issues")
  let containerSections = concat $ flip runReader env . issuesFromContainer <$> containers
  sections <- filterSections (hasCategory "issue")
  return $ (\(Section title body) -> Issue (issueId title) title body) <$> sections ++ containerSections
  where
    issueId :: P.Block -> Maybe Text
    issueId (P.Header _ (_, _, props) _) = snd <$> find (\(name, _) -> name == "issue_id") props
    issueId _ = Nothing

    issuesFromContainer :: Section -> Reader SaunfEnv [Section]
    issuesFromContainer = \case
      (Section (P.Header lvl _ _) body) ->
        local (\env -> env {saunfDoc = P.Pandoc mempty body}) $
          filterSections (isHeaderWithLevel (lvl + 1))
      _ -> return []

createGithubIssue :: Issue -> ReaderT SaunfEnv IO (Either GH.Error GH.Issue)
createGithubIssue (Issue _ title body) = do
  ghConf' <- asks $ github . saunfConf
  case ghConf' of
    Nothing -> error "Could not find Github configuration"
    Just (GithubConf user repo token) -> do
      let auth = GH.OAuth (encodeUtf8 token)
      titleText <- writeMd [title]
      bodyText <- writeMd body
      let newGhIssue = (GH.newIssue titleText) {GH.newIssueBody = Just bodyText}
      let issueReq = GH.createIssueR user repo newGhIssue

      liftIO $ GH.github auth issueReq

push :: ReaderT SaunfEnv IO ()
push = do
  env <- ask
  let allIssues = runReader issues env
  let newIssues = filter (isNothing . issueId) allIssues
  createdIssues <- mapM createGithubIssue newIssues
  let _ = handleCreatedIssue <$> createdIssues
  return ()
  where
    handleCreatedIssue :: Either GH.Error GH.Issue -> IO ()
    handleCreatedIssue = \case
      Left err -> putStrLn $ "Failed to create Github issue: " ++ show err
      Right issue -> putStrLn $ "Successfully created Github issue: " ++ show issue
