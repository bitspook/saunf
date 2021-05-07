{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Saunf.Readme
  ( description,
    soberReadmeTemplate,
    parseInjectedSectionName,
    readme,
  )
where

import Colog (Message, WithLog, log, pattern D, pattern E)
import Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Map as M
import Data.Org
  ( Block (..),
    OrgDoc (..),
    OrgFile (..),
    Section (..),
    org,
    prettyOrgFile,
    prettyWords,
    prettyBlock
  )
import Relude hiding ((<|>))
import Saunf.Conf
import Saunf.Shared
import Saunf.Types
import Text.DocLayout (render)
import Text.DocTemplates (compileTemplate, renderTemplate)
import Text.Megaparsec as P (Parsec, many, parse, (<|>))
import Text.Megaparsec.Char (char, letterChar)

-- | Grab description part of saunf-doc
description :: (MonadReader e m, HasSaunfDoc e) => m [Block]
description = asks $ docBlocks . orgDoc . getSaunfDoc

-- | Parse the name of the section which should be injected out of special
-- syntax "$#section-name$".
parseInjectedSectionName :: Text -> Maybe Text
parseInjectedSectionName xs = either (const Nothing) Just $ parse nameParser "" xs
  where
    nameParser :: Parsec Void Text Text
    nameParser = do
      _ <- char '$'
      _ <- char '#'
      name <- P.many (letterChar <|> char '-')
      _ <- char '$'
      return $ toText name

-- | Try to expand a Section to a Section if it is a valid injection spot
explodeMaybe :: OrgFile -> Section -> Section
explodeMaybe doc sec@Section {sectionHeading = heading} = fromMaybe sec sec'
  where
    cid = parseInjectedSectionName (unwords . toList $ prettyWords <$> heading)
    sec' = findSection doc =<< cid

-- | Evaluate all Saunf syntax in readme template to produce a valid Pandoc
-- template. It parses readme template as an org file, operate on it to remove
-- all special syntax, and return back a markdown string
soberReadmeTemplate ::
  ( MonadReader e m,
    HasSaunfDoc e,
    HasSaunfConf e
  ) =>
  m (Either SaunfError Text)
soberReadmeTemplate = do
  tStr' <- asks (readmeTemplate . getSaunfConf)
  doc <- asks getSaunfDoc
  case tStr' of
    Nothing -> return $ Left (SaunfConfError "Readme Template not found")
    (Just tStr) -> do
      let tOrgFile = walkSections (explodeMaybe doc) <$> org tStr
      return . maybeToRight (ReadmeError "Failed to create readme") $ prettyOrgFile <$> tOrgFile

data ReadmeContext = ReadmeContext
  { readmeTitle :: Text,
    readmeDescription :: Text
  }

instance ToJSON ReadmeContext where
  toJSON e =
    object
      [ "title" .= readmeTitle e,
        "description" .= readmeDescription e
      ]

-- | Resolve Saunf-special syntax from readme-template given in conf, populates
-- it with variables, and produce final Readme file's content in markdown format
readme ::
  ( HasSaunfDoc e,
    HasSaunfConf e,
    WithLog e Message m,
    MonadIO m
  ) =>
  m Text
readme = do
  OrgFile meta _ <- asks getSaunfDoc
  log D "Sobering up saunf-syntax from readme template"
  soberTemplate' <- soberReadmeTemplate
  let soberTemplate = fromRight "" soberTemplate'

  let title = fromMaybe "" $ M.lookup "title" meta

  log D "Rendering description to markdown"
  desc' <- description
  let desc = unwords $ prettyBlock <$> desc'

  let context = ReadmeContext title desc

  log D "Compiling sober readme template"
  tc <- liftIO $ compileTemplate "readme.org" soberTemplate
  case tc of
    Left e -> do
      log E $ "Encountered error when parsing readme template.\n" <> Relude.toText e
      error (Relude.toText e)
    Right t -> do
      log D "Rendering compiled readme template"
      return $ render Nothing $ renderTemplate t (toJSON context)
