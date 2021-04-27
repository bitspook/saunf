{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Saunf.Readme
  ( findDescription,
    soberReadmeTemplate,
    parseInjectedSectionName,
    readme,
  )
where

import Colog (Message, WithLog, log, pattern D, pattern E)
import Relude hiding ((<|>))
import Saunf.Conf
import Saunf.Shared
import Saunf.Types
import Text.DocLayout (render)
import Text.Pandoc as P hiding (Reader)
import Text.Pandoc.Shared
import Text.Pandoc.Writers.Shared
import Text.Parsec as Parsec

findDescription :: Pandoc -> Maybe [Block]
findDescription (Pandoc _ bs) = case takeWhile (not . isHeaderBlock) bs of
  [] -> Nothing
  xs -> Just xs

-- | Parse the name of the section which should be injected out of special
-- syntax "$#section-name$".
parseInjectedSectionName :: Text -> Maybe Text
parseInjectedSectionName xs = either (const Nothing) (Just . Relude.toText) $ parse nameParser "" xs
  where
    nameParser = do
      _ <- Parsec.char '$'
      _ <- Parsec.char '#'
      name <- Parsec.many (Parsec.letter <|> Parsec.char '-' <|> Parsec.char '_')
      _ <- Parsec.char '$'
      return name

-- | Try to expand a Block to a Section if it is a valid injection spot
explodeMaybe :: (MonadReader e m, HasSaunfDoc e) => Block -> m [Block]
explodeMaybe a =
  case a of
    (Header lvl _ xs) -> do
      let grabSection id = do
            sections <- filterSections (hasId id)
            case sections of
              [] -> return $ Section P.Null []
              (s : _) -> return s
      blks <- case parseInjectedSectionName (stringify xs) of
        Nothing -> return $ Section P.Null [a]
        Just name -> grabSection name
      let (Section explodedHeader explodedBlocks) = setSectionHeaderLevel lvl blks
      return $ explodedHeader : explodedBlocks
    _ -> return [a]

-- | Evaluate all Saunf syntax in readme template to produce a valid Pandoc
-- template. It parses readme template as markdown, operate on it to remove all
-- special syntax, and return back a markdown string
soberReadmeTemplate ::
  ( MonadReader e m,
    HasSaunfDoc e,
    HasSaunfConf e,
    PandocMonad m
  ) =>
  m Text
soberReadmeTemplate = do
  tStr' <- asks (readmeTemplate . getSaunfConf)
  case tStr' of
    -- FIXME: Return "Either ConfError Text" instead of throwing a hissy-fit
    Nothing -> error "Template not found"
    (Just tStr) -> do
      (Pandoc tMeta tBlocks) <- readMarkdown def tStr
      blocks <- mapM explodeMaybe tBlocks
      let expandedBlocks :: [Block] = foldr (<>) [] blocks
      writeMarkdown def (Pandoc tMeta expandedBlocks)

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
    MonadIO m,
    PandocMonad m
  ) =>
  m Text
readme = do
  sDoc@(Pandoc meta _) <- asks getSaunfDoc
  log D "Sobering up saunf-syntax from readme template"
  soberTemplate <- soberReadmeTemplate

  let title = lookupMetaString "title" meta

  log D "Rendering description to markdown"
  description <- writeMd $ fromMaybe mempty (findDescription sDoc)

  let context = ReadmeContext title description

  log D "Compiling sober readme template"
  tc <- liftIO $ compileTemplate "readme.md" soberTemplate
  case tc of
    Left e -> do
      log E $ "Encountered error when parsing readme template.\n" <> Relude.toText e
      error (Relude.toText e)
    Right t -> do
      log D "Rendering compiled readme template"
      return $ render Nothing $ renderTemplate t (toJSON context)
