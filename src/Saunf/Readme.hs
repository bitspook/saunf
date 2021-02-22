{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Saunf.Readme
  ( findDescription,
    getReadmeTemplate,
    soberReadmeTemplate,
    parseInjectedSectionName,
    pushReadmeFile,
  )
where

import Control.Monad.Reader
import Data.Aeson
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text.IO as T
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

-- | Get the readme template string from given config-$Section$
getReadmeTemplate :: Section -> Maybe Text
getReadmeTemplate (Section _ bs) = case readmeSection of
    [Section _ xs] -> firstCodeBlock xs
    _ -> Nothing
  where
    readmeSection = runReader (filterSections (isHeaderWithId "readme")) (SaunfEnv (Pandoc mempty bs) mempty)
    isCodeBlock x = case x of
      (CodeBlock _ _) -> True
      _ -> False
    firstCodeBlock xs = case find isCodeBlock xs of
      Just (CodeBlock _ t) -> Just t
      _ -> Nothing

-- | Parse the name of the section which should be injected out of special
-- syntax "$#section-name$".
parseInjectedSectionName :: Text -> Maybe Text
parseInjectedSectionName xs = either (const Nothing) (Just . pack) $ parse nameParser "" xs
  where
    nameParser = do
      _ <- Parsec.char '$'
      _ <- Parsec.char '#'
      name <- Parsec.many (Parsec.letter <|> Parsec.char '-' <|> Parsec.char '_')
      _ <- Parsec.char '$'
      return name

-- | Try to expand a Block to a Section if it is a valid injection spot
explodeMaybe :: Block -> Reader SaunfEnv [Block]
explodeMaybe a = do
  env <- ask
  return $ case a of
    (Header lvl _ xs) -> explodedHeader : explodedBlocks
      where
        grabSection :: Text -> Reader SaunfEnv Section
        grabSection id = do
          sections <- filterSections (isHeaderWithId id)
          case sections of
            [] -> return $ Section P.Null []
            (s : _) -> return s
        blks = case parseInjectedSectionName (stringify xs) of
          Nothing -> Section P.Null [a]
          Just name -> runReader (grabSection name) env
        (Section explodedHeader explodedBlocks) = setSectionHeaderLevel lvl blks
    _ -> [a]

-- | Evaluate all Saunf syntax in readme template to produce a valid Pandoc
-- template. It parses readme template as markdown, operate on it to remove all
-- special syntax, and return back a markdown string
soberReadmeTemplate :: (PandocMonad m) => ReaderT SaunfEnv m Text
soberReadmeTemplate = do
  tStr' <- asks (readmeTemplate . saunfConf)
  case tStr' of
    -- FIXME: Return "Either ConfError Text" instead of throwing a hissy-fit
    Nothing -> error "Template not found"
    (Just tStr) -> do
      (Pandoc tMeta tBlocks) <- readMarkdown def tStr
      env <- ask
      let expandedBlocks :: [Block] = foldl (<>) [] (map ($ env) (runReader . explodeMaybe <$> tBlocks))
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

-- Create a readme doc, and push it to readme.md
pushReadmeFile :: FilePath -> ReaderT SaunfEnv IO ()
pushReadmeFile dest = do
  env <- ask
  pmpDoc@(Pandoc meta _) <- asks saunfDoc

  soberTemplate' <- liftIO $ P.runIO $ runReaderT soberReadmeTemplate env
  soberTemplate <- liftIO $ P.handleError soberTemplate'

  let title = lookupMetaString "title" meta
  description' <- liftIO $ P.runIO $ writeMarkdown def $ Pandoc meta $ fromMaybe mempty (findDescription pmpDoc)
  description <- liftIO $ P.handleError description'

  let context = ReadmeContext title description

  tc <- liftIO $ compileTemplate "readme.md" soberTemplate
  case tc of
    Left e -> error e
    Right t -> liftIO $ T.writeFile dest $ render Nothing $ renderTemplate t (toJSON context)
