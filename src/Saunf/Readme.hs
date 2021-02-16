{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Saunf.Readme
  ( findDescription,
    getReadmeTemplate,
    soberReadmeTemplate,
    parseInjectedSectionName,
    setSectionHeaderLevel,
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
getReadmeTemplate (Section bs) = case readmeSection of
    [Section xs] -> firstCodeBlock xs
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

-- | Adjust the level of a section to given level. This will make the first
-- Header in section of the given level, and every sub-Header's level
-- will be shifted accordingly
setSectionHeaderLevel :: Int -> Section -> Section
setSectionHeaderLevel n (Section xs) = case xs of
  ((Header sectionLvl attr inner) : bs) -> Section (Header n attr inner : (shiftHeader <$> bs))
    where
      delta = n - sectionLvl
      shiftHeader b = case b of
        Header lvl a i -> Header (lvl + delta) a i
        _ -> b
  _ -> Section xs

-- | Try to expand a Block to a Section if it is a valid injection spot
expandToSection :: Block -> Reader SaunfEnv Section
expandToSection a = do
  env <- ask
  return $ case a of
    (Header lvl _ xs) -> setSectionHeaderLevel lvl blks
      where
        grabSection :: Text -> Reader SaunfEnv Section
        grabSection id = do
          sections <- filterSections (isHeaderWithId id)
          case sections of
            [] -> return $ Section []
            (s : _) -> return s
        blks = case parseInjectedSectionName (stringify xs) of
          Nothing -> Section [a]
          Just name -> runReader (grabSection name) env
    _ -> Section [a]

-- | Evaluate all Saunf syntax in readme template to produce a valid Pandoc
-- template. It parses readme template as markdown, operate on it to remove all
-- special syntax, and return back a markdown string
soberReadmeTemplate :: (PandocMonad m) => ReaderT SaunfEnv m Text
soberReadmeTemplate = do
  tStr' <- asks (readmeTemplate . saunfConf)
  case tStr' of
    Nothing -> error "Template not found"
    (Just tStr) -> do
      (Pandoc tMeta tBlocks) <- readMarkdown def tStr
      env <- ask
      let expandedBlocks :: [Block] = foldl (\x (Section xs) -> x <> xs) [] (map ($ env) (runReader . expandToSection <$> tBlocks))
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
