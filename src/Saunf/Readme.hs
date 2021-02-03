{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Saunf.Readme
  ( findDescription,
    getReadmeTemplate,
    soberizeReadmeTemplate,
    parseInjectedSectionName,
    setSectionHeaderLevel,
    pushReadmeFile,
  )
where

import Data.Aeson
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text.IO as T
import Saunf.Config
import Saunf.Shared
import Text.DocLayout (render)
import Text.Pandoc as P
import Text.Pandoc.Shared
import Text.Pandoc.Writers.Shared
import Text.Parsec as Parsec

findDescription :: Pandoc -> Maybe [Block]
findDescription (Pandoc _ bs) = case takeWhile (not . isHeaderBlock) bs of
  [] -> Nothing
  xs -> Just xs

-- | Get the readme template string from the config if it is present
getReadmeTemplate :: SaunfConfig -> Maybe Text
getReadmeTemplate (SaunfConfig bs) = case findSection "readme" bs of
  Just xs -> firstCodeBlock xs
  _ -> Nothing
  where
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
setSectionHeaderLevel :: Int -> [Block] -> [Block]
setSectionHeaderLevel n xs = case xs of
  ((Header sectionLvl attr inner) : bs) -> Header n attr inner : (shiftHeader <$> bs)
    where
      delta = n - sectionLvl
      shiftHeader b = case b of
        Header lvl a i -> Header (lvl + delta) a i
        _ -> b
  _ -> xs

-- | Evaluate all Saunf syntax in readme template to produce a valid Pandoc
-- template. It parses readme template as markdown, operate on it to remove all
-- special syntax, and return back a markdown string
soberizeReadmeTemplate :: (PandocMonad m) => Text -> [Block] -> m Text
soberizeReadmeTemplate tStr pmpBlocks = do
  (Pandoc tMeta tBlocks) <- readMarkdown def tStr
  writeMarkdown def (Pandoc tMeta (soberize pmpBlocks `concatMap` tBlocks))
  where
    soberize :: [Block] -> Block -> [Block]
    soberize bs a = case a of
      (Header lvl _ xs) -> setSectionHeaderLevel lvl blks
        where
          blks = case parseInjectedSectionName (stringify xs) of
            Nothing -> [a]
            Just name -> fromMaybe [] (findSection name bs)
      _ -> [a]

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
pushReadmeFile :: SaunfConfig -> Pandoc -> FilePath -> IO ()
pushReadmeFile conf pmpDoc@(Pandoc meta pmpBs) dest = do
  let template = fromMaybe (error "Couldn't find readme template") (getReadmeTemplate conf)
  soberTemplate' <- P.runIO $ soberizeReadmeTemplate template pmpBs
  soberTemplate <- P.handleError soberTemplate'

  let title = lookupMetaString "title" meta
  description' <- P.runIO $ writeMarkdown def $ Pandoc meta $ fromMaybe mempty $ findDescription pmpDoc
  description <- P.handleError description'

  let context = ReadmeContext title description

  tc <- compileTemplate "readme.md" soberTemplate
  case tc of
    Left e -> error e
    Right t -> T.writeFile dest $ render Nothing $ renderTemplate t (toJSON context)
