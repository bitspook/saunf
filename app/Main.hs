{-# LANGUAGE OverloadedStrings #-}
module Main where

import Saunf
import Saunf.Shared
import Saunf.Types
import Control.Monad.Reader
import Options.Applicative
import qualified Data.Text.IO as T
import Text.Pandoc as P

newtype CliOptions = Readme ReadmeOptions deriving (Show)

data ReadmeOptions
  = Push -- Push changes to readme
  | Pull -- Pull changes back from readme
  deriving (Show)

readmeOptions :: Parser ReadmeOptions
readmeOptions =
  subparser
    ( command "push" (info (pure Push) (progDesc "Push changes to readme.md"))
        <> command "pull" (info (pure Pull) (progDesc "Pull changes made to readme.md back"))
    )

cliOptions :: Parser CliOptions
cliOptions =
  subparser $ command "readme" (info (Readme <$> readmeOptions <**> helper) (progDesc "Manage the readme file"))

cliParser :: ParserInfo CliOptions
cliParser = info (cliOptions <**> helper) (fullDesc <> header "Tasty project management" <> progDesc "Manage software projects with plain-text")

main :: IO ()
main = do
  let readmeDest = "./readme.md"
  pmpText <- T.readFile "./saunf/saunf.org"
  saunfDoc' <- P.runIO $ readOrg def pmpText
  saunfDoc <- P.handleError saunfDoc'
  val <- execParser cliParser

  let config' = getConfig saunfDoc
  case config' of
    Nothing -> error "Could not find Saunf configuration in saunf/saunf.org"
    Just config ->
      let
        env = SaunfEnv saunfDoc config
      in
        case val of
          Readme Push -> runReaderT (pushReadmeFile readmeDest) env
          _ -> putStrLn "Not implemented yet :-("
