{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Reader
import qualified Data.Text.IO as T
import Options.Applicative
import Saunf
import Saunf.Types
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
  saunfDoc <- P.handleError =<< P.runIO (readOrg def pmpText)
  val <- execParser cliParser

  case runReader getConfig (SaunfEnv saunfDoc mempty) of
    Left ConfNotFound -> error "Configuration not found"
    Left ConflictingConf -> error "Could not decide which conf to use. Please check if there are more than one conf sections present"
    Right config -> do
      let env = SaunfEnv saunfDoc config
      case val of
        Readme Push -> runReaderT (pushReadmeFile readmeDest) env
        _ -> putStrLn "Not implemented yet :-("
