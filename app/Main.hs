module Main where

import Saunf
import Options.Applicative

data CliOptions = Readme ReadmeOptions deriving (Show)

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
  val <- execParser cliParser
  case val of
    Readme Push -> pushReadmeFile
    _ -> putStrLn "Not implemented yet :-("
