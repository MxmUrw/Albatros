
module A1_CLI
  (
      Config(..),
      parse
  )
  where

import Options.Applicative
import Data.Semigroup ((<>))

data Config = Full | Monthly
  deriving (Show)


parse :: IO Config
parse = execParser parseCLI

parseCLI :: ParserInfo Config
parseCLI = info (parseCLI' <**> helper)
                (fullDesc <> progDesc "View money" <> header "Albatros")

parseCLI' :: Parser Config
parseCLI' = subparser
            (
                command "full" (info (pure Full) (progDesc "Full view"))
                <>
                command "monthly" (info (pure Monthly) (progDesc "Monthly resets"))
            )

