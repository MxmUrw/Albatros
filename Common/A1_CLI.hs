
module A1_CLI
  (
      Args(..),
      Time(..),
      parse
  )
  where

import qualified B_Parser as PARS

import Control.Lens
import Text.Parsec hiding ((<|>), option, parse)
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Functor.Identity

-- data Config = Full | Monthly
--   deriving (Show)

data Time = TimeYearMonth PARS.YearMonth | TimeNow | TimeNone

data Args = Args
  {
      begin :: Time,
      end :: Time
  }


parse :: IO Args
parse = execParser parseCLI

parseCLI :: ParserInfo Args
parseCLI = info (parseConfig <**> helper)
                (fullDesc <> progDesc "View money" <> header "Albatros")

parseConfig :: Parser Args
parseConfig = Args <$> beginParser <*> endParser

beginParser :: Parser Time
beginParser = option readTime
              ( long "begin"
                <> short 'b'
                <> metavar "BEGIN"
                <> value TimeNone
                <> help "Start at time BEGIN"
              )

endParser :: Parser Time
endParser = option readTime
            ( long "end"
              <> short 'e'
              <> metavar "END"
              <> value TimeNone
              <> help "End at time END"
            )



readTime :: ReadM Time
readTime = eitherReader ((_Left%~show) . runIdentity . runParserT timeParser () "")

timeParser :: ParsecT String u Identity Time
timeParser =
    (TimeYearMonth <$> PARS.yearMonthParser)
    <|> (PARS.symbol "now" *> pure TimeNow)
    <|> (PARS.symbol "none" *> pure TimeNone)

-- parseCLI' :: Parser Config
-- parseCLI' = subparser
--             (
--                 command "view" (info (pure Full) (progDesc "Full view"))
--                 <>
--                 command "monthly" (info (pure Monthly) (progDesc "Monthly resets"))
--             )

