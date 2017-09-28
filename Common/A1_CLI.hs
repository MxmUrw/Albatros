
module A1_CLI
  (
      Args(..), aCommand, aOptions,
      Command(..),
      Options(..),
      ListOptions(..), listTarget,
      ListTarget(..),
      Time(..),
      Config(..),
      parse
  )
  where

import qualified B_Parser as PARS

import Control.Lens
import Text.Parsec hiding ((<|>), option, parse)
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Functor.Identity
import Data.Text

-- data Config = Full | Monthly
--   deriving (Show)


data Command = CmdList ListOptions | CmdView ViewOptions


data ListTarget = Tags | Charges

data ViewOptions = ViewOptions
data ListOptions = ListOptions
  {
      _listTarget :: ListTarget
  }
makeLenses ''ListOptions

data Time = TimeYearMonth PARS.YearMonth | TimeNow | TimeNone


data Options = Options
  {
      begin :: Time,
      end :: Time,
      accounts :: [Text]
  }

data Args = Args
  {
      _aCommand :: Command,
      _aOptions :: Options
  }
makeLenses ''Args


class Config c where
    getAccounts :: c -> [Text]


parse :: Config c => c -> IO Args
parse c = execParser (parseCLI c)

parseCLI :: Config c => c -> ParserInfo Args
parseCLI c = info ((parseArgs c) <**> helper)
                (fullDesc <> progDesc "View money" <> header "Albatros")

parseArgs :: Config c => c -> Parser Args
parseArgs c = Args
              <$> hsubparser
                  (
                      command "list" listInfo
                      <> command "view" viewInfo
                  )
              <*> parseOptions c
  where
    listInfo = info cmdListParser (progDesc "List statistics")
    viewInfo = info cmdViewParser (progDesc "View graphs")

cmdListParser :: Parser Command
cmdListParser = CmdList <$> listOptions

cmdViewParser :: Parser Command
cmdViewParser = CmdView <$> pure ViewOptions

---------------------------------------------------------
--- List options
listOptions :: Parser ListOptions
listOptions = ListOptions <$> (listTargetTags <|> listTargetCharges)

listTargetTags :: Parser ListTarget
listTargetTags = flag' Tags
                 ( long "tags"
                 <> help "list tags"
                 )

listTargetCharges :: Parser ListTarget
listTargetCharges = flag' Charges
                    ( long "charges"
                    <> help "list charges"
                    )


---------------------------------------------------------
--- General options
parseOptions :: Config c => c -> Parser Options
parseOptions c = Options
            <$> beginParser
            <*> endParser
            <*> accountsParser c


beginParser :: Parser Time
beginParser = option (readParsec timePsc)
              ( long "begin"
                <> short 'b'
                <> metavar "BEGIN"
                <> value TimeNone
                <> help "Start at time BEGIN"
              )

endParser :: Parser Time
endParser = option (readParsec timePsc)
            ( long "end"
              <> short 'e'
              <> metavar "END"
              <> value TimeNone
              <> help "End at time END"
            )

accountsParser :: Config c => c -> Parser [Text]
accountsParser c = option (readParsec (accountsPsc c))
                   ( long "accounts"
                     <> short 'a'
                     <> metavar "ACCOUNTS"
                     <> value (getAccounts c)
                     <> help "Charges are filtered by ACCOUNTS"
                   )


------------------------------------------
--- Single argument parsers

readParsec :: Parsec String () a -> ReadM a
readParsec f = eitherReader ((_Left%~show) . runIdentity . runParserT f () "")

accountsPsc :: Config c => c -> Parsec String u [Text]
accountsPsc c = fmap pack <$> PARS.commaSep account
  where account = choice $ try <$> PARS.symbol <$> unpack <$> getAccounts c

timePsc :: ParsecT String u Identity Time
timePsc =
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

