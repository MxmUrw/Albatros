
module B_Parser
  (
      Movement(..),
      Item(..),
      Fnc1(..),
      Value(..),
      Account(..),
      item,
      date,
      value,
      readFnc,
      genMovements
  )
  where

---------------------------------------------------------------------
------ Imports ------
import Text.Parsec
import Text.Parsec.Language (haskellStyle)
import qualified Text.Parsec.Token as P

import Control.Lens
import Control.Monad
import Data.Functor.Identity
import qualified Data.Text as T
import Data.Time.LocalTime
import Data.Time.Calendar


---------------------------------------------------------------------
------ Data ------

class Config c where
    getDefaultAccount :: c -> String
    getAccounts :: c -> [String]

newtype Account = Account { name :: String}

data Value = Absolute Int | Relative Int
  deriving (Show)

data Item = Item
  {
      _value :: Value,
      _account :: Account,
      _iLabel :: T.Text,
      _tag :: T.Text
  }
makeLenses ''Item

data Movement = Movement
  {
      _date :: LocalTime,
      _item :: Item
  }
makeLenses ''Movement

instance Show Movement where
    show (Movement d i) = show d ++ ": \t"
                          ++ show (_value i) ++ " \t"
                          ++ T.unpack (_iLabel i) ++ " \t["
                          ++ T.unpack (_tag i) ++ "]"



-- with format (Month y m [day, items])
--          or [Recurrent item y1 m1 y2 m2]
data Fnc1 = FncMonth YearMonth [(Int, [Item])] | FncRec [Recurrent]

data Recurrent = Recurrent Item YearMonth YearMonth

data YearMonth = YearMonth Int Int
  deriving (Eq)

instance Ord YearMonth where
    (YearMonth y1 m1) <= (YearMonth y2 m2) = y1 < y2 || (y1 == y2 && m1 <= m2)

---------------------------------------------------------------------
------ Functions ------
-- top level

readFnc :: Config c => c -> String -> String -> Either ParseError Fnc1
readFnc c name file = runIdentity $ runParserT fnc1Parser c name file


genMovements :: Fnc1 -> [Movement]
genMovements (FncMonth ym ds) = join $ foldl (\xs b -> travDay b : xs) [] ds
  where
    travDay (d, items) = Movement (mkDate ym d) <$> items
genMovements (FncRec recs) = genRec =<< recs
  where
    genRec (Recurrent i ym1 ym2) = flip Movement i <$> enumMonthly ym1 ym2

enumMonthly :: YearMonth -> YearMonth -> [LocalTime]
enumMonthly ym1 ym2
  | ym1 < ym2 = mkDate ym1 1 : enumMonthly (nextYM ym1) ym2
  | otherwise = []

nextYM :: YearMonth -> YearMonth
nextYM (YearMonth y 12) = YearMonth (y+1) 1
nextYM (YearMonth y m) = YearMonth y (m+1)

mkDate (YearMonth y m) d =
  LocalTime (fromGregorian (fromIntegral y) m d) midnight





-- parser

fnc1Parser :: Config c => ParsecT String c Identity Fnc1
fnc1Parser =
  whiteSpace *> (month <|> recurrent)
  where
    month =
      symbol "month" *>
      (FncMonth <$> yearMonthParser <*> many dayTreeParser)
    recurrent=
      (FncRec <$> many recurrentParser)

yearMonthParser :: ParsecT String u Identity YearMonth
yearMonthParser = YearMonth <$> integer <* symbol "-" <*> integer

recurrentParser :: Config c => ParsecT String c Identity Recurrent
recurrentParser =
  symbol "recurrent" *> recurrent
  where
    recurrent = Recurrent
                <$> parens itemParser
                <* symbol "from"
                <*> yearMonthParser
                <* symbol "till"
                <*> yearMonthParser

dayTreeParser :: Config c => ParsecT String c Identity (Int, [Item])
dayTreeParser = symbol "day" *> ((,) <$> integer <*> many itemParser)

itemParser :: Config c => ParsecT String c Identity Item
itemParser =
  (symbol "ex" *> ex)
  <|> (symbol "in" *> inc)
  <|> (symbol "check" *> check)
  where ex = Item
          <$> (Relative <$> ((-1) *) <$> monetaryValue)
          <*> accountParser
          <*> stringLiteral
          <*> stringLiteral
        inc = Item
          <$> (Relative <$> monetaryValue)
          <*> accountParser
          <*> stringLiteral
          <*> stringLiteral
        check = Item
          <$> (Absolute <$> monetaryValue)
          <*> accountParser
          <*> pure "correction"
          <*> pure "correction"

accountParser :: Config c => ParsecT String c Identity Account
accountParser =
  do
      config <- getState
      let def      = getDefaultAccount config
      let explicit = angles <$> symbol <$> getAccounts config
      Account <$> def `option` choice explicit


-- lexer
lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser haskellStyle

integer :: ParsecT String u Identity Int
integer = fromIntegral <$> P.integer lexer

symbol :: String -> ParsecT String u Identity String
symbol = P.symbol lexer

parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = P.parens lexer

angles :: ParsecT String u Identity a -> ParsecT String u Identity a
angles = P.angles lexer

identifier :: ParsecT String u Identity String
identifier = P.identifier lexer

stringLiteral :: ParsecT String u Identity T.Text
stringLiteral = T.pack <$> P.stringLiteral lexer

monetaryValue :: ParsecT String u Identity Int
monetaryValue = combine <$> P.naturalOrFloat lexer
  where
    combine (Left i) = fromIntegral i
    combine (Right r) = round r

whiteSpace :: ParsecT String u Identity ()
whiteSpace = P.whiteSpace lexer
