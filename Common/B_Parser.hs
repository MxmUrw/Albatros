
module B_Parser
  (
      Movement(..),
      Item(..),
      Fnc1(..),
      Value(..),
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

data Value = Absolute Int | Relative Int
  deriving (Show)

data Item = Item
  {
      _value :: Value,
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




readFnc :: String -> Either ParseError Fnc1
readFnc = runIdentity . runParserT fnc1Parser () ""


-- parser

fnc1Parser :: ParsecT String u Identity Fnc1
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

recurrentParser :: ParsecT String u Identity Recurrent
recurrentParser =
  symbol "recurrent" *> recurrent
  where
    recurrent = Recurrent
                <$> parens itemParser
                <* symbol "from"
                <*> yearMonthParser
                <* symbol "till"
                <*> yearMonthParser

dayTreeParser :: ParsecT String u Identity (Int, [Item])
dayTreeParser = symbol "day" *> ((,) <$> integer <*> many itemParser)

itemParser :: ParsecT String u Identity Item
itemParser =
  (symbol "ex" *> ex)
  <|> (symbol "in" *> inc)
  <|> (symbol "check" *> check)
  where ex = Item <$> (Relative <$> ((-1) *) <$> monetaryValue) <*> stringLiteral <*> stringLiteral
        inc = Item <$> (Relative <$> monetaryValue) <*> stringLiteral <*> stringLiteral
        check = Item <$> Absolute <$> monetaryValue <*> pure "correction" <*> pure "correction"



-- lexer
lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser haskellStyle

integer :: ParsecT String u Identity Int
integer = fromIntegral <$> P.integer lexer

symbol :: String -> ParsecT String u Identity String
symbol = P.symbol lexer

parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = P.parens lexer

stringLiteral :: ParsecT String u Identity T.Text
stringLiteral = T.pack <$> P.stringLiteral lexer

monetaryValue :: ParsecT String u Identity Int
monetaryValue = combine <$> P.naturalOrFloat lexer
  where
    combine (Left i) = fromIntegral i
    combine (Right r) = round r

whiteSpace :: ParsecT String u Identity ()
whiteSpace = P.whiteSpace lexer
