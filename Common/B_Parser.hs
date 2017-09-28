
module B_Parser
  (
      item,
      date,
      value,
      readFnc,
      genMovements,
      addCharges,
      symbol,
      Movement(..),
      Item(..),
      Fnc1(..),
      Value(..),
      Account(..),
      Config(..),
      YearMonth(..), mkDate,
      yearMonthParser, commaSep,
      Charge(..), cAccount, cDate, cAmount, cTag, cLabel
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
-- import Data.Percentage
import qualified Data.Map.Strict as Map


---------------------------------------------------------------------
------ Data ------

class Config c where
    getDefaultAccount :: c -> String
    getWorldAccount :: c -> String
    getAccounts :: c -> [String]

newtype Account = Account { name :: T.Text}
  deriving (Eq, Ord)
instance Show Account where
    show (Account s) = "<" ++ T.unpack s ++ ">"

data Value = Absolute Int | Relative Int
  deriving (Show)
makePrisms ''Value

data Item = Item
  {
      _value :: Value,
      _source :: Account,
      _target :: Account,
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

data Charge = Charge
  {
      _cDate :: LocalTime,
      _cAmount :: Value,
      _cAccount :: Account,
      _cLabel :: T.Text,
      _cTag :: T.Text
  }
makeLenses ''Charge



type Percentage = Int
-- with format (Month y m [day, items])
--          or [Recurrent item y1 m1 y2 m2]
data Fnc1 = FncMonth YearMonth [(Int, [ExItem])] | FncRec [Recurrent]

data ExItem = ExItem Item (Maybe Split)
data Split = Split Percentage Direction Account

data Direction = From | To

data Recurrent = Recurrent ExItem YearMonth YearMonth

data YearMonth = YearMonth Int Int
  deriving (Eq)

instance Ord YearMonth where
    (YearMonth y1 m1) <= (YearMonth y2 m2) = y1 < y2 || (y1 == y2 && m1 <= m2)

---------------------------------------------------------------------
------ Functions ------
-- top level

readFnc :: Config c => c -> (String, String) -> Either ParseError Fnc1
readFnc c (name,file) = runIdentity $ runParserT fnc1Parser c name file


genMovements :: Fnc1 -> [Movement]
genMovements (FncMonth ym days) = travDay =<< days
  where
    travDay (d, items) = Movement (mkDate ym d) <$> (breakExItem =<< items)
-- genMovements (FncMonth ym days) = join $ foldl (\xs b -> travDay b : xs) [] ds
--   where
--     travDay (d, items) = Movement (mkDate ym d) <$> items
genMovements (FncRec recs) = genRec =<< recs
  where
    genRec (Recurrent i ym1 ym2) = Movement <$> enumMonthly ym1 ym2 <*> breakExItem i


addCharges :: Movement -> Map.Map Account [Charge] -> Map.Map Account [Charge]
addCharges
  (Movement date (Item value source target label tag))
  =
  case value of
    Relative _ -> Map.insertWith (++) target [mkCharge value target]
                 . Map.insertWith (++) source [mkCharge (negate value) source]
    Absolute _ -> Map.insertWith (++) target [mkCharge value target]
  where
    mkCharge v a = Charge date v a label tag
    negate v = v & _Relative *~ (-1)





breakExItem :: ExItem -> [Item]
breakExItem (ExItem item Nothing) = pure item
breakExItem (ExItem item (Just (Split p dir acc))) =
  case item^.value of
    Relative _ ->
      [
          item & value._Relative %~ reverseFrac p,
          item & value._Relative %~ applyFrac p & splitAcc dir .~ acc
      ]
    Absolute _ ->
      [
          item
      ]
  where
    reverseFrac :: Percentage -> Int -> Int
    reverseFrac p v = v - applyFrac p v

    applyFrac :: Percentage -> Int -> Int
    applyFrac p v = v * p `div` 100

    splitAcc From = source
    splitAcc To = target


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
                <$> parens exItemParser
                <* symbol "from"
                <*> yearMonthParser
                <* symbol "till"
                <*> yearMonthParser

dayTreeParser :: Config c => ParsecT String c Identity (Int, [ExItem])
dayTreeParser = symbol "day" *> ((,) <$> integer <*> many exItemParser)

exItemParser :: Config c => ParsecT String c Identity ExItem
exItemParser =
  do
      item <- itemParser
      option (ExItem item Nothing) (ExItem item . Just <$> brackets split)
  where
      split = symbol "split" *> (Split <$> percentage <*> direction <*> accountParser)
      direction = (symbol "from" *> pure From) <|> (symbol "to" *> pure To)


itemParser :: Config c => ParsecT String c Identity Item
itemParser =
  (symbol "ex" *> ex)
  <|> (symbol "in" *> inc)
  <|> (symbol "check" *> check)
  <|> (symbol "move" *> move)
  where ex = Item
          <$> (Relative <$> monetaryValue)
          <*> accountParser
          <*> (Account . T.pack . getWorldAccount <$> getState)
          <*> stringLiteral
          <*> stringLiteral
        inc = Item
          <$> (Relative <$> monetaryValue)
          <*> (Account . T.pack . getWorldAccount <$> getState)
          <*> accountParser
          <*> stringLiteral
          <*> stringLiteral
        check = Item
          <$> (Absolute <$> monetaryValue)
          <*> (Account . T.pack . getWorldAccount <$> getState)
          <*> accountParser
          <*> pure "correction"
          <*> pure "correction"
        move = Item
          <$> (Relative <$> monetaryValue)
          <*> accountParser
          <* symbol "to"
          <*> accountParser
          <*> pure "move"
          <*> pure "move"


accountParser :: Config c => ParsecT String c Identity Account
accountParser =
  do
      config <- getState
      let def      = getDefaultAccount config
      let explicit = try <$> angles <$> symbol <$> getAccounts config
      Account <$> T.pack <$> def `option` choice explicit


-- lexer
lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser haskellStyle

integer :: ParsecT String u Identity Int
integer = fromIntegral <$> P.integer lexer

percentage :: ParsecT String u Identity Percentage
percentage = fromIntegral <$> try (integer <* symbol "%")

symbol :: String -> ParsecT String u Identity String
symbol = P.symbol lexer

parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = P.parens lexer

brackets :: ParsecT String u Identity a -> ParsecT String u Identity a
brackets = P.brackets lexer

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

commaSep :: Parsec String u a -> Parsec String u [a]
commaSep = P.commaSep lexer

whiteSpace :: ParsecT String u Identity ()
whiteSpace = P.whiteSpace lexer
