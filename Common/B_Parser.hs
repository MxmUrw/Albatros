
module B_Parser
  (
      Movement(..),
      Item(..),
      Movement(..),
      item,
      date,
      amount,
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

-- data Date = Date Int Int Int
--   deriving (Eq)

-- instance Show Date where
--     show (Date y m d) = show y ++ "-"
--                         ++ show m ++ "-"
--                         ++ show d

-- instance Ord Date where
--     (Date y1 m1 d1) <= (Date y2 m2 d2) = years
--       where
--         years  = y1 < y2 || (y1 == y2 && months)
--         months = m1 < m2 || (m1 == m2 && days)
--         days   = d1 <= d2


data Item = Item
  {
      _amount :: Int,
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
                          ++ show (_amount i) ++ " \t"
                          ++ T.unpack (_iLabel i) ++ " \t["
                          ++ T.unpack (_tag i) ++ "]"







data Fnc1 = Fnc1 Int Int [(Int, [Item])]

---------------------------------------------------------------------
------ Functions ------
-- top level
genMovements :: Fnc1 -> [Movement]
genMovements (Fnc1 y m ds) = join $ foldl (\xs b -> travDay b : xs) [] ds
  where travDay (d, items) = Movement (mkDate y m d) <$> items

mkDate yyyy mm dd =
  LocalTime (fromGregorian (fromIntegral yyyy) mm dd) midnight


readFnc :: String -> Either ParseError Fnc1
readFnc = runIdentity . runParserT fnc1Parser () ""


-- parser
fnc1Parser :: ParsecT String u Identity Fnc1
fnc1Parser = whiteSpace *> symbol "month" *> (Fnc1 <$> integer <*> integer <*> many dayParser)

dayParser :: ParsecT String u Identity (Int, [Item])
dayParser = symbol "day" *> ((,) <$> integer <*> many itemParser)

itemParser :: ParsecT String u Identity Item
itemParser = (symbol "ex" *> ex) <|> (symbol "in" *> inc)

  where ex = Item <$> (((-1) *) <$> monetaryValue) <*> stringLiteral <*> stringLiteral
        inc = Item <$> monetaryValue <*> stringLiteral <*> stringLiteral



-- lexer
lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser haskellStyle

integer :: ParsecT String u Identity Int
integer = fromIntegral <$> P.integer lexer

symbol :: String -> ParsecT String u Identity String
symbol = P.symbol lexer

stringLiteral :: ParsecT String u Identity T.Text
stringLiteral = T.pack <$> P.stringLiteral lexer

monetaryValue :: ParsecT String u Identity Int
monetaryValue = combine <$> P.naturalOrFloat lexer
  where
    combine (Left i) = fromIntegral i
    combine (Right r) = round r

whiteSpace :: ParsecT String u Identity ()
whiteSpace = P.whiteSpace lexer
