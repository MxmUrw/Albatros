
module B_Parser
  (
      Movement(..),
      readFnc,
      genMovements
  )
  where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellStyle)

import Data.Functor.Identity

import qualified Data.Text as T

import Control.Monad


data Movement = Movement
  {
      date :: Date,
      item :: Item
  }

instance Show Movement where
    show (Movement d i) = show d ++ ": \t" ++ show (amount i) ++ " \t" ++ T.unpack (iLabel i) ++ " \t[" ++ T.unpack (tag i) ++ "]"

data Item = Item
  {
      amount :: Int,
      iLabel :: T.Text,
      tag :: T.Text
  }


data Date = Date Int Int Int

instance Show Date where
    show (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d

data Fnc1 = Fnc1 Int Int [(Int, [Item])]

genMovements :: Fnc1 -> [Movement]
genMovements (Fnc1 y m ds) = join $ foldl (\xs b -> travDay b : xs) [] ds
  where travDay (d, items) = Movement (Date y m d) <$> items


readFnc :: String -> Either ParseError Fnc1
readFnc = runIdentity . runParserT fnc1Parser () ""


-- Parser

fnc1Parser :: ParsecT String u Identity Fnc1
fnc1Parser = whiteSpace *> symbol "month" *> (Fnc1 <$> integer <*> integer <*> many dayParser)

dayParser :: ParsecT String u Identity (Int, [Item])
dayParser = symbol "day" *> ((,) <$> integer <*> many itemParser)

itemParser :: ParsecT String u Identity Item
itemParser = (symbol "ex" *> ex) <|> (symbol "in" *> inc)

  where ex = Item <$> (((-1) *) <$> monetaryValue) <*> stringLiteral <*> stringLiteral
        inc = Item <$> monetaryValue <*> stringLiteral <*> stringLiteral



-- Lexer

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
