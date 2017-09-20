
module B_Parser
  (
      Day,
      readFnc
  )
  where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellStyle)

import Data.Functor.Identity

data Day = Day Int Int Int
  deriving (Show)




readFnc :: String -> Identity (Either ParseError Day)
readFnc = runParserT fnc1Parser () ""


-- Parser

fnc1Parser :: ParsecT String u Identity Day
fnc1Parser = symbol "day"
            *> (Day <$> integer <*> integer <*> integer)

-- Lexer

lexer = P.makeTokenParser haskellStyle

integer :: ParsecT String u Identity Int
integer = fromIntegral <$> P.integer lexer

symbol = P.symbol lexer
