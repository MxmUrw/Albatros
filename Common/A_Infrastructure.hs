
module A_Infrastructure
  (
      run
  )
  where

import qualified A1_CLI as CLI
import qualified A2_Configuration as CONF
import qualified B_Parser as PARS

import Data.Functor.Identity

run :: IO ()
run =
  do cmd <- CLI.parse
     case cmd of
        CLI.Full -> fullParse
        CLI.Monthly -> return ()


fullParse :: IO ()
fullParse =
  do case PARS.readFnc file of
       Right f -> print $ PARS.genMovements f
       Left err -> print err

  where file = "month 2017 08\n day 19\n ex 1300 \"Laptop\"\"Personal\""





