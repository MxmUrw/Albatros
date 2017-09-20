
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
  do let day = runIdentity $ PARS.readFnc "day 2017 09 20"
     print day
     return ()





