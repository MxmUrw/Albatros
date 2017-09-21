
module A_Infrastructure
  (
      run
  )
  where

import qualified A1_CLI as CLI
import qualified A2_Configuration as CONF
import qualified A3_Files as FILE
import qualified B_Parser as PARS

-- import Data.Functor.Identity
import Data.Text.Lazy

run :: IO ()
run =
  do cmd <- CLI.parse
     case cmd of
        CLI.Full -> fullParse
        CLI.Monthly -> return ()


fullParse :: IO ()
fullParse =
  do
      CONF.Configuration {CONF.path = root} <- CONF.readConf

      mapM_ printFnc =<< return . fmap (PARS.readFnc) =<< mapM readFile =<< FILE.findFnc1 (unpack root)

  where printFnc (Left e) =
          do
              putStrLn "------"
              putStrLn "Error:"
              print e
        printFnc (Right f) =
          do
              putStrLn "------"
              mapM_ print $ PARS.genMovements f



testParse :: IO ()
testParse =
  do case PARS.readFnc file of
       Right f -> print $ PARS.genMovements f
       Left err -> print err

  where file = "month 2017 08\n day 19\n ex 1300 \"Laptop\"\"Personal\""





