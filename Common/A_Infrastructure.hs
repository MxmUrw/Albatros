
module A_Infrastructure
  (
      run
  )
  where

import qualified A1_CLI as CLI
import qualified A2_Configuration as CONF
import qualified A3_Files as FILE
import qualified B_Parser as PARS
import qualified C_Renderer as REND

-- import Data.Functor.Identity
import Control.Monad
import Control.Lens
import Data.Text.Lazy (pack, unpack, Text)
import Data.Either
import Data.Ord
import Data.List
import Data.Time.LocalTime
import Data.Traversable

run :: IO ()
run =
  do cmd <- CLI.parse
     case cmd of
        CLI.Full -> genParse
        CLI.Monthly -> return ()

genParse :: IO ()
genParse =
  do
      CONF.Configuration {CONF.path = root} <- CONF.readConf
      pipeline (unpack root)

  where
      pipeline =
            FILE.findFnc1
            >=> mapM readFile
            >=> return . fmap PARS.readFnc
            >=> handleError putError render

      handleError f g x =
        do
            let (a,b) = partitionEithers x
            f a
            g b

      putError e =
        do
              putStrLn "------"
              putStrLn "Error:"
              print e

      render fncs =
        do
            let movs = PARS.genMovements =<< fncs
            let sortedMovs = sortBy (comparing PARS._date) movs
            REND.draw (prepareFull sortedMovs)

prepareFull :: [PARS.Movement] -> [(LocalTime,Int)]
prepareFull = snd . mapAccumL f 0
  where
    f acc m = (acc + amount, (date, acc + amount))
      where
        date = m^.PARS.date
        amount = m^.PARS.item.PARS.amount


-- dateToNum :: PARS.Date -> LocalTime
-- dateToNum (PARS.Date y m d) = y 




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





