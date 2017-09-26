
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
import Data.Default

---------------------------------------
------ Data ------
data Stats = Stats
  {
      _current :: Int,
      _corrected :: Int
  }
  deriving (Show)

instance Default Stats where
    def = Stats 0 0

makeLenses ''Stats


instance PARS.Config CONF.Accounts where
    getDefaultAccount = unpack . CONF._main
    getWorldAccount = unpack . CONF._world
    getAccounts c =
      PARS.getDefaultAccount c
      : PARS.getWorldAccount c
      : fmap unpack (CONF._others c)


---------------------------------------
------ Functions ------
run :: IO ()
run =
  do
      conf <- CONF.readConf
      cmd <- CLI.parse

      let r = case cmd of
                CLI.Full -> renderFull
                CLI.Monthly -> fail "not implemented"

      stats <- pipeline r conf

      print stats


  where
      pipeline renderer conf =
        do
            files <- FILE.findFnc1 (unpack $ conf^.CONF.path)
            conts <- mapM readContents files
            let fncs = PARS.readFnc (conf^.CONF.accounts) <$> conts
            handleError putError renderer fncs

      readContents path =
        do
            contents <- readFile path
            return (path,contents)

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

      -- render fncs =
      --   do
      --       let movs = PARS.genMovements =<< fncs
      --       let sortedMovs = sortBy (comparing PARS._date) movs
      --       REND.draw (prepareFull sortedMovs)

-- prepareFull :: [PARS.Movement] -> [(LocalTime,Int)]
-- prepareFull = snd . mapAccumL f 0
--   where
--     f acc m = (acc + amount, (date, acc + amount))
--       where

renderFull :: [PARS.Fnc1] -> IO Stats
renderFull fncs =
  do
      let movs = PARS.genMovements =<< fncs
      let sortedMovs = sortBy (comparing PARS._date) movs
      let (stats,vals) = mapAccumL delta def sortedMovs
      REND.draw vals
      return stats
  where
      delta stats mov
        | PARS.Absolute v <- value =
              (stats & current.~v & corrected+~(abs (stats^.current - v)),
               (date, v))
        | PARS.Relative v <- value =
              (stats & current+~v, (date, stats^.current + v))
        where
          date = mov^.PARS.date
          value = mov^.PARS.item.PARS.value







-- fullParse :: IO ()
-- fullParse =
--   do
--       CONF.Configuration {CONF._path = root} <- CONF.readConf

--       mapM_ printFnc =<< return . fmap (PARS.readFnc) =<< mapM readFile =<< FILE.findFnc1 (unpack root)

--   where printFnc (Left e) =
--           do
--               putStrLn "------"
--               putStrLn "Error:"
--               print e
--         printFnc (Right f) =
--           do
--               putStrLn "------"
--               mapM_ print $ PARS.genMovements f



-- testParse :: IO ()
-- testParse =
--   do case PARS.readFnc file of
--        Right f -> print $ PARS.genMovements f
--        Left err -> print err

--   where file = "month 2017 08\n day 19\n ex 1300 \"Laptop\"\"Personal\""





