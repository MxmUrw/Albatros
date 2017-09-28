
module A_Infrastructure
  (
      run,
      renderFull,
      prettyMap
  )
  where

import qualified A1_CLI as CLI
import qualified A2_Configuration as CONF
import qualified A3_Files as FILE
import qualified B_Parser as PARS
import qualified C_Renderer as REND

-- import Data.Functor.Identity
import qualified Data.Map.Strict as Map

import Control.Monad
import Control.Lens
import Data.Text.Lazy (pack, unpack, Text)
import Data.Either
import Data.Ord
import Data.List
import Data.Time.LocalTime
import Data.Time.Format
import Data.Traversable
import Data.Default
import Data.Text.Prettyprint.Doc

---------------------------------------
------ Data ------
data Stats = Stats
  {
      _current :: Int,
      _corrected :: [(LocalTime,Int)]
  }
  deriving (Show)

instance Default Stats where
    def = Stats 0 []

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
      args <- CLI.parse

      let r = renderFull args

      stats <- pipeline r conf

      print $ prettyMap stats


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

renderFull :: CLI.Args -> [PARS.Fnc1] -> IO (Map.Map PARS.Account Stats)
renderFull args fncs =
  do

      --- time filter
      currentTime <- zonedTimeToLocalTime <$> getZonedTime

      let timeBeginFilter mov
            = case CLI.begin args of
                CLI.TimeNone         -> True
                CLI.TimeNow          -> mov^.PARS.date >= currentTime
                CLI.TimeYearMonth ym -> mov^.PARS.date >= PARS.mkDate ym 1

      let timeEndFilter mov
            = case CLI.end args of
                CLI.TimeNone         -> True
                CLI.TimeNow          -> mov^.PARS.date <= currentTime
                CLI.TimeYearMonth ym -> mov^.PARS.date <= PARS.mkDate ym 31


      --- actions

      let movs = filter (\m -> timeBeginFilter m && timeEndFilter m) <$> PARS.genMovements =<< fncs
      let chrgs = foldl (flip PARS.addCharges) Map.empty movs

      let sortedMovs = sortBy (comparing PARS._cDate) <$> chrgs
      let statVals = mapAccumL delta def <$> sortedMovs

      Map.traverseWithKey draw statVals

  where

      delta acc mov
        | PARS.Absolute v <- value =
              let acc' = acc & current.~v & corrected%~((date, v - acc^.current) :)
              in (acc', (date, v))
        | PARS.Relative v <- value =
              let acc' = acc & current+~v
                  val' = acc'^.current
              in (acc', (date, val'))
        where
          date = mov^.PARS.cDate
          value = mov^.PARS.cAmount

      draw acc (stats,vals) = REND.draw (PARS.name acc) vals >> return stats






prettyMap :: Map.Map PARS.Account Stats -> Doc ()
prettyMap map = vsep $ single <$> Map.toList map
  where
    single (acc,stats) = pretty (show acc) <> line <> indent 4 (prettyStats stats)

prettyStats :: Stats -> Doc ()
prettyStats stats = vsep
                    [
                        "balance:" <+> pretty (stats^.current),
                        "corrections:" <+> list (prettyCorrected <$> stats^.corrected)
                    ]
                    <> line

prettyCorrected :: (LocalTime,Int) -> Doc ()
prettyCorrected (date,val) = pretty (formatTime defaultTimeLocale "%F:" date) <+> pretty val



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





