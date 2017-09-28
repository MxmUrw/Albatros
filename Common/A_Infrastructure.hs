
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
import qualified Data.Set as Set

import Control.Monad
import Control.Lens
-- import Data.Text.Lazy (pack, unpack, Text)
import Data.Text (pack, unpack, Text)
import Data.Text.Lazy (toStrict)
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
    getDefaultAccount = unpack . toStrict . CONF._main
    getWorldAccount = unpack . toStrict . CONF._world
    getAccounts c =
      PARS.getDefaultAccount c
      : PARS.getWorldAccount c
      : fmap unpack (toStrict <$> CONF._others c)

instance CLI.Config CONF.Accounts where
    getAccounts c =
      toStrict (CONF._main c)
      : toStrict (CONF._world c)
      : (toStrict <$> CONF._others c)


---------------------------------------
------ Functions ------

run :: IO ()
run =
  do
      conf <- CONF.readConf
      args <- CLI.parse (conf^.CONF.accounts)
      chrgs <- prepareCharges (args^.CLI.aOptions) conf

      case args^.CLI.aCommand of
        CLI.CmdView viewOpts -> do
            stats <- renderFull chrgs
            print $ prettyMap stats
        CLI.CmdList opts -> do
            let printTarget =
                  case opts^.CLI.listTarget of
                    CLI.Tags    -> prettyMap (sort <$> getTags <$> chrgs)
                    CLI.Charges -> prettyMap chrgs
            print printTarget


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


handleError f x =
    do
        let (a,b) = partitionEithers x
        _ <- f a
        return b

putError e =
    do
        putStrLn "------"
        putStrLn "Error:"
        print e


readContents path =
    do
        contents <- readFile path
        return (path,contents)


prepareCharges :: CLI.Options -> CONF.Config -> IO (Map.Map PARS.Account [PARS.Charge])
prepareCharges args conf =
  do
      --- read fnc
      files <- FILE.findFnc1 (unpack $ toStrict $ conf^.CONF.path)
      conts <- mapM readContents files
      let efncs = PARS.readFnc (conf^.CONF.accounts) <$> conts
      fncs <- handleError putError efncs


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

      --- account filter
      let accountFilter k _ = k `elem` (PARS.Account <$> CLI.accounts args)

      --- actions
      let movs = filter (\m -> timeBeginFilter m && timeEndFilter m) <$> PARS.genMovements =<< fncs
      let chrgs = foldl (flip PARS.addCharges) Map.empty movs
      let filteredChrgs = Map.filterWithKey accountFilter chrgs
      let sortedChrgs = sortBy (comparing PARS._cDate) <$> filteredChrgs

      return sortedChrgs


getTags :: [PARS.Charge] -> [Text]
getTags chrgs = nub $ PARS._cTag <$> chrgs


renderFull :: Map.Map PARS.Account [PARS.Charge] -> IO (Map.Map PARS.Account Stats)
renderFull chrgs =
  do
      let statVals = mapAccumL delta def <$> chrgs
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






prettyMap :: Pretty a => Map.Map PARS.Account a -> Doc ()
prettyMap map = vsep $ single <$> Map.toList map
  where
    single (acc,a) = pretty (show acc) <> line <> indent 4 (pretty a)


instance Pretty Stats where
    pretty stats = vsep
                   [
                       "balance:" <+> pretty (stats^.current),
                       "corrections:" <+> list (prettyCorrected <$> stats^.corrected)
                   ]
                   <> line

prettyCorrected :: (LocalTime,Int) -> Doc ann
prettyCorrected (date,val) = pretty date <> ":" <+> pretty val

instance Pretty LocalTime where
    pretty time = pretty $ formatTime defaultTimeLocale "%F" time

instance Pretty PARS.Charge where
    pretty (PARS.Charge date amount _ label tag) =
        pretty amount
        <+> "\t[" <> pretty date <> "]"
        <+> pretty label
        <+> "(" <> pretty tag <> ")"

instance Pretty PARS.Value where
    pretty (PARS.Relative r) = pretty r
    pretty (PARS.Absolute a) = ":=" <> pretty a


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





