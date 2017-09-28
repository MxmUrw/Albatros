
module A2_Configuration
  (
      Config(..), path, accounts,
      Accounts(..), main, others,
      readConf
  )
  where

import Control.Lens

import Dhall


--- Data ---
data Accounts = Accounts
  {
      _main :: Text,
      _world :: Text,
      _others :: [Text]
  }
  deriving (Generic, Show)
instance Interpret Accounts
makeLenses ''Accounts

data Config = Config
  {
      _path :: Text,
      _accounts :: Accounts
  }
  deriving (Generic, Show)
instance Interpret Config
makeLenses ''Config



--- Functions ---

readConf :: IO Config
readConf = input auto "./config"


