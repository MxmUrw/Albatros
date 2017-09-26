
module A2_Configuration
  (
      Configuration(..), path, accounts,
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
      _others :: [Text]
  }
  deriving (Generic, Show)
instance Interpret Accounts
makeLenses ''Accounts

data Configuration = Configuration
  {
      _path :: Text,
      _accounts :: Accounts
  }
  deriving (Generic, Show)
instance Interpret Configuration
makeLenses ''Configuration



--- Functions ---

readConf :: IO Configuration
readConf = input auto "./config"


