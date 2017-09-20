
module A2_Configuration
  (
      Configuration(..),
      readConf
  )
  where

import Dhall


--- Data ---
data Configuration = Configuration
  {
      path :: Text
  }
  deriving (Generic, Show)
instance Interpret Configuration


--- Functions ---

readConf :: IO Configuration
readConf = input auto "./config"


