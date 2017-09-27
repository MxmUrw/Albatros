
import A_Infrastructure

import qualified A1_CLI as CLI
import qualified A2_Configuration as CONF
import qualified A3_Files as FILE
import qualified B_Parser as PARS
import qualified C_Renderer as REND

import qualified Data.Map.Strict as Map


main :: IO ()
main = test1



test1 :: IO ()
test1 =
  do
      let file = "month 2017-09 day 27 in 100.0 \"Test\" \"Tag\" [ split 30% to <TEST_SIDE> ]"
      let accounts = CONF.Accounts "TEST_MAIN" "TEST_WORLD" ["TEST_SIDE"]
      let (Right fnc) = PARS.readFnc accounts ("testfile.txt",file)

      res <- renderFull [fnc]
      print (prettyMap res)
