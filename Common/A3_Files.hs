
module A3_Files
  (
      findFnc1
  )
  where

import System.FilePath.Find

findFnc1 :: FilePath -> IO [FilePath]
findFnc1 = find always ((== ".fnc1") <$> extension)
