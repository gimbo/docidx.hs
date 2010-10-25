module Distribution.DocIdx.Common where

import qualified Control.Exception as C
import System.IO

-- | Application name, used to construct path to data directory, and
-- in footer of constructed page.
appName :: String
appName = "docidx"

-- | Data type for elements of the table of contents.
data TocItem = TocItem String String
             | TocSeparator
             | TocNewline
               deriving (Eq, Ord, Show)

-- | Try to read the contents of a file, but if it can't be opened,
-- print an informative warning to stderr and return Nothing.
tryReadFile :: FilePath -> IO (Maybe String)
tryReadFile p =
  C.catch
    (do d <- readFile p
        return $ Just d)
    (\e -> do let err = show (e :: C.IOException)
              hPutStrLn stderr ("Warning: Couldn't open " ++ p ++ ": " ++ err)
              return Nothing)
  