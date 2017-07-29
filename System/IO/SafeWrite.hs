module System.IO.SafeWrite
    ( withOutputFile
    , syncFile
    ) where

import           System.FilePath (takeDirectory, takeBaseName)
import           System.Posix.IO (openFd, defaultFileFlags, closeFd, OpenMode(..))
import           System.Posix.Unistd (fileSynchronise)
import           Control.Exception (bracket, onException)
import           System.IO (Handle, hClose, openTempFile)
import           System.Directory (renameFile, removeFile)


-- | Sync a file to disk
--
-- Only supported on Posix (patches with Windows support are welcome)
syncFile :: FilePath -- ^ File to sync
            -> IO ()
syncFile fname = do
    bracket (openFd fname ReadWrite Nothing defaultFileFlags)
        closeFd
        fileSynchronise
    -- The code below will not work on Windows
    bracket (openFd (takeDirectory fname) ReadOnly Nothing defaultFileFlags)
        closeFd
        fileSynchronise

-- | Variation of 'withFile' for output files.
--
-- Output is written to a temporary file. Once the action has completed, this
-- file is then sync'ed to disk (see |syncFile|) and renamed to its final
-- destination. In Posix, this is an atomic operation. If an exception is
-- raised, then the temporary output file will be deleted and not saved to
-- disk. Thus, the result file will either contain the complete result or will
-- be empty.
withOutputFile ::
            FilePath -- ^ Final desired file path
            -> (Handle -> IO a) -- ^ action to execute
            -> IO a
withOutputFile finalname act = do
    (tname, th) <- openTempFile (takeDirectory finalname) (takeBaseName finalname)
    (do
        r <- act th
        hClose th
        syncFile tname
        renameFile tname finalname
        return r) `onException` (hClose th >> removeFile tname)

