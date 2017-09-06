module System.IO.SafeWrite
    ( withOutputFile
    , syncFile
    , allocateTempFile
    , finalizeTempFile
    ) where

import           System.FilePath (takeDirectory, takeBaseName)
import           System.Posix.IO (openFd, defaultFileFlags, closeFd, OpenMode(..))
import           System.Posix.Unistd (fileSynchronise)
import           Control.Exception (bracket, bracketOnError)
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
withOutputFile finalname act =
    bracketOnError
        (allocateTempFile finalname)
        (finalizeTempFile finalname False)
        (\tdata@(_, th) -> do
            r <- act th
            finalizeTempFile finalname True tdata
            return r)

allocateTempFile :: FilePath -> IO (FilePath, Handle)
allocateTempFile finalname = openTempFile (takeDirectory finalname) (takeBaseName finalname)

finalizeTempFile :: FilePath -> Bool -> (FilePath, Handle) -> IO ()
finalizeTempFile finalname ok (tname, th)
    | ok = do
        hClose th
        syncFile tname
        renameFile tname finalname
    | otherwise = do
        hClose th
        removeFile tname

