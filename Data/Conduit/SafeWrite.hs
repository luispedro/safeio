module Data.Conduit.SafeWrite
    ( safeSinkFile
    ) where

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import qualified Data.ByteString as B
import           Data.IORef (newIORef, writeIORef, readIORef)
import           System.FilePath (takeDirectory)
import           System.IO (hClose, openTempFile)
import           System.Directory (renameFile, removeFile)
import           Control.Monad (unless)
import           Control.Monad.Trans.Resource
import           Control.Monad.IO.Class (liftIO)

import           System.IO.SafeWrite (syncFile)

-- | Write to file |finalname| using a temporary file and atomic move.
--
-- The file is only written if the sink runs to completion without errors. Any
-- form of early termination will cause the output to be removed.
safeSinkFile :: (MonadResource m) =>
                    FilePath -- ^ Final filename
                    -> C.Sink B.ByteString m ()
safeSinkFile finalname = C.bracketP
                            acquire 
                            deleteTempOnError
                            writeMove
    where
        acquire = do
            (tname, th) <- openTempFile (takeDirectory finalname) finalname
            completed <- newIORef False
            return (tname, th, completed)
        writeMove (tname, th, completed) = do
            CC.sinkHandle th
            liftIO $ do
                hClose th
                syncFile tname
                renameFile tname finalname
                writeIORef completed True
        deleteTempOnError (tname, th, completed) = do
            completed' <- readIORef completed
            unless completed' $ do
                hClose th
                removeFile tname

