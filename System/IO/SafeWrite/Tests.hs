{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where

import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit

import Control.Exception (throwIO)
import System.IO.Error (isDoesNotExistError, catchIOError)
import System.Directory (doesFileExist, removeFile)
import System.IO (hPutStrLn)

import System.IO.SafeWrite

main :: IO ()
main = do
    removeFileIfExists outname
    $(defaultMainGenerator)

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fp = removeFile fp `catchIOError` ignoreDoesNotExistError
    where
        ignoreDoesNotExistError e
                | isDoesNotExistError e = return ()
                | otherwise = throwIO e

outname :: FilePath
outname = "testing-output.txt"

case_create_output = do
    withOutputFile outname $ flip hPutStrLn "Hello World"
    (doesFileExist outname) >>= assertBool "Output file was not created"
    removeFile outname

case_not_create_on_exception = do
    (withOutputFile outname $ \h -> do
        hPutStrLn h "Hello World"
        throwIO $ userError "Something bad happened") `catchIOError` \_ -> return ()
    (not <$> doesFileExist outname) >>= assertBool "Output file was created despite exception being raised"

