# SafeIO: Haskell library for safe (atomic) IO

This is a simple module, which enables writing in atomic mode. It
implements the following 4 step procedure:

1. Open a temporary file in the same directory as the final output.
2. Write to this temporary file.
3. Close and sync the file.
4. Atomically rename the file to its final destination.

## Example

    import System.IO.SafeWrite
    ...
    main = do
        withOutputFile "output.txt" $ \hout -> do
            hPutStrLn hout "Hello World"


## Author

Luis Pedro Coelho [Email](mailto:luis@luispedro.org)
