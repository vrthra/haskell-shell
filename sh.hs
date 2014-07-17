#!/usr/bin/env runhaskell
import System.IO
import qualified System.IO.Error as IOE

import Shell.Input
import Shell.Parse
import Shell.Run
import Shell.State

main = initializeState >>= shellLoop

shellLoop :: ShellState -> IO ()
shellLoop st = do
               input <- IOE.tryIOError promptInput
               case input of
                 Left e | IOE.isEOFError e -> putStrLn ""
                        | otherwise        -> ioError e
                 Right inStr -> do
                                newState <- pushHistory st inStr
                                runList newState (parseInput inStr)
                                shellLoop newState

