module Shell.Input (promptInput) where
import qualified System.IO as SIO
import qualified System.Posix.Env as Env
import qualified System.Posix.Signals as Signals

promptInput = do sighandle newShellPrompt
                 shellPrompt
                 input <- getInput
                 sighandle blankLine
                 return input

sighandle f = Signals.installHandler Signals.keyboardSignal (Signals.Catch f) Nothing


shellPrompt = do x <- Env.getEnvDefault "PS1" ""
                 putStr x
                 SIO.hFlush SIO.stdout

blankLine = putStrLn ""

newShellPrompt = blankLine >> shellPrompt

getInput :: IO String
getInput = do
           line <- getLine
           if not (null line) && last line == '\\'
           then do
                secondaryPrompt
                next <- getInput
                return (init line ++ next)
           else return line

secondaryPrompt = do x <- Env.getEnvDefault "PS2" ""
                     putStr x
                     SIO.hFlush SIO.stdout

