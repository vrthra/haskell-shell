module Shell.Builtins (builtins, runBuiltin) where
import Control.Exception
import qualified Data.Map as Map
import System.Exit
import System.Directory
import System.IO
import System.Posix.Env (setEnv)
import qualified System.Posix.Process as PP
import qualified System.Process as P (StdStream(..))
import Shell.Error
import Shell.State
import Shell.State.History (getHistory)
import qualified Shell.Grammar as G

type Builtin = ShellState -> Maybe Handle -> [String] -> IO ()

builtins :: [(String, Builtin)]
builtins = [ ("cd", changeDir)
           , ("pwd", printDir)
           , ("exit", exitShell)
           , ("exec", execCommand)
           , ("history", printHistory)
           , ("setenv", setEnvironment)
           ]

runBuiltin :: ShellState -> Maybe Handle -> Builtin -> [String] -> IO ()
runBuiltin st h b (name:args) = handle (shellException stdout [name]) $ b st h args

mhPutStrLn (Just h) s = hPutStrLn h s
mhPutStrLn Nothing  _ = return ()

changeDir st h []      = getHomeDirectory >>= changeDir st h . (:[])
changeDir _ _ (dir:_) = setCurrentDirectory dir

printDir _ h _ = getCurrentDirectory >>= mhPutStrLn h

exitShell _ h [] = exitSuccess

execCommand _ _ (cmd:args) = PP.executeFile cmd True args Nothing

printHistory st h _ = mapM_ printHistoryEntry (getHistory $ history st)
                      where printHistoryEntry (n, s) = mhPutStrLn h $ (spacePad 5 $ show n) ++ "  " ++ s
                            spacePad w s = replicate (w - length s) ' ' ++ s

setEnvironment st _ (k:v:_) = setEnv k v True
setEnvironment _  _ _       = return ()
