module Shell.Grammar where
import System.Posix.Types (Fd)

type Stream = Fd
data Destination = Pipe | File FilePath | AppendFile FilePath
                   deriving (Eq, Show)
type Redirection = ([Stream], Destination)
type PipelineElement = ([String], [Redirection])
type Pipeline = [PipelineElement]
type List = [Pipeline]
