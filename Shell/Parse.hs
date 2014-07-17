module Shell.Parse (parseInput) where
import qualified Data.List.Split as S
import System.Posix.IO (stdInput, stdOutput, stdError)
import qualified Shell.Grammar as G
import Shell.Parse.Lex

-- TODO : Switch to parsec
-- G.Commands = ([String], [([Fd], Destination)])
-- G.Pipeline = [G.Commands]
-- G.List = [G.Pipeline]

parseInput :: String -> G.List
parseInput = parseList . lexInput

parseList :: [ShellToken] -> G.List
parseList lst = filter (/= [([], [])]) $ map parsePipeline mylst
  where mylst = S.splitWhen (isOperator listOperators) lst

parsePipeline :: [ShellToken] -> G.Pipeline
parsePipeline lst = map parsePipelineElement $ S.split (S.keepDelimsR $ (S.whenElt (isOperator pipelineOperators))) lst

parsePipelineElement :: [ShellToken] -> G.PipelineElement
parsePipelineElement lst = (\(cmd, rs) -> (parseCommand cmd, parseRedirections rs)) $ break (isOperator redirectionOperators) lst

parseRedirections :: [ShellToken] -> [G.Redirection]
parseRedirections ((Operator "|" ):xs) = ([stdOutput], G.Pipe) : parseRedirections xs
parseRedirections ((Operator "|&"):xs) = ([stdOutput, stdError], G.Pipe) : parseRedirections xs
parseRedirections ((Operator ">" ):(Word s):xs) = ([stdOutput], G.File s) : parseRedirections xs
parseRedirections ((Operator ">>"):(Word s):xs) = ([stdOutput], G.AppendFile s) : parseRedirections xs
parseRedirections ((Operator ">&"):(Word s):xs) = ([stdOutput, stdError], G.File s) : parseRedirections xs
parseRedirections (_:xs) = parseRedirections xs
parseRedirections []     = []

parseCommand :: [ShellToken] -> [String]
parseCommand = map showToken

isOperator :: [String] -> ShellToken -> Bool
isOperator ops (Operator s) = s `elem` ops
isOperator _   _            = False

-- based on bash(1) and dash(1) man pages
-- These are operators that separate multiple pipelines in the single command.
listOperators = [ ";", "&", "&&", "||" ]
pipelineOperators = [ "|", "|&" ]
redirectionOperators = pipelineOperators ++ [ "<", ">", ">|", "<<", ">>", "<&", ">&", "<<-", "<>" ]
operators = listOperators ++ redirectionOperators -- "(", ")", ";;"

