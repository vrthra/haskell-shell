module Shell.Parse.Lex
     ( ShellToken(..)
     , showToken
     , lexInput
     ) where

data ShellToken = Blank
                | Word String
                | Operator String
                | Quote Char String
                deriving Show

showToken :: ShellToken -> String
showToken Blank        = " "
showToken (Word s)     = s
showToken (Operator s) = s
showToken (Quote c s)  = s


lexInput' :: String -> [ShellToken]
lexInput' "" = []
lexInput' (c:rem)
  | isSpace c            = Blank : (lexInput' rem)
  | isQuote c            = let (cs, _:rest) = break (== c) rem in
                                          (Word cs) : (lexInput' rest)
  | [c] `elem` operators = let (o, rest) = takeOperator [c] rem in
                                          (Operator o) : (lexInput' rest)
  | otherwise            = let (cs, rest) = break (\x -> isSpace x || isQuote x || [x] `elem` operators) rem in
                                     (Word (c:cs)) : (lexInput' rest)
  where isSpace = (==) ' '
        isQuote = flip elem "'\"`"

        takeOperator :: String -> String -> (String, String)
        takeOperator o []     = (o, "")
        takeOperator o (x:xs) = let n = o ++ [x] in
                                if n `elem` operators
                                then takeOperator n xs
                                else (o, x:xs)


lexInput :: String -> [ShellToken]
lexInput = dropBlanks . lexInput'
  where dropBlanks = filter isNotBlank
             where isNotBlank Blank = False
                   isNotBlank _     = True

operators = [ ";", "&", "&&", "||" ] ++ [ "|", "|&" ] ++ [ "<", ">", ">|", "<<", ">>", "<&", ">&", "<<-", "<>" ]

