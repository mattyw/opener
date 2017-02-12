import Text.ParserCombinators.Parsec
import System.Environment
import System.Process
import System.Exit

isFilename :: String -> Maybe (String, String)
isFilename = undefined

data FileName = FN String String deriving Show

fileNamePart:: Parser String
fileNamePart = many1 $ oneOf "&ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_-+."

fileNumberPart :: Parser String
fileNumberPart = many1 $ oneOf "1234567890"

filename :: Parser FileName
filename = do
    name <- fileNamePart
    char ':'
    line <- fileNumberPart
    return (FN name line)

parseFileName :: String -> Either ParseError FileName
parseFileName = parse filename "(none found)"
usage = "must specify filename"

toVim :: FileName -> String
toVim (FN name line) = "vim --remote +" ++ line ++ " " ++ name

vimOpen :: Either ParseError FileName -> IO ExitCode
vimOpen (Left _) = exitWith (ExitFailure 1)
vimOpen (Right cmd) = system $ toVim cmd

run :: [String] -> IO a
run [] = putStrLn usage >> exitWith (ExitFailure 1)
run (x:_) = do
    let fn = parseFileName x
    vimOpen fn
    exitWith ExitSuccess


main = do
    args <- getArgs
    run args
