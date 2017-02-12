import Text.ParserCombinators.Parsec
import System.Environment
import System.Process
import System.Exit
import System.IO

isFilename :: String -> Maybe (String, String)
isFilename = undefined

data FileName = FN String String deriving Show

fileNamePart:: Parser String
fileNamePart = many1 $ oneOf "&\\/ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_-+."

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

findFileName :: [String] -> Either ParseError FileName
findFileName [] = error "nothing"
findFileName (x: xs) = case parseFileName x of
    Left x -> findFileName xs
    Right x -> Right x

parseText :: String -> Either ParseError FileName
parseText inp = findFileName (words inp)

toVim :: FileName -> String
toVim (FN name line) = "vim --remote +" ++ line ++ " " ++ name

vimOpen :: Either ParseError FileName -> IO ExitCode
vimOpen (Left _) = exitWith (ExitFailure 1)
vimOpen (Right cmd) = system $ toVim cmd

run :: [String] -> IO a
run [] = do
    hPutStrLn stderr "Reading from Stdin"
    inp <- getContents
    vimOpen (parseText inp)
    exitWith ExitSuccess
run (x:_) = do
    let fn = parseFileName x
    vimOpen fn
    exitWith ExitSuccess


main = do
    args <- getArgs
    run args
