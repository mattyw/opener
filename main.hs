import Text.Regex.Posix ((=~))
import Text.Regex

example = ["hello there", "   Failed on foobar:127", "some more", "blahblah.go:1345"]

fileRegex = mkRegex "(\\S+):([0-9]+)"

findMatches :: [String] -> [Maybe [String]]
findMatches ls = map (\x -> matchRegex fileRegex x) ls

filterMatches :: [Maybe [String]] -> [Maybe [String]]
filterMatches ls = filter filterer ls

filterer :: Maybe [String] -> Bool
filterer Nothing = False
filterer (Just x) = True

findAndFilter :: [String] -> [Maybe [String]]
findAndFilter ls = filterMatches (findMatches ls)

toVim :: [String] -> String
toVim (a:b:xs) = "+" ++ b ++ " " ++ a

getFilename ls = fmap toVim (head (findAndFilter ls))

main = putStrLn (show (getFilename example))
