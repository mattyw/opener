-- The opener reads from stdin and looks for the last item that looks like
-- filename:linenumber and attempts to get vim to open it.
-- vim should have been run using a command such as vim --servername vim
import Text.Regex
import System.Process
import System.Exit

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
toVim (a:b:xs) = "vim --remote +" ++ b ++ " " ++ a

getFilename ls = fmap toVim (last (findAndFilter ls))

vimOpen :: Maybe String -> IO ExitCode
vimOpen Nothing = error "need something to open"
vimOpen (Just cmd) = system cmd

main = do
    inp <- getContents
    let ls = lines inp
    vimOpen (getFilename ls)
