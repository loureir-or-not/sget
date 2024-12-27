module Main where
import           Data.List          (elemIndex)
import           System.Environment (lookupEnv)
import           System.IO          (readFile')

unknownOrString :: Maybe String -> String
unknownOrString (Just val) = val
unknownOrString Nothing    = "Unknown"

lookupEnv' :: String -> IO String
lookupEnv' env = do
    res <- lookupEnv env
    return $ unknownOrString res

splitAt' :: Char -> String -> (String, String)
splitAt' _ [] = ("", "")
splitAt' s xs = case elemIndex s xs of
    Nothing -> (xs, "")
    Just i  -> splitAt i xs

findPrettyName :: [(String, String)] -> String
findPrettyName [] = "Generic Linux"
findPrettyName (x:xs)
    | fst x == "PRETTY_NAME" = init . tail . tail $ snd x
    | otherwise = findPrettyName xs

getUptime :: String -> String
getUptime ""      = "0 seconds"
getUptime seconds = (head . words $ seconds) ++ " seconds"

getKernel :: String -> String
getKernel = (\l -> head l ++ " " ++ (head . tail) l) . words

parseKB :: String -> Int
parseKB size = read . head . tail . words $ size :: Int

getRamUsage :: String -> String
getRamUsage usage = show memUsed ++ "MB/" ++ show memTotal ++ "MB"
    where
        usageAsKvPairs = map (splitAt' ':') (lines usage)
        memUsed = memTotal - parseKB (snd . head . tail $ usageAsKvPairs) `div` 1000
        memTotal = parseKB (snd . head $ usageAsKvPairs) `div` 1000

getExeNameFromPath :: String -> String
getExeNameFromPath = reverse . takeWhile (/='/') . reverse

wslCheck :: Maybe String -> String
wslCheck Nothing  = ""
wslCheck (Just _) = "Hey! I am using WSL!"

main :: IO ()
main = do
    user <- lookupEnv' "USER"
    lang <- lookupEnv' "LANG"
    xdgSession <- lookupEnv' "XDG_SESSION_TYPE"
    xdgDesktop <- lookupEnv' "XDG_CURRENT_DESKTOP"
    shell <- lookupEnv' "SHELL"
    editor <- lookupEnv' "EDITOR"
    hostname <- lookupEnv' "NAME"
    osRelease <- readFile' "/etc/os-release"
    kernel <- readFile' "/proc/version"
    uptime <- readFile' "/proc/uptime"
    ram <- readFile' "/proc/meminfo"
    usingWSL <- lookupEnv "WSL_DISTRO_NAME"
    putStrLn (unlines
        [ " \x1b[1;35m⠀⠀⢀⣤⣤⣤⣤⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀\x1b[1;39m  " ++ user ++ "@" ++ filter (/='\n') hostname
        , " \x1b[1;35m⠀⠀⢸⣿⣿⣿⣿⣿⣷⡀⠀⠀⠀⠀⠀⠀⠀\x1b[1;39m  "
        , " \x1b[1;35m⠀⠀⠘⠉⠉⠙⣿⣿⣿⣷⠀⠀⠀⠀⠀⠀⠀\x1b[1;39m  " ++ "os\t    " ++ (findPrettyName . map (splitAt' '=')) (lines osRelease)
        , " \x1b[1;35m⠀⠀⠀⠀⠀⠀⢸⣿⣿⣿⣧⠀⠀⠀⠀⠀⠀\x1b[1;39m  " ++ "wm\t    " ++ xdgDesktop ++ " (" ++ xdgSession ++ ")"
        , " \x1b[1;35m⠀⠀⠀⠀⠀⠀⣼⣿⣿⣿⣿⣆⠀⠀⠀⠀⠀\x1b[1;39m  " ++ "locale  " ++ lang
        , " \x1b[1;35m⠀⠀⠀⠀⠀⣼⣿⣿⣿⣿⣿⣿⡀⠀⠀⠀⠀\x1b[1;39m  " ++ "kernel  " ++ getKernel kernel
        , " \x1b[1;35m⠀⠀⠀⠀⣴⣿⣿⣿⠟⣿⣿⣿⣷⠀⠀⠀⠀\x1b[1;39m  " ++ "uptime  " ++ getUptime uptime
        , " \x1b[1;35m⠀⠀⠀⣰⣿⣿⣿⡏⠀⠸⣿⣿⣿⣇⠀⠀⠀\x1b[1;39m  " ++ "ram\t    " ++ getRamUsage ram
        , " \x1b[1;35m⠀⠀⢠⣿⣿⣿⡟⠀⠀⠀⢻⣿⣿⣿⡆⠀⠀\x1b[1;39m  " ++ "shell   " ++ getExeNameFromPath shell
        , " \x1b[1;35m⠀⢠⣿⣿⣿⡿⠀⠀⠀⠀⠀⢿⣿⣿⣷⣤⡄\x1b[1;39m  " ++ "editor  " ++ getExeNameFromPath editor
        , " \x1b[1;35m⢀⣾⣿⣿⣿⠁⠀⠀⠀⠀⠀⠈⠿⣿⣿⣿⡇\x1b[1;39m  " ++ wslCheck usingWSL
        ])
