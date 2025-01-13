module Main where

import Data.List (elemIndex)
import System.Environment (lookupEnv)
import System.IO (readFile')

unknownOrString :: Maybe String -> String
unknownOrString (Just val) = val
unknownOrString Nothing = "Unknown"

lookupEnv' :: String -> IO String
lookupEnv' env = do
  res <- lookupEnv env
  return $ unknownOrString res

splitAt' :: Char -> String -> (String, String)
splitAt' _ [] = ("", "")
splitAt' s xs = case elemIndex s xs of
  Nothing -> (xs, "")
  Just i -> splitAt i xs

findPrettyName :: [(String, String)] -> String
findPrettyName [] = "Generic Linux"
findPrettyName (x : xs)
  | fst x == "PRETTY_NAME" = init . tail . tail $ snd x
  | otherwise = findPrettyName xs

getUptimeSeconds :: String -> Int
getUptimeSeconds "" = 0
getUptimeSeconds seconds = floor (read . head . words $ seconds :: Float)

getUptime :: Int -> String
getUptime time
  | time < 60 = show time
  | time < 3600 = formattedMS
  | otherwise = formattedHMS
  where
    totalHours = time `div` 60
    hours = totalHours `div` 60
    minutes = totalHours `mod` 60
    seconds = time `mod` 60
    formattedS = show seconds ++ "secs"
    formattedMS = show minutes ++ "mins, " ++ formattedS
    formattedHMS = show hours ++ "hrs, " ++ formattedMS

getKernel :: [String] -> String
getKernel [] = ""
getKernel (k : ks) = k ++ " " ++ (head . tail) ks

parseKB :: String -> Int
parseKB size = read . head . tail . words $ size :: Int

getRamUsage :: String -> String
getRamUsage usage = show memUsed ++ "MB/" ++ show memTotal ++ "MB"
  where
    usageAsKvPairs = map (splitAt' ':') (lines usage)
    memUsed = memTotal - parseKB (snd . head . tail $ usageAsKvPairs) `div` 1000
    memTotal = parseKB (snd . head $ usageAsKvPairs) `div` 1000

getExeNameFromPath :: String -> String
getExeNameFromPath = reverse . takeWhile (/= '/') . reverse

wslCheck :: Maybe String -> String
wslCheck Nothing = ""
wslCheck (Just _) = "Hey! I am using WSL!"

getHostname :: String -> String
getHostname s
  | null . lines $ s = "Unknown"
  | otherwise = head . lines $ s

main :: IO ()
main = do
  user <- lookupEnv' "USER"
  lang <- lookupEnv' "LANG"
  xdgSession <- lookupEnv' "XDG_SESSION_TYPE"
  xdgDesktop <- lookupEnv' "XDG_CURRENT_DESKTOP"
  shell <- lookupEnv' "SHELL"
  editor <- lookupEnv' "EDITOR"
  hostname <- readFile' "/etc/hostname"
  osRelease <- readFile' "/etc/os-release"
  kernel <- readFile' "/proc/version"
  uptime <- readFile' "/proc/uptime"
  ram <- readFile' "/proc/meminfo"
  usingWSL <- lookupEnv "WSL_DISTRO_NAME"
  putStrLn
    ( unlines
        [ " \x1b[1;35m⠀⠀⢀⣤⣤⣤⣤⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀\x1b[1;39m  " ++ user ++ "@" ++ getHostname hostname,
          " \x1b[1;35m⠀⠀⢸⣿⣿⣿⣿⣿⣷⡀⠀⠀⠀⠀⠀⠀⠀\x1b[1;39m  ",
          " \x1b[1;35m⠀⠀⠘⠉⠉⠙⣿⣿⣿⣷⠀⠀⠀⠀⠀⠀⠀\x1b[1;39m  " ++ "os\t\t" ++ (findPrettyName . map (splitAt' '=')) (lines osRelease),
          " \x1b[1;35m⠀⠀⠀⠀⠀⠀⢸⣿⣿⣿⣧⠀⠀⠀⠀⠀⠀\x1b[1;39m  " ++ "wm\t\t" ++ xdgDesktop ++ " (" ++ xdgSession ++ ")",
          " \x1b[1;35m⠀⠀⠀⠀⠀⠀⣼⣿⣿⣿⣿⣆⠀⠀⠀⠀⠀\x1b[1;39m  " ++ "locale\t" ++ lang,
          " \x1b[1;35m⠀⠀⠀⠀⠀⣼⣿⣿⣿⣿⣿⣿⡀⠀⠀⠀⠀\x1b[1;39m  " ++ "kernel\t" ++ getKernel (words kernel),
          " \x1b[1;35m⠀⠀⠀⠀⣴⣿⣿⣿⠟⣿⣿⣿⣷⠀⠀⠀⠀\x1b[1;39m  " ++ "uptime\t" ++ getUptime (getUptimeSeconds uptime),
          " \x1b[1;35m⠀⠀⠀⣰⣿⣿⣿⡏⠀⠸⣿⣿⣿⣇⠀⠀⠀\x1b[1;39m  " ++ "ram\t\t" ++ getRamUsage ram,
          " \x1b[1;35m⠀⠀⢠⣿⣿⣿⡟⠀⠀⠀⢻⣿⣿⣿⡆⠀⠀\x1b[1;39m  " ++ "shell\t" ++ getExeNameFromPath shell,
          " \x1b[1;35m⠀⢠⣿⣿⣿⡿⠀⠀⠀⠀⠀⢿⣿⣿⣷⣤⡄\x1b[1;39m  " ++ "editor\t" ++ getExeNameFromPath editor,
          " \x1b[1;35m⢀⣾⣿⣿⣿⠁⠀⠀⠀⠀⠀⠈⠿⣿⣿⣿⡇\x1b[1;39m  " ++ wslCheck usingWSL
        ]
    )
