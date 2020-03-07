import System.Directory
import System.FilePath
import Data.List (dropWhileEnd)
import Data.Char (isSpace, toUpper)

main = do
  mainloop getCurrentDirectory

mainloop :: IO FilePath -> IO ()
mainloop p = do
  path <- p
  setCurrentDirectory path
  putStr $ path ++ "$ "

  input <- getLine
  let args = takeN 3 $ words input

  execute args

  mainloop getCurrentDirectory

stripJust :: Maybe a -> a
stripJust Nothing  = error "tried to strip nothing"
stripJust (Just a) = a

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

upCase :: String -> String
upCase = map toUpper

takeN :: Int -> [a] -> [Maybe a]
takeN 0 xs = []
takeN n [] = [Nothing] ++ takeN (n-1) []
takeN n (x:xs) = [Just x] ++ takeN (n-1) xs

execute :: [Maybe String] -> IO ()
execute (cmd:args) =
  case cmd of
    (Just "cd") -> cd args

cd :: [Maybe FilePath] -> IO ()
cd ((Just p):xs) = setCurrentDirectory p
cd (Nothing:xs)  = do
  path <- getHomeDirectory
  setCurrentDirectory path