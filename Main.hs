import System.Directory
import System.FilePath
import Control.Exception
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

type ArgError = String

stripJust :: Maybe a -> Either ArgError a
stripJust Nothing  = Left "tried to strip nothing"
stripJust (Just a) = Right a

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
    (Just "mv") -> mv args

cd :: [Maybe FilePath] -> IO ()
cd ((Just p):xs) = catch (setCurrentDirectory p) handler
  where handler :: SomeException -> IO ()
        handler ex = putStrLn "cd: path does not exist"
cd (Nothing:xs)  = do
  path <- getHomeDirectory
  setCurrentDirectory path

mv :: [Maybe FilePath] -> IO ()
mv args@(old:new:_) =
  case (stripJust old, stripJust new) of
    (Left _,_)         -> putStrLn "mv: missing file operand"
    (_,Left _)         -> putStrLn "mv: missing file operand"
    (Right o, Right n) -> catch (renameFile o n) handler
      where handler :: SomeException -> IO ()
            handler ex = putStrLn "mv: one or more files or directories mentioned do not exist"
