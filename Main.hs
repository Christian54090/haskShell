import System.Directory
import System.FilePath
import Control.Exception
import System.IO
import Data.List (dropWhileEnd)
import Data.Char (isSpace, toUpper)

main = do
  mainloop getCurrentDirectory

mainloop :: IO FilePath -> IO ()
mainloop p = do
  path <- p
  setCurrentDirectory path
  formattedPath >>= putStr

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

formattedPath :: IO FilePath
formattedPath = do
  home <- getHomeDirectory
  current <- getCurrentDirectory
  return $ "~" ++ drop (length home) current ++ "$ "

takeN :: Int -> [a] -> [Maybe a]
takeN 0 xs = []
takeN n [] = [Nothing] ++ takeN (n-1) []
takeN n (x:xs) = [Just x] ++ takeN (n-1) xs

execute :: [Maybe String] -> IO ()
execute (cmd:args) =
  case cmd of
    (Just "cd") -> cd args
    (Just "mv") -> mv args
    (Just "rm") -> rm args
    (Just "touch") -> touch args
    (Just "rmdir") -> rmdir args
    (Just "mkdir") -> mkdir args
    (Just "ls") -> ls args
    (Just "cat") -> cat args
    (Nothing) -> putStr ""
    _           -> putStrLn "error: unrecognized command"

cd :: [Maybe FilePath] -> IO ()
cd ((Just p):xs) = catch (setCurrentDirectory p) handler
  where handler :: SomeException -> IO ()
        handler ex = putStrLn "cd: path does not exist"
cd (Nothing:xs)  = do
  path <- getHomeDirectory
  setCurrentDirectory path

mv :: [Maybe FilePath] -> IO ()
mv (old:new:_) =
  case (stripJust old, stripJust new) of
    (Left _,_)         -> putStrLn "mv: missing file operand"
    (_,Left _)         -> putStrLn "mv: missing file operand"
    (Right o, Right n) -> catch (renameFile o n) handler
      where handler :: SomeException -> IO ()
            handler ex = putStrLn "mv: one or more files or directories stated do not exist"

rm :: [Maybe FilePath] -> IO ()
rm (path:_) =
  case (stripJust path) of
    (Left _)  -> putStrLn "rm: missing file operand"
    (Right p) -> catch (removeFile p) handler
      where handler :: SomeException -> IO ()
            handler ex = putStrLn $ "rm: cannot remove '" ++ p ++ "': No such file"

touch :: [Maybe FilePath] -> IO ()
touch (Nothing:_)  = putStrLn "touch: missing file operand"
touch ((Just p):_) = do
  h <- openFile p AppendMode
  hClose h

rmdir :: [Maybe FilePath] -> IO ()
rmdir (path:_) =
  case path of
    Nothing  -> putStrLn "rmdir: missing file operand"
    (Just p) -> catch (removeDirectory p) handler
      where handler :: SomeException -> IO ()
            handler ex = putStrLn $ "rmdir: failed to remove '" ++ p ++ "': No such directory"

mkdir :: [Maybe FilePath] -> IO ()
mkdir (path:_) =
  case path of
    Nothing  -> putStrLn "mkdir: missing file operand"
    (Just p) -> catch (createDirectory p) handler
      where handler :: SomeException -> IO ()
            handler ex = putStrLn "rmdir: directory already exists"

ls :: [Maybe FilePath] -> IO ()
ls ((Just p):_) = do
  listResult <- try (listDirectory p) :: IO (Either SomeException [FilePath])
  case listResult of
    Left ex -> putStrLn $ "ls: cannot access '" ++ p ++ "': No such file or directory"
    Right l -> mapM_ putStrLn $ filter (\e -> head e /= '.') l
ls (Nothing:_) = do
  path <- getCurrentDirectory
  list <- listDirectory path
  mapM_ putStrLn $ filter (\e -> head e /= '.') list

cat :: [Maybe FilePath] -> IO ()
cat (Nothing:_) = putStrLn "cat: missing file operand"
cat (Just p:_)  = do
  fileExists <- doesFileExist p
  case fileExists of
    False -> putStrLn "cat: file does not exist"
    True  -> do file <- readFile p
                putStr file