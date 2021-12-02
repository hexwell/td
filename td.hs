import Control.Category ((>>>))
import Control.Monad (unless)
import Data.Set (Set, empty, insert, delete, toList)
import Data.Function ((&))
import System.IO (stdout, hFlush)

type Item = String
type State = Set Item

initial :: State
initial = empty

add :: Item -> State -> State
add = insert

remove :: Int -> State -> State
remove i = flip delete <*> (toList >>> (!! i))

edit :: Int -> Item -> State -> State
edit i t = (remove i) >>> (add t)

exec :: [String] -> State -> State
exec ("a":ss) = add $ unwords ss
exec ("r":i:[]) = remove (read i)
exec ("e":i:ss) = edit (read i) $ unwords ss
exec _ = id

format :: (Int, Item) -> String
format (i, s) = (show i) ++ " " ++ s

menu :: State -> IO ()
menu s = do
  putStrLn ""
  putStrLn "List:"
  toList s & zip [0..] & mapM_ (format >>> putStrLn)
  putStrLn "--- ~ ---"
  putStrLn ""

  putStr "> "
  hFlush stdout
  cmd <- getLine

  unless (cmd == "q")
    $ menu $ exec (words cmd) s

main :: IO ()
main = menu initial
