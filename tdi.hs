import Control.Category ((>>>))
import Control.Monad (unless)
import Data.Function ((&))
import Data.Functor (void)
import Data.List (isInfixOf)
import Data.Set (Set, empty, toList, insert, delete)
import System.IO (stdout, hFlush)
import System.Process (system)

type Item = String
data Mode = C | A | R | E deriving Eq
type Text = String
data State = State (Set Item) Mode Text

instance Show Mode where
  show C = "!"
  show A = "a"
  show R = "r"
  show E = "e"

initial :: State
initial = State empty C ""

format :: (Int, Item) -> String
format (i, s) = show i ++ " " ++ s

setMode :: Mode -> State -> State
setMode m (State s _ text) = State s m text

clear :: State -> State
clear (State set mode _) = State set mode ""

add :: State -> State
add (State set mode text) = State (insert text set) mode text

remove :: State -> State
remove (State set mode textIndex) = State (delete (toList set !! read textIndex) set) mode textIndex

edit :: State -> State
edit (State set mode textIndex) = let item = toList set !! read textIndex in State (delete item set) mode item

textbox :: (State -> State) -> Char -> State -> State
textbox _      '!'    s                     = (clear >>> setMode C) s
textbox action '\n'   s                     = action s
textbox _      '\DEL' (State set mode "")   = State set mode ""
textbox _      '\DEL' (State set mode text) = State set mode (init text)
textbox _      char   (State set mode text) = State set mode (text ++ [char])

cmdbox :: Char -> State -> State
cmdbox 'a' = setMode A
cmdbox 'r' = setMode R
cmdbox 'e' = setMode E
cmdbox _   = id

modebox :: State -> Char -> State -> State
modebox (State _ C _) = cmdbox
modebox (State _ A _) = textbox (   add >>> clear >>> setMode C)
modebox (State _ R _) = textbox (remove >>> clear >>> setMode C)
modebox (State _ E _) = textbox (  edit           >>> setMode A)

menu :: State -> IO ()
menu s@(State set mode text) = do
  system "clear"

  putStrLn "List:"

  toList set
    & (
      if mode == A
      then filter (isInfixOf text)
      else id
    )
    & zip [0..]
    & mapM_ (format >>> putStrLn)

  putStrLn "--- ~ ---"
  putStrLn ""

  putStr $ show mode ++ " > " ++ text
  hFlush stdout
  char <- getChar

  unless (char == 'q')
    $ menu
      $ (modebox s) char s

main :: IO ()
main = do
    menu initial
    system "clear" & void
