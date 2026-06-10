{-# LANGUAGE LambdaCase #-}
{- |


Seline is a small package providing something like a web @select@
input for command line scripts

= Usage

Here is an interpreter session showcasing the interface:

@
ghci> import qualified Seline
ghci> Seline.simple ["apples", "oranges", "grapes"]
grapes · 3,  oranges · 2,  apples · 1

1
grapes · 2,  oranges · 1
apples
2
oranges · 1
apples > grapes

Just ["grapes","apples"]
@

Users can type a number or a full item to select it, then seline
prints remaining choices. Typing an empty line causes seline to return
the whole selection

= Maybe

Seline returns a @Maybe [String]@ because @Nothing@ indicates that an
user pressed @Ctrl-D@. So application code can distinguish empty input
submission (no selection) from input termination (quitting the
interface)

-}
module Seline
  ( simple,
    seline,
    Options(..),
    -- ** Internal functions exported for testing
    format,
    user,
    Action(..)
  ) where

import Control.Exception (try)
import Data.List (intercalate, sortOn, (\\))
import Data.Maybe (fromMaybe)
import Safe (atMay)
import System.IO.Error ()
import Text.Read (readMaybe)

newtype Options = Options {
  shorter :: Bool -- ^ Assign smaller numbers to shorter choices (default False)
  }

defaultOptions :: Options
defaultOptions = Options False

simple :: [String] -> IO (Maybe [String])
simple s = seline Nothing s []

seline
  :: Maybe Options
  -> [String]
  -- ^ Choices for the user
  -> [String]
  -- ^ Selected choices
  -> IO (Maybe [String])
seline options choices' selected = do
  let choices = choices' \\ selected
  putStrLn $ format (fromMaybe defaultOptions options) choices selected
  line <- try getLine
  case user choices selected line of
    Quit -> pure Nothing
    Specify c t -> seline options c t
    Enter -> pure . Just $ selected

data Action = Quit | Specify [String] [String] | Enter deriving (Show, Eq)

user :: [String] -> [String] -> Either IOError String -> Action
user choices selected = \case
  (Left _) ->
    case selected of
      [] -> Quit
      h:t ->
        let c = words h <> choices
        in Specify c t
  (Right "") -> Enter
  (Right line) ->
    let
      numbered :: Maybe String
      numbered = readMaybe line >>= atMay choices . pred
      selection = fromMaybe line numbered
      s = selection:selected
      c = choices \\ words selection
    in Specify c s

format :: Options -> [String] -> [String] -> String
format o c s =
  let enumerated :: (Int, String) -> String
      enumerated (i, t) = t <> " · " <> show i
      enumerate :: [String] -> [(Int, String)]
      enumerate a =
        let f = if shorter o then sortOn length else id
        in reverse . zip [1..] . f $ a
      activities = intercalate ",  "
        . fmap enumerated
        . enumerate
        $ c
      context = intercalate " > " (reverse s)
  in activities <> "\n" <> context
