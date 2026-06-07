{-# LANGUAGE LambdaCase #-}
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
