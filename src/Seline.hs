module Seline (seline, format, Options(..)) where

import Control.Exception (try)
import Data.List (intercalate, sortOn, (\\))
import Data.Maybe (fromMaybe)
import Safe (atMay)
import System.IO.Error ()
import Text.Read (readMaybe)

newtype Options = Options { shorter :: Bool }

defaultOptions :: Options
defaultOptions = Options False

seline
  :: Maybe Options
  -> [String]
  -> [String]
  -> IO [String]
seline options choices' selected = do
  putStrLn $ format options' choices selected
  userLine <- (try getLine :: IO (Either IOError String))
  case userLine of
    (Left _) ->
      case selected of
        [] -> pure []
        h:t ->
          let c = words h <> choices
          in seline options c t
    (Right "") -> pure selected
    (Right line) ->
      let
        numbered :: Maybe String
        numbered = readMaybe line >>= atMay choices
        selection = fromMaybe line numbered
        s = selection:selected
        c = consume (words selection) choices
      in seline options c s
  where
    options' = fromMaybe defaultOptions options
    choices = consume selected choices'

format :: Options -> [String] -> [String] -> String
format o c s =
  let enumerated :: (Int, String) -> String
      enumerated (i, t) = t <> " · " <> show i
      enumerate :: [String] -> [(Int, String)]
      enumerate a =
        let f = if shorter o then sortOn length else id
        in reverse . zip [0..] . f $ a
      activities = intercalate ",  "
        . fmap enumerated
        . enumerate
        $ c
      context = intercalate " > " (reverse s)
  in activities <> "\n" <> context

consume :: [String] -> [String] -> [String]
consume consumed = (\\ consumed)
