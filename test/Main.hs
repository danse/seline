module Main (main) where

import Seline
import Test.Hspec

main :: IO ()
main = hspec $ do
  context "format" $
    specify "sorts by length" $
    let o = Options True
        c = ["1", "12", "123"]
        s = []
    in format o c s `shouldBe` "123 · 3,  12 · 2,  1 · 1\n"
