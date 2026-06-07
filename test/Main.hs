module Main (main) where

import GHC.IO.Exception (IOException(..), IOErrorType(EOF))
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
  context "user" $ do
    specify "selects an item by number" $
      user ["apples"] [] (Right "1") `shouldBe` Specify [] ["apples"]
    specify "selects an item by typing it" $
      user ["apples"] [] (Right "apples") `shouldBe` Specify [] ["apples"]
    specify "inserts a new item when not existent" $
      user ["apples"] [] (Right "new") `shouldBe` Specify ["apples"] ["new"]
    context "Ctrl-D" $ do
      let e = IOError Nothing EOF "" "" Nothing Nothing
      specify "removes last selection" $
        user [] ["apples"] (Left e) `shouldBe` Specify ["apples"] []
      specify "quits if there is no selection" $
        user [] [] (Left e) `shouldBe` Quit
    specify "can select something" $
      user [] ["apples"] (Right "") `shouldBe` Enter
    specify "can select nothing" $
      user ["apples"] [] (Right "") `shouldBe` Enter
