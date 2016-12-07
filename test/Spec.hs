import Test.Hspec

import Control.Exception (evaluate)

import Run
import Lifetime
import Syntax

main :: IO ()
main = mapM_ hspec [ lifetimeTests
                   , generalTests
                   ]

langerr = const True :: Selector LangError

generalTests = do
    describe "General tests" $ do
        it "tests the loading code" $ do
            ran <- runFromFile "test_programs/lifetime/test1.calc" runLt
            ran `shouldBe` LTDummy

lifetimeTests = do
    describe "Lifetime tests, some good, some bad" $ do
        it "gets lifetime of a lit" $ do
            ltgo "1" `shouldBe` LTDummy

        it "fails with var lookup" $ do
            evaluate (ltgo "x") `shouldThrow` (const True :: Selector LangError)

        it "func lifetime is dummy" $ do
            ran <- runFromFile "test_programs/lifetime/test1.calc" runLt
            ran `shouldBe` LTDummy

        it "func fails, borrow lifetime evaluation" $ do
            ran <- runFromFile "test_programs/lifetime/test2.calc" runLt
            evaluate ran `shouldThrow` langerr

ltgo s = runThing s runLt
