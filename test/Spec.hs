import Test.Hspec

import Control.Exception (evaluate)

import Run
import Lifetime
import Syntax

main :: IO ()
main = mapM_ hspec [ generalTests
                   , lifetimeTests
                   ]

lterr = (== ErrLifetimeViolation)

generalTests = do
    describe "General tests" $ do
        it "tests the loading code" $ do
            ran <- runFromFile "test_programs/lifetime/test1.calc" runLt
            ran `shouldBe` LTDummy

        it "fails with var lookup" $ do
            evaluate (ltgo "x") `shouldThrow` (== ErrVarNotFound "x")

lifetimeTests = do
    describe "Lifetime tests, some good, some bad" $ do
        it "gets lifetime of a lit" $ do
            ltgo "1" `shouldBe` LTDummy

        it "func lifetime is dummy" $ do
            ran <- runFromFile "test_programs/lifetime/test1.calc" runLt
            ran `shouldBe` LTDummy

        it "func fails, borrow lifetime evaluation" $ do
            ran <- runFromFile "test_programs/lifetime/test2.calc" runLt
            evaluate ran `shouldThrow` lterr

        it "func body is a constant, with dummy lt" $ do
            ran <- runFromFile "test_programs/lifetime/test3.calc" runLt
            ran `shouldBe` LTDummy

        it "borrow at basic scope has lt 0" $ do
            ran <- runFromFile "test_programs/lifetime/test4.calc" runLt
            ran `shouldBe` (LifetimeLit 0)

        it "check borrow fails at nested scope" $ do
            ran <- runFromFile "test_programs/lifetime/test5.calc" runLt
            evaluate ran `shouldThrow` lterr

        it "checks let in violations" $ do
            ran <- runFromFile "test_programs/lifetime/test6.calc" runLt
            evaluate ran `shouldThrow` lterr

        it "checks let in with constant" $ do
            ran <- runFromFile "test_programs/lifetime/test7.calc" runLt
            ran `shouldBe` LTDummy

        it "function returns dummy" $ do
            ran <- runFromFile "test_programs/lifetime/test8.calc" runLt
            ran `shouldBe` LTDummy

        it "returns the lifetime of a de reference ok" $ do
            pendingWith "implement dereference"

ltgo s = runThing s runLt
