import Test.Hspec

import Control.Monad.Except
import Control.Exception (evaluate)
import qualified Data.IntMap as I

import Run
import Lifetime
import Syntax
import Borrow

main :: IO ()
main = mapM_ hspec [ generalTests
                   , lifetimeTests
                   , borrowTests
                   , borrowUnitTests
                   ]

lterr = (== ErrLifetimeViolation)
moveerr = (== ErrMovedValue)

generalTests = do
    describe "General tests" $ do
        it "tests the loading code" $ do
            ran <- runFromFile "test_programs/lifetime/test1.calc" runLt
            ran `shouldBe` LTDummy

        it "fails with var lookup" $ do
            evaluate (ltgo "x") `shouldThrow` (== ErrVarNotFound "x")

lifetimeTests = do
    describe "Testing the Lifetime checker. " $ do
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

borrowTests = do
    describe "Testing the borrow checker. " $ do
        it "throws local scope move violation" $ do
            pendingWith "just a sec..."
            -- ran <- runFromFile "test_programs/borrow/test1.calc" runBorrow
            -- evaluate ran `shouldThrow` moveerr

borrowUnitTests = do
    let goodLoan = Loan { lifetime = LTDummy
        , qualifier = Mut
        , restrictions = [(0, [Claim, Freeze])
            , (1, [Mutate])
            ]
        }

    let badLoan = Loan { lifetime = LTDummy
        , qualifier = Mut
        , restrictions = [(0, [Claim, Freeze, Mutate])
            , (1, [Claim, Freeze, Mutate])
            ]
        }

    describe "Unit tests from Borrow" $ do
        it "Test canAssign" $ do
            let lheap = I.singleton 0 goodLoan
            runExcept (canAssign 0 lheap) `shouldBe` Right True

        it "Test canAssign, bad" $ do
            let lheap = I.singleton 0 badLoan
            runExcept (canAssign 0 lheap) `shouldBe` Right False

        it "Test canAssign, loc missing" $ do
            let lheap = I.singleton 0 badLoan
            runExcept (canAssign 999 lheap) `shouldBe` Left (ErrLocNotFound 999)

ltgo s = runThing s runLt
