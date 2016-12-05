import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
    describe "Lib test" $ do
        it "matchs lol" $ do
            otherFunc `shouldBe` "lol"
