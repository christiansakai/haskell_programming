import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "fillInCharacter" $ do
    it "fill a character in a puzzle" $ do
      let puzzle :: Puzzle
          puzzle = Puzzle "hello" [] []

          expected :: Puzzle
          expected = Puzzle "hello" [] ['h']

      fillInCharacter puzzle 'h' `shouldBe` expected

  describe "handleGuess" $ do
    it "handle guess by a user" $ do
      let puzzle :: Puzzle
          puzzle = Puzzle "hello" [] []

          expected :: Puzzle
          expected = Puzzle "hello" [] ['h']

      actual <- handleGuess puzzle 'h' 
      
      actual `shouldBe` expected
