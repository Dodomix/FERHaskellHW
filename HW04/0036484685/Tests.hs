import Homework
import Test.Hspec

-- https://hspec.github.io/
main = hspec $ do
  describe "factorial" $ do
    it "factorial works correctly for edge values" $ do
      factorial 0 `shouldBe` 1
      factorial 1 `shouldBe` 1
    it "factorial works correctly" $ do
      factorial 5 `shouldBe` 120

  describe "sum" $ do
    it "sum' works correctly for empty list" $ do
      sum' [] `shouldBe` 0
    it "sum' should return sum of list elements" $ do
      sum' [1, 3, -5, 14] `shouldBe` 13

  describe "accumulator factorial" $ do
    it "factorial' works correctly for edge values" $ do
      factorial' 0 `shouldBe` 1
      factorial' 1 `shouldBe` 1

    it "factorial' works correctly" $ do
      factorial' 5 `shouldBe` 120

  describe "accumulator sum" $ do
    it "sum'' works correctly for empty list" $ do
      sum'' [] `shouldBe` 0
    it "sum'' should return sum of list elements" $ do
      sum'' [1, 3, -5, 14] `shouldBe` 13

  describe "nats" $ do
    it "nats returns natural numbers" $ do
      (take 10 $ nats) `shouldBe` [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

  describe "map'" $ do
    it "map' the same as map" $ do
      map' (+10) [1, 2, 3] `shouldBe` map (+10) [1, 2, 3]

  describe "zip'" $ do
    it "zip' works the same as zip" $ do
      zip' [1, 2, 3] "abcd" `shouldBe` zip [1, 2, 3] "abcd"
      zip' [1, 2, 3, 4] "abc" `shouldBe` zip [1, 2, 3, 4] "abc"

  describe "subsets" $ do
    it "equalSubsets returns true if subsets equal" $ do
      equalSubsets [1, 2] [2, 1] `shouldBe` True
    it "equalSubsets returns false if subsets not equal" $ do
      equalSubsets [1, 2] [1, 2, 3] `shouldBe` False
    it "subsets returns subsets of length 2 of a set" $ do
      (length $ subsets 2 [1, 2, 3]) `shouldBe` 3
      (any (`equalSubsets` [1, 2]) $ subsets 2 [1, 2, 3]) `shouldBe` True
      (any (`equalSubsets` [1, 3]) $ subsets 2 [1, 2, 3]) `shouldBe` True
      (any (`equalSubsets` [2, 3]) $ subsets 2 [1, 2, 3]) `shouldBe` True

    it "subsets returns no subsets when length 0" $ do
      subsets 0 [1, 2, 3] `shouldBe` [[]]

    it "subsets returns no subsets when length longer than set length" $ do
      subsets 4 [1, 2, 3] `shouldBe` [[]]
