import Homework
import Test.Hspec

-- https://hspec.github.io/
main = hspec $ do
  describe "localMaxima" $ do
    it "localMaxima works correctly for regular arrays" $ do
      localMaxima [2, 9, 5, 6, 1] `shouldBe` [9, 6]
      localMaxima [2, 3, 4, 1, 5] `shouldBe` [4]
      localMaxima [1, 2, 3, 4, 5] `shouldBe` []
    it "localMaxima works correctly for empty array" $ do
      localMaxima [] `shouldBe` []

  describe "transform" $ do
    it "transform should convert [(Int, String)] format to [(Char, Int)] format" $ do
      transform [(1, "AEIOU"), (2, "dg"), (5, "k")] `shouldBe`
        [('a', 1), ('e', 1), ('i', 1), ('o', 1), ('u', 1), ('d', 2), ('g', 2), ('k', 5)]

  describe "rule90" $ do
    it "rule90Step should create the next step of rule 90" $ do
      rule90Step [False, True, False] `shouldBe` [True, False, True]

    it "rule90Step works for an empty array" $ do
      rule90Step [] `shouldBe` []

    it "rule90 should create a list of rule90 steps" $ do
      (take 3 $ rule90 [False, True, False]) `shouldBe`
        [[False, True, False], [True, False, True], [False, False, False]]

    it "rule90 works for an empty array" $ do
      (take 3 $ rule90 []) `shouldBe` [[], [], []]

    it "pretty should take the result of rule 90 and make it pretty" $ do
      (pretty $ take 3 $ rule90 [False, True, False]) `shouldBe` " # \n# #\n   \n"

  describe "lookAndSay" $ do
    it "f should generate a list of look and say starting from 1" $ do
      (take 8 $ f) `shouldBe` ["1", "11", "21", "1211", "111221", "312211", "13112221", "1113213211"]
