import Homework
import Test.Hspec

-- https://hspec.github.io/
main = hspec $ do
  describe "Leap year" $ do
    it "1997 is not a leap year" $ do
      isLeapYear 1997 `shouldBe` False

    it "1996 is a leap year" $ do
      isLeapYear 1996 `shouldBe` True

    it "1900 is not a leap year" $ do
      isLeapYear 1900 `shouldBe` False

    it "2000 is a leap year" $ do
      isLeapYear 2000 `shouldBe` True

    it "leapList should contain all leap years from 1996 to 2017" $ do
      leapList `shouldBe` [1996, 2000, 2004, 2008, 2012, 2016]

  describe "Exponential function" $ do
    it "Correctly evaluates polynomial with x = 0" $ do
      evaluate 0 [2, 3, 4] `shouldBe` 2

    it "Correctly evaluates polynomial with x = 1" $ do
      evaluate 1 [2, 3, 4] `shouldBe` 9

    it "Correctly evaluates arbitrary polynomial" $ do
      evaluate 2.5 [3, 7.2, 6.4, 2] `shouldBe` 92.25

    it "Correctly calculates factorial of 0" $ do
      factorial 0 `shouldBe` 1

    it "Correctly calculates arbitrary factorial" $ do
      factorial 12 `shouldBe` 479001600

    it "Correctly calculates e^0" $ do
      exp' 0 `shouldBe` 1.0

    it "Calculates e^5 with a small error" $ do
      abs(exp 5 - exp' 5) < 1e-9 `shouldBe` True

    it "Calculates e^10 with a small error" $ do
      abs(exp 5 - exp' 5) < 1e-9 `shouldBe` True

  describe "Key value list" $ do
    it "Should return the item if it exists in the list" $ do
      findItem [("key1", 0), ("key2", 5), ("key3", 7)] "key2" `shouldBe` [("key2", 5)]

    it "Should return an empty list if the key value list does not contain an item" $ do
      findItem [("key1", 0), ("key2", 5)] "key3" `shouldBe` []

    it "Contains should return true if the key is contained in the list" $ do
      contains [("key1", 0), ("key2", 5), ("key3", 7)] "key2" `shouldBe` True

    it "Contains should return false if the key is not contained in the list" $ do
      contains [("key1", 0), ("key2", 5)] "key3" `shouldBe` False

    it "Lookup should return the value if the key exists" $ do
      Homework.lookup [("key1", 0), ("key2", 5)] "key2" `shouldBe` 5

    it "Lookup should throw an error if the key doesn't exist" $ do
      Homework.lookup [] "key2" `shouldThrow` anyException

    it "Insert should insert the element into the list if it doesn't exist" $ do
      Homework.insert [("key1", 0), ("key2", 5)] ("key3", 7) `shouldBe` [("key3", 7), ("key1", 0), ("key2", 5)]

    it "Insert shouldn't do anything if the key already exists" $ do
      Homework.insert [("key1", 0), ("key2", 5)] ("key2", 1) `shouldBe` [("key1", 0), ("key2", 5)]

    it "Remove should remove an item with the key given as the argument" $ do
      remove [("key1", 0), ("key2", 5)] "key2" `shouldBe` [("key1", 0)]

    it "Remove should do nothing if the key doesn't exist" $ do
      remove [("key1", 0), ("key2", 5)] "key3" `shouldBe` [("key1", 0), ("key2", 5)]

    it "Update updates the value of a key" $ do
      update [("key1", 0), ("key2", 5)] "key2" 2 `shouldBe` [("key1", 0), ("key2", 2)]

    it "Update doesn't do anything if the key doesn't exist" $ do
      update [("key1", 0), ("key2", 5)] "key3" 9 `shouldBe` [("key1", 0), ("key2", 5)]
  
  describe "Cosine similarity" $ do
    it "lowerCaseLetters should convert all letters of a word to lower case" $ do
      lowerCaseLetters "tHiS IS a weIRD Way to TYPe" `shouldBe` "this is a weird way to type"

    it "lowerCaseWords returns the words of a string in lower case" $ do
      lowerCaseWords "tHiS IS a weIRD Way to TYPe A a" `shouldBe` ["this", "is", "a", "weird", "way", "to", "type", "a", "a"]

    it "wordsUnion should return all the words from two strings, converted to lowercase" $ do
      wordsUnion "tHiS IS a weIRD WAY" "Way to TYPe A a" `shouldBe` ["this", "is", "a", "weird", "way", "to", "type"]

    it "constructWordsVector should return a vector with 1s on positions of words in the string and the allWords vector" $ do
      constructWordsVector "tHiS IS a weIRD TYPE" ["this", "is", "a", "weird", "way", "to", "type"] `shouldBe` [1, 1, 1, 1, 0, 0, 1]

    it "wordCount should return the the number of times a word appears in a list of words" $ do
      wordCount "a" ["this", "is", "a", "weird", "way", "to", "type", "a", "a"] `shouldBe` 3.0

    it "dotProduct should return the dot product of two vectors" $ do
      dotProduct [1, 0, 1, 1] [1, 1, 0, 1] `shouldBe` 2.0

    it "vectorDistance should return the absolute value of a vector" $ do
      vectorDistance [1, 2, 0, 4, 3, 2, 1, 1] `shouldBe` 6.0

    it "computeCosineSimilarity should return the cosine similarity of two vectors" $ do
      computeCosineSimilarity [1, 0, 0] [3, 4, 0] `shouldBe` 0.6

    it "cosineSimilarity should return the cosine similarity of two strings" $ do
      abs (cosineSimilarity "Haskell makes me giddy! I want to jump around like a little girl." "Haskell makes me happy! So much that I want to jump around like a little pony" - 0.76271276980969) < 1e-9 `shouldBe` True
