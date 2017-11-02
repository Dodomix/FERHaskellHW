import Homework
import Test.Hspec
import Control.Exception (evaluate)

-- https://hspec.github.io/
main = hspec $ do
  describe "To RNA" $ do
    it "toRNA of \"gctagctaAAG\" should be \"CGAUCGAUUUC\"" $ do
      toRNA "gctagctaAAG" `shouldBe` "CGAUCGAUUUC"

    it "nucleotideToRNA of 'G' should be C" $ do
      nucleotideToRNA 'G' `shouldBe` 'C'

    it "nucleotideToRNA of 'C' should be G" $ do
      nucleotideToRNA 'C' `shouldBe` 'G'

    it "nucleotideToRNA of 'T' should be A" $ do
      nucleotideToRNA 'T' `shouldBe` 'A'

    it "nucleotideToRNA of 'A' should be U" $ do
      nucleotideToRNA 'A' `shouldBe` 'U'

  describe "arithmetic" $ do
    describe "multiply" $ do
      it "multiplication of positive integers works correctly" $ do
        multiply 4 7 `shouldBe` 28

      it "multiplying with 0 works correcty" $ do
        multiply 29 0 `shouldBe` 0

      it "multiplying 0 with a number works correctly" $ do
        multiply 0 29 `shouldBe` 0

      it "multiplying when the first number is negative works correctly" $ do
        multiply (-3) 4 `shouldBe` (-12)

      it "multiplying when the second number is negative works correctly" $ do
        multiply 4 (-3) `shouldBe` (-12)

      it "multiplying two negative numbers works correctly" $ do
        multiply (-4) (-7) `shouldBe` 28

    describe "divide" $ do
      it "division of positive numbers works correctly" $ do
        divide 9 4 `shouldBe` 2

      it "division of positive with a negative number works correctly" $ do
        divide 9 (-4) `shouldBe` (-2)

      it "division of negative number with a positive number works correctly" $ do
        divide (-9) 4 `shouldBe` (-2)

      it "division of two negative numbers works correctly" $ do
        divide (-9) (-4) `shouldBe` 2

    describe "gcd" $ do
      it "gcd works correctly" $ do
        greatestCD 42 56 `shouldBe` 14

    it "isOperationNegative should be true if the operation should return a negative result" $ do
      isOperationNegative [-3, 5, -7, -12] `shouldBe` True

    it "isOperationNegative should be false if the operation should return a positive result" $ do
      isOperationNegative [-3, 5, -7] `shouldBe` False

  describe "number to words" $ do
    it "numberToWords works correctly for 1" $ do
      numberToWords 1 `shouldBe` "one"

    it "numberToWords works correctly for 10" $ do
      numberToWords 10 `shouldBe` "ten"

    it "numberToWords works correctly for 11" $ do
      numberToWords 11 `shouldBe` "eleven"

    it "numberToWords works correctly for 100" $ do
      numberToWords 100 `shouldBe` "one hundred"

    it "numberToWords works correctly for 115" $ do
      numberToWords 115 `shouldBe` "one hundred fifteen"

    it "numberToWords works correctly for 1213" $ do
      numberToWords 1213 `shouldBe` "one thousand two hundred thirteen"

    it "numberToWords works correctly for 1005" $ do
      numberToWords 1005 `shouldBe` "one thousand five"

    it "numberToWords works correctly for 22213" $ do
      numberToWords 22213 `shouldBe` "twenty-two thousand two hundred thirteen"

    it "numberToWords works correctly for 1000000" $ do
      numberToWords 1000000 `shouldBe` "one million"

    it "numberToWords works correctly for 1000001" $ do
      numberToWords 1000001 `shouldBe` "one million one"

    it "numberToWords works correctly for 1002001" $ do
      numberToWords 1002001 `shouldBe` "one million two thousand one"

    it "numberToWords works correctly for 0" $ do
      numberToWords 0 `shouldBe` "zero"

    it "trim should trim trailing whitespace of a string" $ do
      trim "dominik   " `shouldBe` "dominik"

    it "digits should return the digits of a number as list" $ do
      digits 123456 `shouldBe` [1, 2, 3, 4, 5, 6]
