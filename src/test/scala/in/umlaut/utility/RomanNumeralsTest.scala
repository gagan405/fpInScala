package in.umlaut.utility

import org.scalatest.{FunSuite, Matchers}

class RomanNumeralsTest extends FunSuite with Matchers {

  test("Should get correct numeric value"){
    RomanNumerals.roman2Int("VIII") should equal(8)
    RomanNumerals.roman2Int("IV") should equal(4)
    RomanNumerals.roman2Int("XIX") should equal(19)
    RomanNumerals.roman2Int("IX") should equal(9)
    RomanNumerals.roman2Int("XIV") should equal(14)
    RomanNumerals.roman2Int("MMCCLXXI") should equal(2271)
    RomanNumerals.roman2Int("XC") should equal(90)
  }

  test("Should get correct roman value"){
    RomanNumerals.int2Roman(8) should equal("VIII")
    RomanNumerals.int2Roman(4) should equal("IV")
    RomanNumerals.int2Roman(10) should equal("X")
    RomanNumerals.int2Roman(1000) should equal("M")
    RomanNumerals.int2Roman(19) should equal("XIX")
    RomanNumerals.int2Roman(9) should equal("IX")
    RomanNumerals.int2Roman(14) should equal("XIV")
    RomanNumerals.int2Roman(2271) should equal("MMCCLXXI")
    RomanNumerals.int2Roman(90) should equal("XC")
  }
}
