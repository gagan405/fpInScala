package in.umlaut.maths

import in.umlaut.maths.Maths._

import org.scalatest.FunSuite

class MathsTest extends FunSuite {
  test("Should get correct nth fibonacci number") {
    assertResult(1){
      nThFibonacci(1)
    }
    assertResult(1){
      nThFibonacci(2)
    }
    assertResult(2){
      nThFibonacci(3)
    }
    assertResult(3){
      nThFibonacci(4)
    }
    assertResult(5){
      nThFibonacci(5)
    }
    assertResult(8){
      nThFibonacci(6)
    }
    assertResult(6765){
      nThFibonacci(20)
    }
  }

  /**
    * Partition function table :
    * http://www.numericana.com/data/partition.htm
    */
  test("Should get the correct partition number") {
    assertResult(5){
      getPartitionsOfN(4)
    }
    assertResult(7){
      getPartitionsOfN(5)
    }
    assertResult(11){
      getPartitionsOfN(6)
    }
    assertResult(15){
      getPartitionsOfN(7)
    }
    assertResult(173525){
      getPartitionsOfN(49)
    }
  }

  test("Get square root integer") {
    assertResult((5, 1)){
      getSquareRootWholeInteger(26)
    }
    assertResult((5, 3)){
      getSquareRootWholeInteger(28)
    }
  }

  test("Get square root integer irrational") {
    assertResult(475){
      getSquareRootDecimals(2)._2
    }
  }

  test("Get prime numbers till n") {
    val x = getPrimesTillN(7654321)
    println(x)
  }

  test("Get prime numbers till n using iteration") {
    val x = sieveOfEratosthenes(7654321)
    println(x)
  }

  test("Get totient function till n") {
    val x = calculateTotientFunctionTillN(1000000)
    println(x)
  }

  test("Count digits in a number"){
    assertResult(5){
      countDigits(12304)
    }
  }
}
