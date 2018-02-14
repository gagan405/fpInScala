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
}
