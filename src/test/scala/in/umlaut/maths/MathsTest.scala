package in.umlaut.maths

import org.scalatest.FunSuite

class MathsTest extends FunSuite {
  test("Should get correct nth fibonacci number") {
    assertResult(1){
      Maths.nThFibonacci(1)
    }
    assertResult(1){
      Maths.nThFibonacci(2)
    }
    assertResult(2){
      Maths.nThFibonacci(3)
    }
    assertResult(3){
      Maths.nThFibonacci(4)
    }
    assertResult(5){
      Maths.nThFibonacci(5)
    }
    assertResult(8){
      Maths.nThFibonacci(6)
    }
    assertResult(6765){
      Maths.nThFibonacci(20)
    }
  }
}
