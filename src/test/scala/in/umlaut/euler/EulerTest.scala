package in.umlaut.euler

import org.scalatest.{FunSuite, Matchers}

class EulerTest  extends FunSuite with Matchers {
  test("Should get me the result"){
    println(DigitalSumOfSquareRoots.getSumOfFirstHundredDigits)
  }
}
