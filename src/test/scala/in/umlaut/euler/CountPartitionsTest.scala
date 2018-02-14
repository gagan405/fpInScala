package in.umlaut.euler

import org.scalatest.{FunSuite, Matchers}

class CountPartitionsTest extends FunSuite with Matchers {
  test("Should get me the result"){
    println(CountPartitions.findNWithPartitionValueDivisibleByMillion)
  }
}
