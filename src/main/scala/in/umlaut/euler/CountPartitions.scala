package in.umlaut.euler

import in.umlaut.maths.Maths.getPartitionsOfNGivenPreviousPartitions

object CountPartitions {

  /**
    * Implements solution to Project Euler problem 78
    * @return
    */
  def findNWithPartitionValueDivisibleByMillion: Long = {
    def partitionFunHelper(n: Int, precomputedVals: Map[Int, Long]): Long = {
      val nxtNum = getPartitionsOfNGivenPreviousPartitions(n, precomputedVals)
      if (nxtNum % 1000000 == 0) {
        n
      }
      else {
        partitionFunHelper(n + 1, precomputedVals ++ Map(n -> nxtNum))
      }
    }
    partitionFunHelper(0, Map())
  }
}
