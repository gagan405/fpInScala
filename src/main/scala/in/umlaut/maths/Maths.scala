package in.umlaut.maths

object Maths {

  /**
    * Returns the nth fibonacci number
    * Series index starts with n = 1
    *
    * 1, 1, 2, 3, 5 ....
    *
    * Exercise 2.1 of Functional Programming In Scala
    *
    * @param n : nth number in fibonacci series to be generated
    * @return nth number in the fibonacci series
    */
  def nThFibonacci(n: Int):Int = {
    def sumUp(count: Int, currentNum: Int, nextNum: Int, depth: Int): Int =
      if (count == depth) nextNum else sumUp(count + 1, nextNum, currentNum + nextNum, depth)

    if (n == 1 || n == 2) 1 else sumUp(3, 1, 2, n)
  }


  /**
    * Implements Euler's recurrence to compute partitions of a given number.
    *
    * Remove the modulo 10000000 to get the absolute numbers. Change to BigInt to support
    * larger ranges
    *
    * The Map can be removed from the parameters, but that means calculating each of the
    * terms repeatedly.
    *
    * @param n : Number for which partition function P(n) to be calculated
    * @param precomputedVals Map of n -> P(n) as pre computed values. Pass an empty Map first, and
    *                        update the map after every new result.
    * @return
    */
  def getPartitionsOfNGivenPreviousPartitions(n: Int, precomputedVals:Map[Int, Long]): Long = {
    def sumUp(sum:Long, pIdx:Int, sign:Int,n: Int, precomputedVals:Map[Int, Long]): Long = {
      val p1 = (pIdx * (3 * pIdx - 1))/2
      val p2 = ((-1 * pIdx) * ((-3 * pIdx) - 1))/2

      val newSum = sum + ((sign * (if (p1 <= n) precomputedVals(n-p1) else 0)) % 1000000 +
        (sign * (if (p2 <= n) precomputedVals(n-p2) else 0)) % 1000000) % 1000000

      if(p1 >= n && p2 >=n) {
        newSum
      } else {
        sumUp(newSum, pIdx + 1, -1 * sign, n, precomputedVals)
      }
    }
    n match {
      case x if x < 0 => 0
      case x if x == 0 => 1
      case x if x < 4 => x
      case _ => sumUp(0L, 1, 1, n, precomputedVals)
    }
  }

  /**
    * Calculates number of partitions for a given number.
    * Uses Euler's recurrance relation
    * @param n
    * @return
    */
  def getPartitionsOfN(n: Int): Long = {
    def partitionFunHelper(k: Int, n: Int, precomputedVals: Map[Int, Long]): Long = {
      val nxtNum = getPartitionsOfNGivenPreviousPartitions(k, precomputedVals)
      if (k == n) {
        nxtNum
      }
      else {
        partitionFunHelper(k + 1, n, precomputedVals ++ Map(k -> nxtNum))
      }
    }
    partitionFunHelper(0, n, Map())
  }

}
