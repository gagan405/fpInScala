package in.umlaut.euler

import in.umlaut.maths.Maths

/**
  * Holds generic prime number related problems from project euler
  */
object Primes {

  /**
    * Finds the prime number below the given limit which can be written as a
    * sum of consecutive prime numbers with the longest length
    * project euler problem 50
    * @param limit
    * @return
    */
  def consecutivePrimeSum(limit: Int): Int = {
    val primes = Maths getPrimesTillN limit
    val setOfPrimes = primes.toSet
    val prefixSum = primes.scanLeft(0)(_ + _).takeWhile(_ <= limit)
    var length = 0
    var maxPrime = 0

    for (i <- prefixSum.indices) {
      for (j <- 0 until i) {
        val possiblePrime = setOfPrimes.contains(prefixSum(i) - prefixSum(j))
        if (possiblePrime && length < i - j) {
          length = i - j
          maxPrime = prefixSum(i) - prefixSum(j)
        }
      }
    }
    maxPrime
  }

  /**
    * Project euler problem 58
    * @param limit
    * @return
    */

  def spiralPrimes(limit: Double): Int = {

    def getTotalDiagonalElements(n: Int): Int = {
      1 + 4 * n
    }

    def getSideLength(n: Int): Int = {
      2 * n + 1
    }

    def getPercentageOfPrimes(p: Int, n: Int): Double = {
      (p * 100f) / getTotalDiagonalElements(n)
    }

    def getNextDiagCorners(n: Int): List[Int] = {
      val a = 4 * n * n
      val b = 2 * n
      List(
        a - b + 1,
        a + 1,
        a + b + 1
      )
    }

    var primeCount = 0
    var n = 0
    var percentOfPrimes = 100d

    while (percentOfPrimes >= limit) {
      n = n + 1
      primeCount = primeCount + getNextDiagCorners(n).count(Maths.isPrime)
      percentOfPrimes = getPercentageOfPrimes(primeCount, n)
    }

    getSideLength(n)
  }

}
