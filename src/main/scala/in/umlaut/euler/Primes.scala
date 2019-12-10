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

}
