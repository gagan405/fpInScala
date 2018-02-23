package in.umlaut.euler

import in.umlaut.maths.Maths._
import in.umlaut.utility.Utility

object CircularPrimes {

  /**
    * Gets the circular prime numbers under the limit n
    * Project Euler 35
    * @param n
    * @return
    */
  def getCircularPrimes(n: Int):Int = {
    def isProbableCircularPrime(x: Int):Boolean = {
      if(x < 10) {
        true
      } else {
        (Set(1,3,7,9) contains(x % 10)) && isProbableCircularPrime(x / 10)
      }
    }
    val primes = getPrimesTillN(n).toSet
    val circularPrimes = primes.filter(p => isProbableCircularPrime(p)).filter(p => Utility.generateRotations(p).toSet.subsetOf(primes))
    circularPrimes.size
  }

}
