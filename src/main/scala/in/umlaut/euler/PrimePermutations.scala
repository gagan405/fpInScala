package in.umlaut.euler

import in.umlaut.maths.Maths._
import in.umlaut.utility.Utility._

object PrimePermutations {

  /**
    * Solves project euler problem 49
    * @param n
    * @param d
    * @return
    */
  def getPrimePermutations(n: Int, d: Int): List[(Int, Int, Int)]= {
    val primes = getPrimesTillN(n).filter(_ > 1000).sorted
    primes.groupBy(int2ListOfDigits(_).sorted.mkString).filter(i => i._2.lengthCompare(d-1) >= 0)
      .map(i =>
        (i._1, uniquePairsOfList(i._2).filter(item => i._2 contains (item._1 + item._2) / 2).map(i => (i._1, (i._1 + i._2) / 2, i._2))))
      .filter(i => i._2.nonEmpty).values.toList.flatten
  }
}
