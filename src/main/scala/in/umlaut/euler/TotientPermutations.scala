package in.umlaut.euler

import in.umlaut.maths.Maths._
import in.umlaut.utility.Utility._

object TotientPermutations {

  /**
    * Brute force implementation of Project Euler problem 70
    * A better and faster way is given here: http://www.mathblog.dk/project-euler-70-investigate-values-of-n-for-which-%CF%86n-is-a-permutation-of-n/
    * @param n
    * @return
    */
  def getTotientFunctionsWithPermutations(n: Int): (Int, Int) = {
    val totients = calculateTotientFunctionTillN(n)
    val permutationTotients = totients.zipWithIndex.filter(item => item._1 > 9 && isPermutationOf(item._1, item._2))
    permutationTotients.minBy(i => i._2.toFloat/i._1)
  }

}
