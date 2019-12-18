package in.umlaut.euler

import in.umlaut.maths.Maths

// Problem 504
object Lattice {

  val precomputedGcd = Maths.precomputeGcd(101)
  val squares = Maths.precomputeSquares(101)

  def getNumOfLatticePointsOnLine(x: Int, y: Int): Int = {
    precomputedGcd(x)(y) + 1
  }

  def getNumOfQuadsWithSquareNumOfLattice(arr: Array[Int]): Int = {
    val lp = getLatticePointsInsideQuad(arr)
    if (squares(lp)) getNumOfQuads(arr) else 0
  }

  def getNumOfQuads(x: Array[Int]): Int = {
    var configs = 0
    // 1111, 2222
    if ((x(0) == x(1)) && (x(1) == x(2)) && (x(2) == x(3))) {
      configs = 1
    } else { // 1212
      if ((x(0) != x(1)) && (x(2) == x(0)) && (x(3) == x(1))) {
        configs = 2
      } else if (((x(0) == x(1)) && (x(2) != x(1)) && (x(2) != x(3))) || // 1123
        ((x(0) != x(1)) && (x(2) != x(1)) && (x(2) == x(3))) || // 1233
        ((x(0) != x(1)) && (x(2) == x(1)) && (x(3) != x(2))) || // 1223
        ((x(0) != x(1)) && (x(2) != x(1)) && (x(2) != x(0)) && (x(3) != x(0)) && (x(3) != x(1)))) { // 1234
        configs = 8
      }
      else if (((x(0) != x(1)) && (x(2) == x(0)) && (x(3) != x(1))) || // 1213
        ((x(0) != x(1)) && (x(2) != x(1)) && (x(2) != x(0)) && (x(3) == x(1))) || // 1232
        ((x(0) == x(1)) && (x(2) != x(1)) && (x(2) == x(3))) || // 1122
        ((x(0) != x(1)) && (x(2) == x(1)) && (x(3) == x(2))) || // 1222
        ((x(0) == x(1)) && (x(2) == x(1)))) { // 1112
        configs = 4
      }
    }
    configs
  }

  def getLatticePointsInsideQuad(arr: Array[Int]): Int = (getArea(arr) + 1 - getBoundaryLatticePoints(arr) / 2).toInt

  def getArea(arr: Array[Int]): Double = 0.5 * (arr(0) + arr(2)) * (arr(1) + arr(3))

  def getBoundaryLatticePoints(arr: Array[Int]): Int = {
      getNumOfLatticePointsOnLine(arr(0), arr(1)) + getNumOfLatticePointsOnLine(arr(1), arr(2)) +
        getNumOfLatticePointsOnLine(arr(2), arr(3)) + getNumOfLatticePointsOnLine(arr(3), arr(0)) - 4
  }


  def countQuadrilateralsWithSquareNumberOfLatticePoints(limit: Int): Int = {
    var quadsWithSquareNumberOfLatticePoints = 0
    for(a <- 1 to limit) {
      for (b <- a to limit) {
        for (c <- b to limit) {
          for (d <- c to limit) {
            quadsWithSquareNumberOfLatticePoints += getNumOfQuadsWithSquareNumOfLattice(Array[Int](a, b, c, d))
            if ((a != b) && (b == c) && (c != d)) { // 1223 -> 1232
              quadsWithSquareNumberOfLatticePoints += getNumOfQuadsWithSquareNumOfLattice(Array[Int](a, b, d, c))
            } else if (b != c) {
              // 1233 -> 1323
              // 1234 -> 1324
              // 1123 -> 1213
              quadsWithSquareNumberOfLatticePoints += getNumOfQuadsWithSquareNumOfLattice(Array[Int](a, c, b, d))
              if ((a < b) && (c < d)) { // 1234 -> 1243
                quadsWithSquareNumberOfLatticePoints += getNumOfQuadsWithSquareNumOfLattice(Array[Int](a, b, d, c))
              }
            }
          }
        }
      }
    }
    quadsWithSquareNumberOfLatticePoints
  }

}


