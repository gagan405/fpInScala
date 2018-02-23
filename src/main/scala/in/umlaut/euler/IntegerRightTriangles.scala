package in.umlaut.euler

import in.umlaut.maths.Maths

object IntegerRightTriangles {

  /**
    * Wrapper function to count whatever is needed with the perimeters (max frequency perimeter /
    * number of perimeters occuring only once/ ...)
    * Returns the perimeter with the highest count of integer right triangles with that perimeter
    * where perimeter is less than equal to maxPerimeter
    * @param maxPerimeter
    * @return
    */
  def countTriangles(maxPerimeter:Int): Int = {
    val m = math.sqrt(maxPerimeter/2).toInt
    val triangles = generateTripletsForM(m, maxPerimeter, Map())
    triangles.count(_._2 == 1)
  }

  /**
    * Returns the count of triangles against all possible perimeters under the max perimeter of p
    * @param m
    * @param n
    * @param p
    * @param perimeters
    * @return
    */
  def getTrianglesForGivenSidesWithMaxPerimeter(m: Int, n:Int, p:Int, perimeters: Map[Int, Int]): Map[Int, Int] = {
    def countTrianglesForMNP(m: Int, n:Int, p:Int, k:Int, perimeters: Map[Int, Int]):Map[Int, Int] = {
      val perimeter = k * 2 * m * (m + n)
      if(perimeter <= p){
        if (perimeters isDefinedAt perimeter) {
          countTrianglesForMNP(m, n, p, k + 1, perimeters.updated(perimeter, perimeters(perimeter) + 1))
        } else {
          countTrianglesForMNP(m, n, p, k + 1, perimeters + (perimeter -> 1))
        }
      } else {
        perimeters
      }
    }
    countTrianglesForMNP(m, n, p, 1, perimeters)
  }

  /**
    * Returns frequency of perimeters of triangles for m in the range 1 to m with perimeter less
    * than equal to p
    * What is m ?
    * Refer Euler's formula to gerenate Pythagorean Triplets
    * @param m
    * @param p
    * @param triangles
    * @return
    */
  def generateTripletsForM(m:Int, p:Int, triangles: Map[Int, Int]):Map[Int, Int] = {

    def getPerimetersForGivenMandN(m: Int, n: Int, bound : Int, maxPerimeter:Int, perimeterCounts:Map[Int, Int]): Map[Int, Int] = {
      // to generate primitive triplets m & n have to be relatively co-prime and one of them odd
      if(n <= bound && (2 * m * (m + n) <= p) && (m > n)) {
        if(((m + n) % 2 > 0) && Maths.isRelativelyCoprime(m,n)) {
          val updatedCounts = getTrianglesForGivenSidesWithMaxPerimeter(m, n, p, perimeterCounts)
          getPerimetersForGivenMandN(m, n + 1, bound, maxPerimeter, updatedCounts)
        } else {
          getPerimetersForGivenMandN(m, n + 1, bound, maxPerimeter, perimeterCounts)
        }
      } else {
        perimeterCounts
      }
    }
    if(m == 1){
      triangles
    } else {
      val upperBound = (p - 2 * (m * m))/m
      val countOfPerimeters = getPerimetersForGivenMandN(m,1,upperBound, p, triangles)
      generateTripletsForM(m-1, p, countOfPerimeters)
    }
  }
}
