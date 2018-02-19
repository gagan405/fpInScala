package in.umlaut.euler

import in.umlaut.utility.Utility

object IntegerRightTriangles {

  /**
    * Returns the perimeter with the highest count of integer right triangles with that perimeter
    * where perimeter is less than equal to maxPerimeter
    * @param maxPerimeter
    * @return
    */
  def countTriangles(maxPerimeter:Int): (Int, Int) = {
    val m = math.sqrt(maxPerimeter/2).toInt
    val triangles = generateTripletsForM(m, maxPerimeter, Set())
    triangles.groupBy(x => x._1 + x._2 + x._3).mapValues(_.size).maxBy(_._2)
  }

  /**
    * Returns all triangles for given m and n with perimeter less than equal to p
    * @param m
    * @param n
    * @param p
    * @param triangles
    * @return
    */
  def getTrianglesForGivenSidesWithMaxPerimeter(m: Int, n:Int, p:Int, triangles: Set[(Int, Int, Int)]): Set[(Int, Int, Int)] = {
    def countTrianglesForMNKP(m: Int, n:Int, p:Int, k:Int, set: Set[(Int, Int, Int)]):Set[(Int, Int, Int)] = {
      if((k * 2 * m * (m + n)) <= p){
        val sides = Utility.sortTriplet(k * ((m * m) + (n * n)), 2 * k * m * n, k * ((m * m) - (n * n)))
        val newSet = if (set contains sides) set else set + sides
        countTrianglesForMNKP(m, n, p, k + 1, newSet)
      } else {
        set
      }
    }
    countTrianglesForMNKP(m, n, p, 1, triangles)
  }

  /**
    * Returns all triangles for m in the range 1 to m with perimeter less than equal to p
    * What is m ?
    * Refer Euler's formula to gerenate Pythagorean Triplets
    * @param m
    * @param p
    * @param triangles
    * @return
    */
  def generateTripletsForM(m:Int, p:Int, triangles: Set[(Int, Int, Int)]):Set[(Int, Int, Int)] = {
    if(m == 1){
      triangles
    } else {
      val upperBound = (p - 2 * (m * m))/m
      val maps:Set[(Int, Int, Int)] = (1 to upperBound).map(x => {
        if((2 * m * (m + x) <= p) && (m > x)) {
          getTrianglesForGivenSidesWithMaxPerimeter(m,x,p, triangles)
        } else {
          Set[(Int, Int, Int)]()
        }
      }).filter(x => x.nonEmpty).flatten.toSet
      generateTripletsForM(m-1, p, triangles ++ maps)
    }
  }

}
