package in.umlaut.euler

import in.umlaut.maths.Hungarian

object HungarianMax {

  /**
    * find max sum in matrix each row col only once
    * Project euler problem 345
    */

  def hungarianMax(matrix: Array[Array[Int]]): Int = {
    for(i <- matrix.indices){
      for (j <- matrix.indices){
        matrix(i)(j) *= -1
      }
    }
    val res = Hungarian.compute(matrix)
    var sum = 0
    res.foreach(i => {
      println(s"${(i._1, i._2)}")
      sum += matrix(i._1)(i._2) * -1
    })
    sum
  }

}
