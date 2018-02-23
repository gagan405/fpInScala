package in.umlaut.utility

import in.umlaut.maths.Maths._

import scala.collection.mutable

object Utility {
  /**
    * Creates n lists out of a given list by taking elements from this spaced by n-1 index difference
    * i.e., Given a list (1,2,3,4,5,6,7) and n = 2, it will create 2 lists with elements (1,3,5,7) and
    * (2,4,6) respectively
    *
    * @param aList : a List which is to be decomposed
    * @param n : number of resultimng lists
    * @tparam A : type parameter
    * @return
    */
  def decomposeList[A](aList: List[A], n: Int): List[List[A]] = {
    def skip[T](l:List[T], n:Int, offset: Int = 0) =
      l.zipWithIndex.collect {case (e,i) if ((i + 1 + offset) % n) == 0 => e}
    List.tabulate(n)(x => skip(aList, n, n - (x + 1)))
  }

  /**
    * Combines multiple lists to a single list taking one element from each list at a time
    * i.e., Given lists (1,2,3), (4,5,6), (7,8) the resulting list would be
    * (1,4,7,2,5,8,3,6)
    * @param xs list of lists to be combined
    * @tparam A generic type
    * @return list after combining elements of all lists
    */
  def combineLists[A](xs: List[List[A]]): List[A] = {
    def transform[T](xs: List[List[T]]): List[List[T]] = xs.filter(_.nonEmpty) match {
      case Nil => Nil
      case ys: List[List[A]] => ys.map {_.head} :: transform(ys.map {_.tail})
    }
    transform(xs).flatten
  }


  def reduceListOfMapsToMap[K, V](list: List[Map[K, V]], f:(V, V) => V):Map[K, V] = {
    list.flatten.groupBy(_._1).mapValues(_.map(_._2).reduceLeft(f))
  }


  def sortTriplet(triplet: (Int, Int, Int)):(Int, Int, Int) = {
    val high = triplet._1 max triplet._2 max triplet._3
    val low = triplet._1 min triplet._2 min triplet._3
    (high, triplet._1 + triplet._2 + triplet._3 - (high + low), low)
  }

  /**
    * Swaps digits of a number
    * e.g., swapping 1st and 3rd digit of 1234 -> 1432
    * the positions are counted from 1 to n from right to left
    *
    * @param n
    * @param d1
    * @param d2
    * @return
    */

  def swapDigitsOfNumber(n:Int, d1: Int, d2: Int):Int = {
    val lo = d1 min d2
    val hi = d1 max d2
    if (d1 == d2) {
      n
    } else {
      val lowerSegment = n % positivePower(10, lo - 1)
      val lowerPosDigitWeighted = (n - lowerSegment) % positivePower(10, lo)
      val lowerPosDigit = lowerPosDigitWeighted / positivePower(10, lo - 1)
      val midSegment = (n - lowerPosDigitWeighted - lowerSegment) % positivePower(10, hi - 1)
      val upperPosDigitWeighted = (n - midSegment - lowerPosDigitWeighted - lowerSegment) % positivePower(10, hi)
      val upperPosDigit = upperPosDigitWeighted / positivePower(10, hi - 1)
      val upperSegment = n - upperPosDigitWeighted - midSegment - lowerPosDigitWeighted - lowerSegment
      upperSegment + lowerPosDigit * positivePower(10, hi - 1) + midSegment + upperPosDigit * positivePower(10, lo - 1) + lowerSegment
    }
  }

  /**
    * Generates permutations of an integer
    * e.g., 123, 321, 213 ...
    * Yet to figure out how to achieve this in purely functional form
    *
    * Uses Heap's algorithm
    * @param n
    * @return
    */
  def generatePermutations(n:Int):List[Int] = {
    def generate(count: Int, number: Int, soFar: mutable.MutableList[Int]): List[Int] = {
      var num = number
      if (count == 1) {
        soFar += number
      }
      for (x <- 0 until count) {
        generate(count - 1, num, soFar).last
        num = if (count % 2 == 0) swapDigitsOfNumber(num, x + 1, count) else swapDigitsOfNumber(num, 1, count)
      }
      List.empty ++ soFar
    }
    val res = generate(n.toString.length, n, mutable.MutableList())
    res
  }

  /**
    * Shifts and rotates an integer by a given position
    * i.e., 123 -> 312 -> 231 ...
    * @param n
    * @param pos
    * @return
    */
  def shiftRight(n: Int, pos: Int):Int = {
    val shiftVal = n % positivePower(10, pos)
    shiftVal * positivePower(10, n.toString.length - pos) + (n / positivePower(10, pos))
  }

  def generateRotations(n:Int):List[Int] = (1 to n.toString.length) map(i => shiftRight(n, i)) toList

  def int2ListOfDigits(n:Int):List[Int] = Stream.iterate(n)(_ / 10) takeWhile(_ != 0) map(_ % 10) toList

  /**
    * Tests if 2 numbers are permutations of each other
    * @param n
    * @param that
    * @return
    */
  def isPermutationOf(n: Int, that: Int):Boolean = {
    val x = int2ListOfDigits(n).sorted mkString
    val y = int2ListOfDigits(that).sorted.mkString
    x == y
  }

}
