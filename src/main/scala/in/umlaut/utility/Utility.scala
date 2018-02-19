package in.umlaut.utility

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

}
