package in.umlaut.maths

object Maths {

  /**
    * Returns the nth fibonacci number
    * Series index starts with n = 1
    *
    * 1, 1, 2, 3, 5 ....
    *
    * Exercise 2.1 of Functional Programming In Scala
    *
    * @param n : nth number in fibonacci series to be generated
    * @return nth number in the fibonacci series
    */
  def nThFibonacci(n : Int):Int = {
    def sumUp(count: Int, currentNum: Int, nextNum: Int, depth: Int): Int =
      if (count == depth) nextNum else sumUp(count + 1, nextNum, currentNum + nextNum, depth)

    if (n == 1 || n == 2) 1 else sumUp(3, 1, 2, n)
  }

}
