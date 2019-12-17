package in.umlaut.euler

import in.umlaut.maths.Maths
import in.umlaut.maths.Maths._
import in.umlaut.utility.Utility._

import util.control.Breaks._

object Digits {

  def getPanDigitalNumber():Int = {
    def panDigitalMultiplier(x:Int):Option[Int] = {
      val v = List.range(1, 10).map(i => (i * x).toString).scanLeft("")(_ + _).takeWhile(_.length < 10).last.toInt
      if(isPanDigitalNumber(v)){
        Option.apply(v)
      } else {
        Option.empty
      }
    }
    (9111 to 9999).map(i => panDigitalMultiplier(i)).filter(_.isDefined).map(_.get).toList.max
  }


  def getLargestPandigitalPrime():Int = {
    sieveOfEratosthenes(7654321)
      .filter(_ > 1000000)
      .filter(i => isPanDigitalNumber(i, countDigits(i))) max
  }

  /**
    * Project Euler problem 63
    * @return
    */

  def countNDigitNumbersWhichAreAlsoNthPowers(): Int = {
    var count = 0
    var m = 1

    for (n <- 1 to 9) {
      var d = Maths.countDigitsInPower(n, m)
      while (d >= m) {
        if (d == m) {
          println(s"$n to the power of $m is of $d digits")
          count += 1
        }
        m += 1
        d = Maths.countDigitsInPower(n, m)
      }
      m = 1
    }
    count
  }

  /**
    * Project euler problem 56
    * @return
    */
  def numberWithHighestSumOfDigits(): Int = {
    var max = 0

    for (a <- 1 until 100) {
      for (b <- 1 until 100) {
        val x = BigInt(a).pow(b)
        val y = Maths.getSumOfDigits(x)
        if( y > max ) {
          max = y
        }
      }
    }
    max
  }


  /**
    * Project Euler problem 97
    * @return
    */
  def lastTenDigitsOfNonMersennePrime(): BigInt = {
    val digits = Maths.getLastNDigitsOfPower(2, 7830457, 10)
    val q = BigInt(10).pow(10)
    (BigInt(28433) * digits + 1) % q
  }

  /**
    * Project euler problem 206
    * @return
    */

  def concealedSquare(): Long = {
    val min = 101010102L
    val max = 138902662L

    var res = 1L

    breakable {
      for (x <- min to max) {
        if (x % 10 == 3 || x % 10 == 7) {
          val z = BigInt(x).pow(2).toString
          if (z.matches("1[0-9]2[0-9]3[0-9]4[0-9]5[0-9]6[0-9]7[0-9]8[0-9]9")) {
            res = x.toLong
            break
          }
        }
      }
    }

    res * 10
  }

}
