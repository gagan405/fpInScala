package in.umlaut.euler

import in.umlaut.maths.{Maths, ModCache}
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

  def smallestNumberWithDigitSum(s: Int): Int = {
    val numberOf9s = s / 9
    if (numberOf9s < 1) {
      s % 9
    } else {
      val offset = ("9" * numberOf9s)
      ((s % 9) + offset).toInt
    }
  }

  def sumOfDigitSums(limit: Int): Int = {
    var sum = 0
    for(i <- 1 to limit) {
      val x = smallestNumberWithDigitSum(i)
      sum += x
    }
    sum
  }

  def sumOfDigitSumsV1(limit: Long, mod: Int): BigInt = {
    if (limit < 10) {
      return sumOfDigitSums(limit.toInt)
    }
    val q = limit / 9
    val r = limit % 9

    var sum = BigInt(0)
    for (i <- 2L to r + 1) {
      sum += BigInt(i)
    }

    sum = ((sum * Maths.tenthPower(q.toInt)) - r) % mod

    if (q > 1) {
      sum = ((sum + (540 * (BigInt("1" * (q.toInt - 1)) % mod)) % mod) % mod - ((q - 1) * 9) % mod) % mod
    }
    (sum + 45) % mod
  }

  def sumOfDigitSumsV2(limit: Long, knownIdx: Long, knownVal: Long, mod: Int, modCache: ModCache): Long = {
    if(limit < 20) {
      return sumOfDigitSumsV1(limit, mod).toInt % mod
    }

    val (q, r) = getQuotientAndReminder(limit, 9L)
    val (kq, kr) = getQuotientAndReminder(knownIdx, 9L)

    var sum = knownVal

    var off = sumRange(kr + 2, 10L)
    off = ((off * modCache.getModOfAllTens(kq)) % mod - (9 - kr)) % mod

    var offset = sumRange(2L, r + 1)
    offset = ((offset * modCache.getModOfAllTens(q)) % mod - r) % mod

    val intervalStart = modCache.getModOfAllTens(kq + 1)
    val terms = q - (kq + 1)
    if (terms > 0) {
      val sumGp = modCache.getModOfGp(terms)
      var sumOfIntervals = ((((54 * intervalStart) % mod) * sumGp) % mod - (terms * 9) % mod) % mod
      if (sumOfIntervals < 0) {
        sumOfIntervals = sumOfIntervals + mod
      }
      sum = (sum + sumOfIntervals) % mod
    }

    ((sum % mod) + (offset % mod + off % mod) % mod) % mod
  }

  /**
   * Implements a closed form formula for the sum S(k)
   *
   * k=(q∗9)+r , where 0≤r<9.
   *
   * S(k)=(10^q)(5+(r+1)(r+2)2)−(k+6)
   *
   * Seen from Project euler discussions
   *
   * @param limit
   * @param mod
   * @param modCache
   * @return
   */
  def sumOfDigitSumsV3(limit: Long, mod: Long, modCache: ModCache): Long = {
    val (q, r) = getQuotientAndReminder(limit, 9L)
    val offset = 5 + ((r + 1) * (r + 2))/2
    val res = ((modCache.getModOfAllTens(q) * offset) % mod - (limit + 6)) % mod
    if (res < 0) {
      res + mod
    } else {
      res
    }
  }

  /**
    * Problem 684 project euler
    * @param i
    * @return
    */
  def sumOfDigitSumsModulo(i: Int): BigInt = {
    var a = 0L
    var b = 1L

    var sum = BigInt(0)
    val mod = 1000000007

    val modCache = new ModCache(mod)

    var c = 1L
    var lastValue = 0L

    var last = 0L
    for (count <- 20 to i) {
      c = a + b // next fibonacci number
      lastValue = sumOfDigitSumsV2(c, b, lastValue, mod, modCache)
      last = sumOfDigitSumsV3(c, mod, modCache)
      assert(last == lastValue)
      sum = (sum + lastValue) % mod
      a = b
      b = c
    }
    sum
  }

}
