package in.umlaut.maths

import in.umlaut.maths.Maths.getQuotientAndReminder

import scala.collection.mutable.Map

class ModCache(mod: Long) {

  private val bitMask = 1L << 62
  private val inverseModOfNine = Maths.modInverse(9, mod)
  private var cache = Map[Long, Long]()
  private var cache10s = Map[Long, Long]()
  private var cache9s = Map[Long, Long]()
  private var cache1s = Map[Long, Long]()

  private var sortedSet9s = new java.util.TreeSet[Int]()
  private var sortedSet1s = new java.util.TreeSet[Int]()
  private var sortedSet10s = new java.util.TreeSet[Long]()

  private var maxKnown10 = 1L
  private var maxKnown9 = 1
  private var maxKnown1 = 1

  cache9s.put(maxKnown9, 9)
  cache1s.put(maxKnown1, 1)
  cache10s.put(maxKnown10, 10)

  sortedSet9s.add(maxKnown9)
  sortedSet10s.add(maxKnown10)
  sortedSet1s.add(maxKnown1)

  private def computeMod(k: Long): Long = {
    k % mod
  }

  def get(key: Long): Long = {
    cache.getOrElseUpdate(key, computeMod(key))
  }

  def put(key: Long, value: Long): Unit = {
    cache.put(key, value)
  }

  def getModOfAllNines(length: Int): Long = {
    val known = sortedSet9s.floor(length)
    val res = getModOfAll9sFromCache(length, known, cache9s.get(known).get)
    maxKnown9 = maxKnown9 max length
    sortedSet9s.add(maxKnown9)
    sortedSet9s.add(length)
    res
  }

  def getModOfAllOnes(length: Int): Long = {
    val known = sortedSet1s.floor(length)
    val res = getModOfAll1sFromCache(length, known, cache1s.get(known).get)
    maxKnown1 = maxKnown1 max length
    sortedSet1s.add(maxKnown1)
    sortedSet1s.add(length)
    res
  }

  def getModOfAllTens(power: Long): Long = {
    if(power == 0) {
      return 1L
    }
    val known = sortedSet10s.floor(power)
    val res = getModOfAll10sFromCache(power, known, cache10s.get(known).get)
    maxKnown10 = maxKnown10 max power
    sortedSet10s.add(maxKnown10)
    sortedSet10s.add(power)
    res
  }

  // Gets mod of the GP series 1, 10, 100 .....
  def getModOfGp(terms: Long) : Long = {
    cache1s.getOrElseUpdate(terms, (((getModOfTenthPower(terms) - 1) % mod) * inverseModOfNine) % mod)
  }

  def getModOfAll9sFromCache(length: Int, knownLength: Int, knownMod: Long): Long = {
    cache9s.getOrElseUpdate(length, getModOfAll9s(length, knownLength, knownMod))
  }

  def getModOfAll1sFromCache(length: Int, knownLength: Int, knownMod: Long): Long = {
    cache1s.getOrElseUpdate(length, getModOfAll1s(length, knownLength, knownMod))
  }

  def getModOfAll10sFromCache(power: Long, knownLength: Long, knownMod: Long): Long = {
    cache10s.getOrElseUpdate(power, getModOfTenthPower(power, knownLength, knownMod))
  }

  def getModOfAll1s(length: Int, knownLength: Int, knownMod: Long): Long = {
    // it is assumed that mod is >= 1000000007 -> which is more than 10 to power 9
    var result = knownMod
    val lenDiff = length - knownLength

    if (lenDiff > 9) {
      val (q, r) = getQuotientAndReminder(lenDiff, 9)
      for(i <- 1 to q.toInt) {
        result = (result % mod * 1000000000) % mod
        result = (result + 111111111) % mod
      }

      // read this value from a table
      if(r != 0) {
        result = (result * getModOfAllTens(r.toInt)) % mod
        result = (result + getModOfAllOnes(r.toInt)) % mod
      }
    } else {
      result = ( result *(Maths.tenthPower(lenDiff).toLong)) % mod
      result = (result + (BigInt("1" * lenDiff).toLong)) % mod
    }
    result
  }

  // unoptimized dirty way
  def getModOfAll9s(length: Int, knownLength: Int, knownMod: Long): Long = {
    // it is assumed that mod is >= 1000000007 -> which is more than 10 to power 9
    var result = knownMod
    val lenDiff = length - knownLength

    if (lenDiff > 9) {
      val (q, r) = getQuotientAndReminder(lenDiff, 9)
      for(i <- 1 to q.toInt) {
        result = (result % mod * 1000000000) % mod
        result = (result + 999999999) % mod
      }

      // read this value from a table
      if(r != 0) {
        result = (result * getModOfAllTens(r.toInt)) % mod
        result = (result + getModOfAllNines(r.toInt)) % mod
      }
    } else {
      result = ( result *(Maths.tenthPower(lenDiff).toLong)) % mod
      result = (result + (BigInt("9" * lenDiff).toLong)) % mod
    }
    result
  }

  def getModOfTenthPower(power: Long, knownPower: Long, knownMod: Long): Long = {
    // it is assumed that mod is >= 1000000007 -> which is more than 10 to power 9
    var result = 1L
    val diff = power - knownPower
    if (diff > 9) {
      val (q, r) = getQuotientAndReminder(power, knownPower)
      for(i <- 1 to q.toInt) {
        result = (result % mod * knownMod % mod) % mod
      }
      // read this value from a table
      if (r != 0) {
        result = (result * getModOfAllTens(r)) % mod
      }
    } else {
      result = (knownMod * Maths.tenthPower(diff.toInt).toLong) % mod
    }
    result
  }

  def getPosOfFirstSetBitFromHigherEnd(power: Long, bitPos: Long = 0): Long = {
    var bitShift = bitPos
    var mask = bitMask >> bitShift

    var pp = power & mask

    while (pp == 0) {
      bitShift += 1
      mask = mask >> 1
      pp = power & mask
    }
    62 - bitShift
  }

  def getModOfTenthPower(power: Long, bitPos: Int = 0): Long = {
    if (cache10s.contains(power)) {
      cache10s(power)
    } else {
      val pp = getPosOfFirstSetBitFromHigherEnd(power, bitPos)
      val mask = bitMask >> (62 - pp)
      val partialProduct = power & mask

      var result = 0L

      if (partialProduct == power) {
        result = (getModOfTenthPower(partialProduct >> 1) % mod * getModOfTenthPower(partialProduct >> 1) % mod) % mod
      } else {
        result = (getModOfTenthPower(partialProduct) % mod * getModOfTenthPower(power - partialProduct) % mod) % mod
      }
      cache10s.put(power, result)
      result
    }
  }

}
