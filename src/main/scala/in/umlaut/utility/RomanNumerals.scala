package in.umlaut.utility

import scala.collection.immutable.{ListMap, TreeMap}

object RomanNumerals {

  val romanDenominations = Map('I' -> 1, 'V' -> 5, 'X' -> 10, 'L' -> 50, 'C' -> 100, 'D' -> 500, 'M' -> 1000)
  val valueToRomanNumerals = ListMap(romanDenominations.toSeq.sortWith(_._2 > _._2):_*)
  val subtractableDenominationPairs = Map('I' -> Set('X', 'V') , 'X' -> Set('L', 'C'), 'C' -> Set('D', 'M'))
  val singleOccurances = Set('V', 'L', 'D')
  val intermediateLiterals = Map('I' -> 'V', 'X' -> 'L', 'C' -> 'D')

  /**
    * Convert a Roman numeral string to corresponding integer value
    * @param r
    * @return
    */
  def roman2Int(r: String): Int = {
    def evaluatorHelper(r: String, prevChar: Option[Char], sum: Int):Int = {
      if(r.length == 0) {
        sum
      }
      else if (prevChar.isDefined && (subtractableDenominationPairs contains prevChar.get) &&
        (subtractableDenominationPairs(prevChar.get) contains r.charAt(0)))
      {
        evaluatorHelper(r.substring(1), Option.apply(r.charAt(0)), sum + romanDenominations(r.charAt(0)) - 2 * romanDenominations(prevChar.get))
      } else {
        evaluatorHelper(r.substring(1), Option.apply(r.charAt(0)), sum + romanDenominations(r.charAt(0)))
        }
      }
    evaluatorHelper(r, Option.empty, 0)
  }

  /**
    * For a given int, and the previously seen roman char, gets the string equivalent
    * @param q
    * @param nextRomanLiteral
    * @return
    */
  def getNextStringLiteral(q: Int, nextRomanLiteral: Char):String = {
    if (subtractableDenominationPairs contains nextRomanLiteral) {
      val isSubtractable = subtractableDenominationPairs(nextRomanLiteral)
        .find(x => q == (romanDenominations(x) - romanDenominations(nextRomanLiteral)) / romanDenominations(nextRomanLiteral))
      q match {
        case x if isSubtractable.isDefined => nextRomanLiteral.toString + isSubtractable.get
        case x if x == 5 => intermediateLiterals(nextRomanLiteral).toString
        case x if x > 5 => intermediateLiterals(nextRomanLiteral) + nextRomanLiteral.toString * (q - 5)
        case _ => nextRomanLiteral.toString * q
      }
    } else {
      nextRomanLiteral.toString * q
    }
  }

  /**
    * Convert an integer to corresponding roman numeral notation
    * @param n
    * @return
    */
  def int2Roman(n: Int): String = {
    def converter(n: Int, itr:Iterator[(Char, Int)], result: String):String = {
       if(itr.hasNext) {
        val (nextRomanLiteral, nextDigit) = itr.next()
        if (singleOccurances contains nextRomanLiteral){
          converter(n, itr, result)
        } else {
          val (q, r) = (n / nextDigit, n % nextDigit)
          if (q > 0) {
            converter(r, itr, result + getNextStringLiteral(q, nextRomanLiteral))
          } else {
            converter(r, itr, result)
          }
        }
      } else {
          result
      }
    }
    converter(n, valueToRomanNumerals.iterator, "")
  }
}
