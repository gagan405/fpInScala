package in.umlaut.euler

import in.umlaut.maths.Maths.getSquareRootDecimals

object DigitalSumOfSquareRoots {
  /**
    * Solves project euler problem 80
    * @return
    */
  def getSumOfFirstHundredDigits:Int = (1 to 100) map (x => getSquareRootDecimals(x)._2) sum

}
