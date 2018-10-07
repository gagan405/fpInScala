package in.umlaut.euler

import in.umlaut.maths.Maths._
import in.umlaut.utility.Utility._

object PanDigital {

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
    sieveOfEratosthenes(7654321).filter(_ > 1000000).filter(i => isPanDigitalNumber(i, countDigits(i))) max
  }

}
