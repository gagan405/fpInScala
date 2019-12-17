package in.umlaut.euler

import in.umlaut.maths.Maths

object Lattice {

  def latticePointsOnALineWithOneEndAtOrigin(x: Int, y: Int): Int = {
     Maths.gcd(x, y) + 1
  }




}
