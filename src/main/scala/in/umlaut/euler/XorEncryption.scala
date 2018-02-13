package in.umlaut.euler

import in.umlaut.utility.Utility._

/**
  * This implements the solution to ProjectEuler problem number 59
  */
object XorEncryption {

  val spaceAscii = 32

  def findKey(encryptedVals: List[Int]): Int =
     encryptedVals.groupBy(identity).mapValues(_.size).maxBy(_._2)._1 ^ spaceAscii

  def decrypt(text: String, keyLength: Int): (Int, String) = {
    val encryptedValues = text.split(",").toList.map(x => x.toInt)
    val subLists = decomposeList(encryptedValues, keyLength)
    val lists = subLists.map(x => (findKey(x), x)).map(x => x._2.map(y => y ^ x._1))
    val output = combineLists(lists)
    (output.sum, output map (item => item.toChar) mkString)
  }
}
