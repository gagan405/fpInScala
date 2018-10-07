package in.umlaut.strings

import scala.collection.mutable.ListBuffer

object StringUtils {

  def countWords(in: String, tokenizer: String => List[String]):Int =
    tokenizer(in).size

  def countWordsInCamelCase(in: String):Int = {
    in.zipWithIndex.count(x => x._1.isUpper) + 1
  }

  def getWordsFromCamelCase(in: String):List[String] = {
    val words = new ListBuffer[String]()

    var w = ""
    for (c <- in) {
      if (c.isUpper) {
        words += w
        w = "" + c
      } else {
        w += c
      }
    }

    words.toList
  }

}
