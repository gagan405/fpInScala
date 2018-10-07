package in.umlaut.strings

import in.umlaut.strings.StringUtils._
import org.scalatest.{FunSuite, Matchers}

class StringUtilsTest extends FunSuite with Matchers {

  test("Count words in camel case") {
    countWordsInCamelCase("thisIsValidCamelCaseString") should equal(6)
  }

  test("Get words in camel case") {
    val words = getWordsFromCamelCase("thisIsValidCamelCaseString")
    println(words)
  }

}
