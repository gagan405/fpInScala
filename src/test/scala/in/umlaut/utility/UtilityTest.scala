package in.umlaut.utility

import org.scalatest.{FunSuite, Matchers}

class UtilityTest extends FunSuite with Matchers {

  test("Should split a list to two lists with alternate indexed elements"){
    val a = List.range(1, 10)
    val res = Utility.decomposeList(a, 2)
    assert(res.lengthCompare(2) == 0)
    res.head should equal (List(1,3,5,7,9))
    res(1) should equal (List(2,4,6,8))
  }

  test("Should split a list to three lists with correct indexed elements"){
    val a = List.range(1, 10)
    val res = Utility.decomposeList(a, 3)
    assert(res.lengthCompare(3) == 0)
    res.head should equal (List(1,4,7))
    res(1) should equal (List(2,5,8))
    res(2) should equal (List(3,6,9))
  }

  test("Should split a list to four lists with correct indexed elements"){
    val a = List.range(1, 15)
    val res = Utility.decomposeList(a, 4)
    assert(res.lengthCompare(4) == 0)
    res.head should equal (List(1,5,9,13))
    res(1) should equal (List(2,6,10,14))
    res(2) should equal (List(3,7,11))
  }

  test("Should combine lists to a single list"){
    val l1 = List(1,2,3)
    val l2 = List(4,5,6)
    val l3 = List(7)
    val res = Utility.combineLists(List(l1,l2,l3))
    assert(res.lengthCompare(7) == 0)
    res should equal (List(1, 4, 7, 2, 5, 3, 6))
  }

}
