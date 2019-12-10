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

  test("Should combine list of Maps to a single map"){
    val myList =  List(Map(1 -> 1), Map(2 -> 2), Map(2 -> 7))
    val res = Utility.reduceListOfMapsToMap(myList, (x:Int, y:Int) => {x * y})
    assert(res.keySet.size == 2)
    assert(res(1) == 1)
    assert(res(2) == 14)
  }

  test("Should correctly swap the digits"){
    val res = Utility.swapDigitsOfNumber(568977, 3, 6)
    assert(res == 968577)
  }

  test("Should correctly generate all permutations"){
    val res = Utility.generatePermutations(321)
    println(res)
  }

  test("Should correctly search items in sorted list") {
    val list = List(10,11,12,13,14)
    assert(Utility.binarySearch(list, 11).get == 1)
    assert(Utility.binarySearch(list, 14).get == 4)
    assert(Utility.binarySearch(list, 9).isEmpty)
  }

}
