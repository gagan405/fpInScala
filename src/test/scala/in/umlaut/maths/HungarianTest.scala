package in.umlaut.maths

import org.scalatest.{FunSuite, Matchers}

import scala.collection.mutable.Set

class HungarianTest extends FunSuite with Matchers {

  test("Test hungarian algorithm get min lines") {
    val e = Array(
      Array(250, 0, 0, 0),
      Array(0, 600, 350, 11),
      Array(20, 40, 0, 10),
      Array(0, 65, 40, 10)
    )

    val f = Hungarian.getMinLines(e)._1

    f.foreach(l => println(l.lineType + " " + l.lineIndex))
  }

  test("Should find optimal assignment") {
    var e = Array(
      Array(0, 0, 100),
      Array(50, 100, 0),
      Array(0, 10, 50)
    )

    var f = Hungarian.findOptimalAssignment(e);

    f.foreach(i => println(s"${(i._1, i._2)}"))

    e = Array(
      Array(7, 8, 0, 2),
      Array(50, 0, 10, 11),
      Array(0, 10, 0, 60),
      Array(0,1,96,0)
    )

    f = Hungarian.findOptimalAssignment(e);

    f.foreach(i => println(s"${(i._1, i._2)}"))

    println("------------------------------")

    e = Array(
      Array(7, 0, 5, 0),
      Array(0, 20, 0, 0),
      Array(30, 0, 0, 60),
      Array(0,1,96,20)
    )

    f = Hungarian.findOptimalAssignment(e);


    f.foreach(i => println(s"${(i._1, i._2)}"))

    println("------------------------------")

    e = Array(
      Array(7, 0, 0),
      Array(0, 0, 11),
      Array(0, 10, 3)
    )

    f = Hungarian.findOptimalAssignment(e);

    f.foreach(i => println(s"${(i._1, i._2)}"))
  }

  test("Should get the minimum uncovered value") {
    var mat = Array(
      Array(15, 0, 0, 0),
      Array(0, 50, 20, 25),
      Array(35, 5, 0, 10),
      Array(0, 65, 50, 65)
    )

    var coveredRows = Set[Int](0)
    var coveredCols = Set[Int](0, 2)

    assert(Hungarian.getMinimumUncovered(mat, coveredRows, coveredCols) == 5)

    mat = Array(
      Array(20, 0, 5, 0),
      Array(0, 45, 20, 20),
      Array(35, 0, 0, 5),
      Array(0, 60, 50, 60)
    )

    coveredRows = Set[Int](0, 2)
    coveredCols = Set[Int](0)

    assert(Hungarian.getMinimumUncovered(mat, coveredRows, coveredCols) == 20)
  }

  test("Should compute the optimal solution"){

    var mat = Array(
      Array(90, 75, 75, 80),
      Array(35, 85, 55, 65),
      Array(125, 95, 90, 105),
      Array(45, 110, 95, 115)
    )

    var res = Hungarian.compute(mat)
    var sum = 0
    res.foreach(i => {
      println(s"${(i._1, i._2)}")
      sum += mat(i._1)(i._2)
    })

    assert(sum == 275)

    println("--------------------")

    mat = Array(
      Array(82, 83, 69, 92),
      Array(77, 37, 49, 92),
      Array(11, 69, 5, 86),
      Array(8, 9, 98, 23)
    )

    res = Hungarian.compute(mat)
    sum = 0
    res.foreach(i => {
      println(s"${(i._1, i._2)}")
      sum += mat(i._1)(i._2)
    })

    assert(sum == 140)

    /*println("--------------------")

    mat = Array(
      Array(1, 2, 3, 4),
      Array(2, 4, 6, 8),
      Array(3, 6, 9, 12),
      Array(4, 8, 12, 16)
    )

    res = Hungarian.compute(mat)
    sum = 0
    res.foreach(i => {
      println(s"${(i._1, i._2)}")
      sum += mat(i._1)(i._2)
    })

    assert(sum == 20)
*/
    println("--------------------")
    mat = Array(
      Array(5,	70,	35,	62,	51,	43),
      Array(47,	26,	36,	69,	45,	85),
      Array(17,	30,	49,	88,	42,	39),
      Array(3,	43,	9,	33,	20,	69),
      Array(36,	72,	45,	59,	28,	4),
      Array(31,	79,	9,	7,	39,	42)
    )

    res = Hungarian.compute(mat)
    sum = 0
    res.foreach(i => {
      println(s"${(i._1, i._2)}")
      sum += mat(i._1)(i._2)
    })

    assert(sum == 93)

    println("--------------------")

  }

  test("Test hungarian algorithm get min lines in the new approach") {
    // wikipedia example
    var e = Array(
      Array(0, 1, 2, 3),
      Array(1, 2, 3, 0),
      Array(0, 1, 2, 3),
      Array(1, 0, 0, 1)
    )

    var f = Hungarian.getCoveringLines(e)._1

    f.foreach(l => println(l.lineType + " " + l.lineIndex))
    assert(f.length == 3)

    e = Array(
      Array(0, 1, 0, 0, 5),
      Array(1, 0, 3, 4, 5),
      Array(7, 0, 0, 4, 5),
      Array(9, 0, 3, 4, 5),
      Array(3, 0, 3, 4, 5)
    )

    f = Hungarian.getCoveringLines(e)._1

    f.foreach(l => println(l.lineType + " " + l.lineIndex))
    assert(f.length == 3)

    e = Array(
      Array(13, 0, 7, 0),
      Array(0, 80, 12, 40),
      Array(6, 0, 0, 66),
      Array(0, 1, 90, 90)
    )

    f = Hungarian.getCoveringLines(e)._1

    f.foreach(l => println(l.lineType + " " + l.lineIndex))
    assert(f.length == 3)

    // The method suggested in https://stackoverflow.com/a/14795379/564503
    // doesnt work for this
    e = Array(
      Array(0, 57, 20, 49, 21, 30),
      Array(29, 0, 8, 43, 2, 59),
      Array(0, 5, 22, 63, 0, 14),
      Array(8, 40, 4, 30, 0, 66),
      Array(40, 68, 39, 55, 7, 0),
      Array(32, 72, 0, 0, 15, 35)
    )

    f = Hungarian.getCoveringLines(e)._1

    f.foreach(l => println(l.lineType + " " + l.lineIndex))
    assert(f.length == 5)


    e = Array(
      Array(0,	53,	16,	45,	21,	26),
      Array(33,	0,	8,	43,	6,	59),
      Array(0,	1,	18,	59,	0,	10),
      Array(8,	36,	0,	26,	0,	62),
      Array(44,	68,	39,	55,	11,	0),
      Array(36,	72,	0,	0,	19,	35)
    )

    f = Hungarian.getCoveringLines(e)._1

    f.foreach(l => println(l.lineType + " " + l.lineIndex))
    assert(f.length == 6)

    // wikipedia algo doesnt work for this

    e = Array(
      Array(0, 0, 0, 0),
      Array(0, 1,	2, 3),
      Array(0, 2,	4, 6),
      Array(0, 3,	6, 9)
    )

    f = Hungarian.getCoveringLines(e)._1

    f.foreach(l => println(l.lineType + " " + l.lineIndex))
    //assert(f.length == 6)


    e = Array(
      Array(0, 0, 0, 0),
      Array(1, 2, 3, 0),
      Array(2, 4, 6, 0),
      Array(3, 6, 9, 0)
    )

    f = Hungarian.getCoveringLines(e)._1

    f.foreach(l => println(l.lineType + " " + l.lineIndex))
    assert(f.length == 6)
  }

}
