package in.umlaut.maths

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Set}

object Hungarian {

  sealed trait LineType
  case object HORIZONTAL extends LineType
  case object VERTICAL extends LineType
  case object NONE extends LineType

  case class Line(lineType: LineType, lineIndex: Int) {
    val isHorizontal: Boolean = lineType == HORIZONTAL
  }

    // step 1
    def subtractMinFromRows(matrix: Array[Array[Int]]): Array[Array[Int]] = {
      for (row <- matrix.indices) {
        val min = matrix(row).min
        if (min != 0) {
          for (col <- matrix(row).indices) {
            matrix(row)(col) = matrix(row)(col) - min
          }
        }
      }
      matrix
    }

  // step 2
  def subtractMinFromCols(matrix: Array[Array[Int]]): Array[Array[Int]] = {
    for (col <- matrix.indices) {
      var minimum = Int.MaxValue
      for (row <- matrix.indices.indices) {
        minimum = matrix(row)(col) min minimum
      }
      if(minimum != 0) {
        for (row <- matrix.indices.indices) {
          matrix(row)(col) = matrix(row)(col) - minimum
        }
      }
    }
    matrix
  }

  def isAllZero(zerosInArr: Array[Int]): Boolean = {
    !zerosInArr.exists(i => i != 0)
  }

  def isAllNegativeOrZero(zerosInArr: Array[Int]): Boolean = {
    !zerosInArr.exists(i => i > 0)
  }

  def printMatrix(matrix: Array[Array[Int]]):Unit = {
    println("---------------------------------------------------------------------")
    for (i <- matrix.indices){
      for (j <- matrix.indices){
        print(matrix(i)(j) + " ")
      }
      println
    }
  }

  // step 3 -- buggy, implements the algo suggested at https://stackoverflow.com/a/14795379/564503
  def getMinLines(matrix: Array[Array[Int]]): (List[Line], Set[Int], Set[Int]) = {
    printMatrix(matrix)
    var result = new ListBuffer[Line]()
    val (zerosInRows, zerosInCols) = getZerosInRowsAndCols(matrix)
    var (coveredRows, coveredCols) = (Set[Int](), Set[Int]())

    var lastInsertedLineType: LineType = NONE
    while(!isAllZero(zerosInRows) && !isAllZero(zerosInCols)) {
      val lineWithMostZeros = getLineWithMostZeros(zerosInRows, zerosInCols, lastInsertedLineType)

      if (lineWithMostZeros.isHorizontal) {
        zerosInRows(lineWithMostZeros.lineIndex) = 0
        coveredRows += lineWithMostZeros.lineIndex
      } else {
        zerosInCols(lineWithMostZeros.lineIndex) = 0
        coveredCols += lineWithMostZeros.lineIndex
      }

      updateZeros(matrix, zerosInRows, zerosInCols, lineWithMostZeros)

      result += lineWithMostZeros
      lastInsertedLineType = lineWithMostZeros.lineType
    }
    (result.toList, coveredRows, coveredCols)
  }

  def getLineWithMostZeros(zerosInRows: Array[Int], zerosInCols: Array[Int], lastInsertedLineType: LineType): Line = {
    var max = -1
    var lineWithMostZeros = Line(NONE, -1)
    for(i <- zerosInRows.indices) {
      if (zerosInRows(i) > max || (zerosInRows(i) == max && (lastInsertedLineType == HORIZONTAL))) {
        lineWithMostZeros = Line(HORIZONTAL, i)
        max = zerosInRows(i)
      }
    }

    for(i <- zerosInCols.indices) {
      if (zerosInCols(i) > max || (zerosInCols(i) == max && (lastInsertedLineType == VERTICAL))) {
        lineWithMostZeros = Line(VERTICAL, i)
        max = zerosInCols(i)
      }
    }
    lineWithMostZeros
  }

  def getLineWithLeastZeros(zerosInRows: Array[Int], zerosInCols: Array[Int]): Line = {
    var min = Int.MaxValue
    var lineWithLeastZeros = Line(NONE, -1)
    for(i <- zerosInRows.indices) {
      if ((zerosInRows(i) < min) && zerosInRows(i) > 0) {
        lineWithLeastZeros = Line(HORIZONTAL, i)
        min = zerosInRows(i)
      }
    }

    for(i <- zerosInCols.indices) {
      if ((zerosInCols(i) < min) && (zerosInCols(i) > 0)) {
        lineWithLeastZeros = Line(VERTICAL, i)
        min = zerosInCols(i)
      }
    }

    lineWithLeastZeros
  }

  def getFirstZeroInArray(arr: Array[Int], crossedZeros: mutable.Set[Int]): Int = {
    arr.zipWithIndex.indexWhere(i => i._1 == 0 && !crossedZeros.contains(i._2))
  }

  // Step 3 -- implements algorithm as suggested in wikipedia
  def getCoveringLines(matrix: Array[Array[Int]]): (List[Line], Set[Int], Set[Int]) = {
    val (zerosInRows, zerosInCols) = getZerosInRowsAndCols(matrix)

    val (assignedRows, assignedCols) = assignRows(matrix, zerosInRows, zerosInCols)

    val (markedRows, markedCols) = markRowAndCols(matrix, assignedRows, assignedCols)
    var (coveredRows, coveredCols) = (Set[Int](), Set[Int]())

    var lines = new ListBuffer[Line]()
    for (i <- markedRows.indices){
      if(!markedRows(i)) {
        val line = Line(HORIZONTAL, i)
        lines += line
        coveredRows += i
      }

      if(markedCols(i)) {
        val line = Line(VERTICAL, i)
        lines += line
        coveredCols += i
      }
    }

    (lines.toList, coveredRows, coveredCols)
  }

  private def markRowAndCols(matrix: Array[Array[Int]],
                             assignedRows: Array[Boolean],
                             assignedCols: Array[Int]):(Array[Boolean], Array[Boolean]) = {
    val markedRows = Array.ofDim[Boolean](matrix.length)
    val markedCols = Array.ofDim[Boolean](matrix.length)

    def recursivelyMarkRowsAndCols(markedRows: Array[Boolean],
                                   markedCols: Array[Boolean],
                                   lastMarkedRow: Boolean,
                                   index: Int): Unit = {
      if(lastMarkedRow) {
        for (j <- matrix.indices) {
          if (matrix(index)(j) == 0 && !markedCols(j)) {
            markedCols(j) = true
            recursivelyMarkRowsAndCols(markedRows, markedCols, false, j)
          }
        }
      } else {
        val r = assignedCols.indexWhere(i => i == index)
        if (r != -1 && !markedRows(r)) {
          markedRows(r) = true
          recursivelyMarkRowsAndCols(markedRows, markedCols, true, r)
        }
      }
    }

    for (i <- assignedRows.indices) {
      if (!assignedRows(i)) {
        markedRows(i) = true
      }
    }

    val newlyMarkedRows = mutable.Set[Int]()
    for (i <- markedRows.indices) {
      if (markedRows(i)) {
        newlyMarkedRows += i
      }
    }

    for (i <- newlyMarkedRows) {
      recursivelyMarkRowsAndCols(markedRows, markedCols, true, i)
    }

    (markedRows, markedCols)
  }


  private def assignRows(matrix: Array[Array[Int]], zerosInRows: Array[Int], zerosInCols: Array[Int]): (Array[Boolean], Array[Int])= {
    val assignedRows = Array.ofDim[Boolean](matrix.length)
    val assignedCols = Array.fill[Int](matrix.length)(-1)
    val crossedZeros = mutable.Map[Int, mutable.Set[Int]]()

    for (i <- matrix.indices) {
      if (zerosInRows(i) > 0) {
        assignedRows(i) = true
        val col = getFirstZeroInArray(matrix(i), crossedZeros.getOrElse(i, mutable.Set()))
        assignedCols(i) = col
        if (zerosInCols(col) > 1) {
          for (k <- i + 1 until matrix.length) {
            if (matrix(k)(col) == 0) {
              zerosInRows(k) -= 1
              var crossedEntries = crossedZeros.getOrElse(k, mutable.Set())
              crossedEntries += col
              crossedZeros.put(k, crossedEntries)
            }
          }
        }
      }
    }
    (assignedRows, assignedCols)
  }

  def getZerosInRowsAndCols(matrix: Array[Array[Int]]): (Array[Int], Array[Int]) = {
    val zerosPerRow = Array.ofDim[Int](matrix.length)
    val zerosPerCol = Array.ofDim[Int](matrix.length)
    // Count the number of 0's per row and the number of 0's per column
    for (i <- matrix.indices) {
      for (j <- matrix.indices) {
        if (matrix(i)(j) == 0) {
          zerosPerRow(i) += 1
          zerosPerCol(j) += 1
        }
      }
    }
    (zerosPerRow, zerosPerCol)
  }

  def markRowColAsDone(matrix: Array[Array[Int]],
                       zerosInRows: Array[Int],
                       zerosInCols: Array[Int],
                       r: Int,
                       c: Int): Unit = {
    for(i <- matrix.indices) {
      if(matrix(r)(i) == 0) {
        zerosInCols(i) -= 1
      }
      if(matrix(i)(c) == 0) {
        zerosInRows(i) -= 1
      }
    }
  }

  // Step 5
  def findOptimalAssignment(matrix: Array[Array[Int]]): List[(Int, Int)] = {
    var result = new ListBuffer[(Int, Int)]()

    val (zerosInRows, zerosInCols) = getZerosInRowsAndCols(matrix)

    while (!isAllNegativeOrZero(zerosInRows) && !isAllNegativeOrZero(zerosInCols)) {

      val selectedLine = getLineWithLeastZeros(zerosInRows, zerosInCols)
      var (r, c) = (-1, -1)

      if (selectedLine.isHorizontal) {
        for (i <- matrix.indices) {
          if (matrix(selectedLine.lineIndex)(i) == 0 && zerosInCols(i) > 0) {
            r = selectedLine.lineIndex
            c = i
          }
        }
        zerosInRows(selectedLine.lineIndex) = 0
        zerosInCols(c) = 0
      } else {
        for (i <- matrix.indices) {
          if (matrix(i)(selectedLine.lineIndex) == 0 && zerosInRows(i) > 0) {
            c = selectedLine.lineIndex
            r = i
          }
        }
        zerosInCols(selectedLine.lineIndex) = 0
        zerosInRows(r) = 0
      }

      result += ((r, c))

      markRowColAsDone(matrix, zerosInRows, zerosInCols, r, c)
    }

    result.toList
  }

  private def updateZeros(matrix: Array[Array[Int]], zerosInRows: Array[Int], zerosInCols: Array[Int], selectedLine: Line) = {
    if (selectedLine.isHorizontal) {
      for (j <- matrix.indices) {
        if (matrix(selectedLine.lineIndex)(j) == 0) {
          zerosInCols(j) -= 1
        }
      }
    } else {
      for (i <- matrix.indices) {
        if (matrix(i)(selectedLine.lineIndex) == 0) {
          zerosInRows(i) -= 1
        }
      }
    }
  }

  def getMinimumUncovered(matrix: Array[Array[Int]], coveredRows: mutable.Set[Int], coveredCols: mutable.Set[Int]): Int = {
    var min = Int.MaxValue
    for(i <- matrix.indices) {
      if (!coveredRows.contains(i)) {
        for (j <- matrix.indices) {
          if (!coveredCols.contains(j) && matrix(i)(j) < min) {
            min = matrix(i)(j)
          }
        }
      }
    }
    min
  }

  def subtractMinFromUncoveredRows(matrix: Array[Array[Int]], coveredRows: mutable.Set[Int], minimumUncovered: Int): Unit = {
    for(i <- matrix.indices) {
      if (!coveredRows.contains(i)) {
        for (j <- matrix.indices) {
          matrix(i)(j) -= minimumUncovered
        }
      }
    }
  }

  def addMinToCoveredCols(matrix: Array[Array[Int]], coveredCols: mutable.Set[Int], minimumUncovered: Int): Unit = {
    for(i <- matrix.indices) {
      if (coveredCols.contains(i)) {
        for (j <- matrix.indices) {
          matrix(j)(i) += minimumUncovered
        }
      }
    }
  }

  def reCompute(matrix: Array[Array[Int]], coveredRows: Set[Int], coveredCols: Set[Int]): Unit = {
    val minimumUncovered = getMinimumUncovered(matrix, coveredRows, coveredCols)
    subtractMinFromUncoveredRows(matrix, coveredRows, minimumUncovered)
    addMinToCoveredCols(matrix, coveredCols, minimumUncovered)
  }

  def compute(input: Array[Array[Int]]): List[(Int, Int)] = {
    val matrix = input map(_.clone)
    subtractMinFromRows(matrix)
    subtractMinFromCols(matrix)
    var (lines, coveredRows, coveredCols) = getCoveringLines(matrix)
    while (lines.size < matrix.length) {
      reCompute(matrix, coveredRows, coveredCols)
      val res = getCoveringLines(matrix)
      lines = res._1
      coveredRows = res._2
      coveredCols = res._3
    }
    findOptimalAssignment(matrix)
  }
}
