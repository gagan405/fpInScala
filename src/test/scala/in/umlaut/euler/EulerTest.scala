package in.umlaut.euler

import org.scalatest.{FunSuite, Matchers}

class EulerTest  extends FunSuite with Matchers {

  /**
    * Tests solution of project euler problem 80
    */
  test("Should get me the result for problem 80"){
    println(DigitalSumOfSquareRoots.getSumOfFirstHundredDigits)
  }

  /**
    * Tests solution of project euler problem 59
    */
  test("Should decrypt just fine") {
    val input = "79,59,12,2,79,35,8,28,20,2,3,68,8,9,68,45,0,12,9,67,68,4,7,5,23,27,1,21,79,85," +
      "78,79,85,71,38,10,71,27,12,2,79,6,2,8,13,9,1,13,9,8,68,19,7,1,71,56,11,21,11,68,6,3,22,2," +
      "14,0,30,79,1,31,6,23,19,10,0,73,79,44,2,79,19,6,28,68,16,6,16,15,79,35,8,11,72,71,14,10,3," +
      "79,12,2,79,19,6,28,68,32,0,0,73,79,86,71,39,1,71,24,5,20,79,13,9,79,16,15,10,68,5,10,3,14," +
      "1,10,14,1,3,71,24,13,19,7,68,32,0,0,73,79,87,71,39,1,71,12,22,2,14,16,2,11,68,2,25,1,21," +
      "22,16,15,6,10,0,79,16,15,10,22,2,79,13,20,65,68,41,0,16,15,6,10,0,79,1,31,6,23,19,28,68," +
      "19,7,5,19,79,12,2,79,0,14,11,10,64,27,68,10,14,15,2,65,68,83,79,40,14,9,1,71,6,16,20,10,8," +
      "1,79,19,6,28,68,14,1,68,15,6,9,75,79,5,9,11,68,19,7,13,20,79,8,14,9,1,71,8,13,17,10,23,71," +
      "3,13,0,7,16,71,27,11,71,10,18,2,29,29,8,1,1,73,79,81,71,59,12,2,79,8,14,8,12,19,79,23,15," +
      "6,10,2,28,68,19,7,22,8,26,3,15,79,16,15,10,68,3,14,22,12,1,1,20,28,72,71,14,10,3,79,16,15," +
      "10,68,3,14,22,12,1,1,20,28,68,4,14,10,71,1,1,17,10,22,71,10,28,19,6,10,0,26,13,20,7,68,14," +
      "27,74,71,89,68,32,0,0,71,28,1,9,27,68,45,0,12,9,79,16,15,10,68,37,14,20,19,6,23,19,79,83," +
      "71,27,11,71,27,1,11,3,68,2,25,1,21,22,11,9,10,68,6,13,11,18,27,68,19,7,1,71,3,13,0,7,16," +
      "71,28,11,71,27,12,6,27,68,2,25,1,21,22,11,9,10,68,10,6,3,15,27,68,5,10,8,14,10,18,2,79,6," +
      "2,12,5,18,28,1,71,0,2,71,7,13,20,79,16,2,28,16,14,2,11,9,22,74,71,87,68,45,0,12,9,79,12," +
      "14,2,23,2,3,2,71,24,5,20,79,10,8,27,68,19,7,1,71,3,13,0,7,16,92,79,12,2,79,19,6,28,68,8," +
      "1,8,30,79,5,71,24,13,19,1,1,20,28,68,19,0,68,19,7,1,71,3,13,0,7,16,73,79,93,71,59,12,2,79," +
      "11,9,10,68,16,7,11,71,6,23,71,27,12,2,79,16,21,26,1,71,3,13,0,7,16,75,79,19,15,0,68,0,6," +
      "18,2,28,68,11,6,3,15,27,68,19,0,68,2,25,1,21,22,11,9,10,72,71,24,5,20,79,3,8,6,10,0,79,16," +
      "8,79,7,8,2,1,71,6,10,19,0,68,19,7,1,71,24,11,21,3,0,73,79,85,87,79,38,18,27,68,6,3,16,15," +
      "0,17,0,7,68,19,7,1,71,24,11,21,3,0,71,24,5,20,79,9,6,11,1,71,27,12,21,0,17,0,7,68,15,6,9," +
      "75,79,16,15,10,68,16,0,22,11,11,68,3,6,0,9,72,16,71,29,1,4,0,3,9,6,30,2,79,12,14,2,68,16," +
      "7,1,9,79,12,2,79,7,6,2,1,73,79,85,86,79,33,17,10,10,71,6,10,71,7,13,20,79,11,16,1,68,11," +
      "14,10,3,79,5,9,11,68,6,2,11,9,8,68,15,6,23,71,0,19,9,79,20,2,0,20,11,10,72,71,7,1,71,24,5," +
      "20,79,10,8,27,68,6,12,7,2,31,16,2,11,74,71,94,86,71,45,17,19,79,16,8,79,5,11,3,68,16,7,11," +
      "71,13,1,11,6,1,17,10,0,71,7,13,10,79,5,9,11,68,6,12,7,2,31,16,2,11,68,15,6,9,75,79,12,2," +
      "79,3,6,25,1,71,27,12,2,79,22,14,8,12,19,79,16,8,79,6,2,12,11,10,10,68,4,7,13,11,11,22,2," +
      "1,68,8,9,68,32,0,0,73,79,85,84,79,48,15,10,29,71,14,22,2,79,22,2,13,11,21,1,69,71,59,12," +
      "14,28,68,14,28,68,9,0,16,71,14,68,23,7,29,20,6,7,6,3,68,5,6,22,19,7,68,21,10,23,18,3,16," +
      "14,1,3,71,9,22,8,2,68,15,26,9,6,1,68,23,14,23,20,6,11,9,79,11,21,79,20,11,14,10,75,79," +
      "16,15,6,23,71,29,1,5,6,22,19,7,68,4,0,9,2,28,68,1,29,11,10,79,35,8,11,74,86,91,68,52,0," +
      "68,19,7,1,71,56,11,21,11,68,5,10,7,6,2,1,71,7,17,10,14,10,71,14,10,3,79,8,14,25,1,3,79," +
      "12,2,29,1,71,0,10,71,10,5,21,27,12,71,14,9,8,1,3,71,26,23,73,79,44,2,79,19,6,28,68,1,26," +
      "8,11,79,11,1,79,17,9,9,5,14,3,13,9,8,68,11,0,18,2,79,5,9,11,68,1,14,13,19,7,2,18,3,10,2," +
      "28,23,73,79,37,9,11,68,16,10,68,15,14,18,2,79,23,2,10,10,71,7,13,20,79,3,11,0,22,30,67," +
      "68,19,7,1,71,8,8,8,29,29,71,0,2,71,27,12,2,79,11,9,3,29,71,60,11,9,79,11,1,79,16,15,10,68," +
      "33,14,16,15,10,22,73"

    val res = XorEncryption.decrypt(input, 3)
    println(res._1, res._2)
  }

  /**
    * Tests solution of project euler problem 78
    */
  test("Should get me the result"){
    println(CountPartitions.findNWithPartitionValueDivisibleByMillion)
  }

  /**
    * Tests solution of project euler problem 89
    */
  test("Should get me the result for RomanNumerals"){
    println(RomanNumeralsShortener.shortenAndCountCharsGained)
  }


  /**
    * Tests solution of project euler problem 39 / 75
    */
  test("Should get me the result for Integer RightTriangles"){
    println(IntegerRightTriangles.countTriangles(1500000))
  }

  /**
    * Tests solution of project euler problem 35
    */
  test("Should count the number of circular primes"){
    println(CircularPrimes.getCircularPrimes(1000000))
  }

  /**
    * Tests solution of project euler problem 70
    */
  test("Should get the totient functions"){
    println(TotientPermutations.getTotientFunctionsWithPermutations(10000000))
  }

  /**
    * Tests solution of project euler problem 49
    */
  test("Should get the prime permutations"){
    println(PrimePermutations.getPrimePermutations(10000, 4))
  }


  /**
    * Tests solution of project euler problem 38
    */
  test("Should get the pandigital number"){
    println(Digits.getPanDigitalNumber())
  }

  /**
    * Tests solution of project euler problem 41
    */
  test("Should get the pandigital prime number"){
    println(Digits.getLargestPandigitalPrime())
  }

  test("Should get the max consecutive sum prime") {
    val start = System.currentTimeMillis()
    println(Primes.consecutivePrimeSum(1000000))
    println(System.currentTimeMillis() - start)
  }

  test("Should count the primes in spiral") {
    println(Primes.spiralPrimes(10))
  }

  test("Should count n-digit nth power numbers") {
    println(Digits.countNDigitNumbersWhichAreAlsoNthPowers())
  }

  test("Should get maximum digital sum") {
    println(Digits.numberWithHighestSumOfDigits())
  }

  test("Should get last 10 digits of NonMarsenne prime") {
    println(Digits.lastTenDigitsOfNonMersennePrime())
  }

  test("Concealed square") {
    println(Digits.concealedSquare())
  }

  test("Should get the number of quadrilaters with square number of lattice points") {
    val start = System.currentTimeMillis()
    println(Lattice.countQuadrilateralsWithSquareNumberOfLatticePoints(100))
    println(System.currentTimeMillis() - start)
  }

  // project euler 345
  test("Should compute the optimal solution in matrix") {
    var mat = Array(
      Array(7, 53, 183, 439, 863),
        Array(497, 383, 563, 79, 973),
        Array(287, 63, 343, 169, 583),
        Array(627, 343, 773, 959, 943),
        Array(767, 473, 103, 699, 303)
    )

    var res = HungarianMax.hungarianMax(mat)
    assert(res == 3315)

    mat = Array(
      Array(7, 53,  183, 439, 863, 497, 383, 563,  79, 973, 287,  63, 343, 169, 583),
      Array(627, 343, 773, 959, 943, 767, 473, 103, 699, 303, 957, 703, 583, 639, 913),
      Array(447, 283, 463,  29,  23, 487, 463, 993, 119, 883, 327, 493, 423, 159, 743),
      Array(217, 623,   3, 399, 853, 407, 103, 983,  89, 463, 290, 516, 212, 462, 350),
      Array(960, 376, 682, 962, 300, 780, 486, 502, 912, 800, 250, 346, 172, 812, 350),
      Array(870, 456, 192, 162, 593, 473, 915,  45, 989, 873, 823, 965, 425, 329, 803),
      Array(973, 965, 905, 919, 133, 673, 665, 235, 509, 613, 673, 815, 165, 992, 326),
      Array(322, 148, 972, 962, 286, 255, 941, 541, 265, 323, 925, 281, 601,  95, 973),
      Array(445, 721,  11, 525, 473,  65, 511, 164, 138, 672,  18, 428, 154, 448, 848),
      Array(414, 456, 310, 312, 798, 104, 566, 520, 302, 248, 694, 976, 430, 392, 198),
      Array(184, 829, 373, 181, 631, 101, 969, 613, 840, 740, 778, 458, 284, 760, 390),
      Array(821, 461, 843, 513,  17, 901, 711, 993, 293, 157, 274,  94, 192, 156, 574),
      Array(34, 124,   4, 878, 450, 476, 712, 914, 838, 669, 875, 299, 823, 329, 699),
      Array(815, 559, 813, 459, 522, 788, 168, 586, 966, 232, 308, 833, 251, 631, 107),
      Array(813, 883, 451, 509, 615,  77, 281, 613, 459, 205, 380, 274, 302,  35, 805)
    )

    res = HungarianMax.hungarianMax(mat)
    println("Solution: " + res)
  }

  }
