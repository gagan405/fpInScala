package in.umlaut.maths

import org.scalatest.{FunSuite, Matchers}

class ModCacheTest extends FunSuite with Matchers {

  test("Get modulo of tenth power") {
    val mod = 1000000007
    val cache = new ModCache(mod)

    assert(BigInt(cache.getModOfTenthPower(15, 1, 10)) == Maths.tenthPower(15) % 1000000007)
    assert(BigInt(cache.getModOfTenthPower(25, 15, 993000007)) == Maths.tenthPower(25) % 1000000007)
    assert(BigInt(cache.getModOfTenthPower(250, 25, 490000000L)) == Maths.tenthPower(250) % 1000000007)

    val start = System.currentTimeMillis()
    val x = Maths.tenthPower(50000) % 1000000007
    val mid = System.currentTimeMillis()
    val y = cache.getModOfTenthPower(50000, 25, 490000000L)
    val end = System.currentTimeMillis()

    assert(x == BigInt(y))
    println("Time taken in 1st method: " + (mid - start))
    println("Time taken in 2nd method: " + (end - mid))

  }

  test("Get modulo of all 9s") {
    val mod = 1000000007
    val cache = new ModCache(mod)

    val x = BigInt("9999999999") % mod
    var y = cache.getModOfAll9s(11, 10, x.toInt)
    assert(BigInt("9" * 11) % mod == y)

    y = cache.getModOfAll9s(18, 10, x.toInt)
    assert(BigInt("9" * 18) % mod == y)

    y = cache.getModOfAll9s(11, 10, x.toInt)
    assert(BigInt("9" * 11) % mod == y)

    y = cache.getModOfAll9s(19, 10, x.toInt)
    assert(BigInt("9" * 19) % mod == y)

    y = cache.getModOfAll9s(20, 10, x.toInt)
    assert(BigInt("9" * 20) % mod == y)

    y = cache.getModOfAll9s(399, 10, x.toInt)
    assert(BigInt("9" * 399) % mod == y)
  }

  test("Retrieve from cache") {
    val mod = 1000000007
    val cache = new ModCache(mod)
    val x = BigInt("9999999999") % mod

    val start = System.currentTimeMillis()
    var y = cache.getModOfAll9sFromCache(399, 10, x.toInt)
    assert(BigInt("9" * 399) % mod == y)

    val mid = System.currentTimeMillis()
    y = cache.getModOfAll9sFromCache(399, 10, x.toInt)
    assert(BigInt("9" * 399) % mod == y)

    val end = System.currentTimeMillis()

    println("Time without cache " + (mid - start) )
    println("Time with cache " + (end - mid) )

  }

  test("Retrieve from cache 2") {
    val mod = 1000000007
    val cache = new ModCache(mod)

    val x = BigInt("9999999999") % mod
    var y = cache.getModOfAllNines(11)
    assert(BigInt("9" * 11) % mod == y)


    y = cache.getModOfAllNines(15)
    assert(BigInt("9" * 15) % mod == y)

  }


  test("Retrieve for GP") {
    val mod = 1000000007
    val cache = new ModCache(mod)

    var x = cache.getModOfAllOnes(4)
    assert(x == 1111)

    x = cache.getModOfAllOnes(5)
    assert(x == 11111)

    x = cache.getModOfAllOnes(6)
    assert(x == 111111)

    x = cache.getModOfAllOnes(9)
    assert(x == 111111111)

    x = cache.getModOfAllOnes(10)
    assert(x == 1111111111L % mod)

    x = cache.getModOfAllOnes(15)
    assert(x == 111111111111111L % mod)

    x = cache.getModOfAllOnes(1529)
    assert(x == BigInt("1" * 1500) % mod)

  }


  test("Get mod of tenth power using binary factorization") {
    val mod = 1000000007
    val cache = new ModCache(mod)

    var x = cache.getModOfTenthPower(2)
    assert(x == 100)

    x = cache.getModOfTenthPower(3)
    assert(x == 1000)


    x = cache.getModOfTenthPower(4)
    assert(x == 10000)

    x = cache.getModOfTenthPower(16)
    assert(Maths.tenthPower(16) % mod == x)

    x = cache.getModOfTenthPower(25)
    assert(Maths.tenthPower(25) % mod == x)

    x = cache.getModOfTenthPower(41)
    assert(Maths.tenthPower(41) % mod == x)

    x = cache.getModOfTenthPower(268)
    assert(Maths.tenthPower(268) % mod == x)

    x = cache.getModOfTenthPower(26892)
    assert(Maths.tenthPower(26892) % mod == x)

  }


  test("Get first set bit") {
    val mod = 1000000007
    val cache = new ModCache(mod)

    var x = cache.getPosOfFirstSetBitFromHigherEnd(1024)
    assert(x == 10)

    x = cache.getPosOfFirstSetBitFromHigherEnd(1028)
    assert(x == 10)

    x = cache.getPosOfFirstSetBitFromHigherEnd(65536)
    assert(x == 16)

    x = cache.getPosOfFirstSetBitFromHigherEnd(65538)
    assert(x == 16)
  }

  test("test get mod of GP") {
    val mod = 1000000007
    val cache = new ModCache(mod)

    var x = cache.getModOfGp(100)
    assert(x == BigInt("1" * 100) % mod)

    x = cache.getModOfGp(10000)
    assert(x == BigInt("1" * 10000) % mod)
  }

}
