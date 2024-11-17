class MapTest extends munit.FunSuite:
  test("Simple apply"):
    val map = Map((50, 98, 2), (52, 50, 48))
    assertEquals(map.get(98), 50L)
    assertEquals(map.get(99), 51L)

  test("Range apply"):
    val result = mappings(0)(Set(seedRanges(0)))
    assertEquals(result, Set(NumericRange(81, 14)))