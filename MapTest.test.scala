class MapTest extends munit.FunSuite:
  test("Simple apply"):
    val map = Map((50, 98, 2), (52, 50, 48))
    assertEquals(map(98), 50L)
    assertEquals(map(99), 51L)
