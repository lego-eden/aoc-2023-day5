extension (mappings: Vector[Map])
  def multiRangeApply(keys: Vector[NumericRange]): Vector[NumericRange] = 
    mappings.foldLeft(keys): (acc, mapping) =>
      mapping(acc)

def lowestLocation2(inputSeeds: Vector[Vector[NumericRange]], mappings: Vector[Map]): Map.Number =
  inputSeeds.map(mappings.multiRangeApply)
  ???

@main def problem2(): Unit =
  val (inputSeeds, mappings) = fromString(os.read(wd/"example1.txt"))
  val inputSeedRanges = inputSeeds.grouped(2).map:
    case Vector(start, length) => NumericRange(start, length)

  