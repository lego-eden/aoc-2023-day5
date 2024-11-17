extension (mappings: Vector[Map])
  def multiRangeApply(keys: Set[NumericRange]): Set[NumericRange] =
    mappings.foldLeft(keys): (acc, mapping) =>
      mapping(acc)

@main def problem2(): Unit =
  println(minimumLocation)

def minimumLocation =
  val (inputSeeds, mappings) = fromString(os.read.lines(wd/"example1.txt"))
  val inputSeedRanges = inputSeeds
    .grouped(2)
    .collect:
      case Vector(start, length) => NumericRange(start, length)
  inputSeedRanges.minBy: range =>
    mappings
      .multiRangeApply(Set(range))
      .map(_.start)
      .min