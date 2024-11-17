extension (mappings: Vector[Map])
  def multiRangeApply(keys: Set[NumericRange]): Set[NumericRange] =
    mappings.foldLeft(keys): (acc, mapping) =>
      mapping(acc)

val (seeds, mappings) = fromString(os.read.lines(os.pwd / "example1.txt"))
val seedRanges = seeds.toRanges

@main def problem2(): Unit =
  println(minimumLocation)

def minimumLocation =
  val (inputSeeds, mappings) = fromString(os.read.lines(wd / "problem1.txt"))
  val inputSeedRanges = inputSeeds.toRanges
  inputSeedRanges.map(range =>
    mappings
      .multiRangeApply(Set(range))
      .map(_.start)
      .min
  ).min

extension (seeds: Vector[Long])
  def toRanges: Vector[NumericRange] =
    seeds
      .grouped(2)
      .collect:
        case Vector(start, length) => NumericRange(start, length)
      .toVector
