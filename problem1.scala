lazy val wd = os.pwd

def parse(s: String): Map.Number = s.toLong

extension [A](xs: IndexedSeq[A])
  def split(elem: A): IndexedSeq[IndexedSeq[A]] =
    xs.foldLeft(Vector(Vector.empty[A])): (acc, x) =>
      if x == elem then acc :+ Vector.empty
      else acc.updated(acc.indices.last, acc.last :+ x)

def fromString(str: IndexedSeq[String]): (Vector[Map.Number], Vector[Map]) =
  val inputSeeds = str.head.split(" ").tail.map(parse).toVector
  val mappings =
    str.drop(2)
      .split("")
      .map(
        _.tail.map { case s"$destination $source $len" =>
          (destination, source, len).map[[_] =>> Map.Number](
            [_] =>
              s =>
                s match
                  case s: String => parse(s.strip())
          )
        }
      )
      .map(Map(_*))
      .toVector

  (inputSeeds, mappings)
end fromString

extension (mappings: Vector[Map])
  def multiApply(key: Map.Number): Map.Number =
    mappings.foldLeft(key): (acc, mapping) =>
      mapping(acc)

def lowestLocation(
    inputSeeds: Vector[Map.Number],
    mappings: Vector[Map]
): Map.Number =
  inputSeeds.map(mappings.multiApply).min

def minimumSeed(input: (Vector[Map.Number], Vector[Map])): Map.Number =
  lowestLocation(input._1, input._2)

@main def problem1(): Unit =
  println(minimumSeed(fromString(os.read.lines(wd/"problem1.txt"))))
