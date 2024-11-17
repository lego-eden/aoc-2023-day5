lazy val wd = os.pwd

extension [A](xs: IndexedSeq[A])
  def split(elem: A): IndexedSeq[IndexedSeq[A]] =
    xs.foldLeft(Vector(Vector.empty[A])): (acc, x) =>
      if x == elem then acc :+ Vector.empty
      else acc.updated(acc.indices.last, acc.last :+ x)

def fromString(str: IndexedSeq[String]): (Vector[Long], Vector[Map]) =
  val inputSeeds = str.head.split(" ").tail.map(_.toLong).toVector
  val mappings =
    str
      .drop(2)
      .split("")
      .map(
        _.tail.map { case s"$destination $source $len" =>
          (destination, source, len).map[[_] =>> Long](
            [_] =>
              s =>
                s match
                  case s: String => s.strip().toLong
          )
        }
      )
      .map(Map(_*))
      .toVector

  (inputSeeds, mappings)
end fromString

extension (mappings: Vector[Map])
  def multiApply(key: Long): Long =
    mappings.foldLeft(key): (acc, mapping) =>
      mapping.get(acc)

def lowestLocation(
    inputSeeds: Vector[Long],
    mappings: Vector[Map]
): Long =
  inputSeeds.map(mappings.multiApply).min

def minimumSeed(input: (Vector[Long], Vector[Map])): Long =
  lowestLocation(input._1, input._2)

@main def problem1(): Unit =
  println(minimumSeed(fromString(os.read.lines(wd / "problem1.txt"))))
