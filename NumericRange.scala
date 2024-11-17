case class NumericRange(start: Map.Number, length: Map.Number):

  val end = start + length - 1

  infix def contains(value: Map.Number): Boolean =
    nonEmpty && start <= value && value <= start + length

  infix def intersect(other: NumericRange): NumericRange =
    val maxStart = start max other.start
    val minEnd = end min other.end

    maxStart::minEnd

  infix def splitRange(other: NumericRange): Set[NumericRange] =
    if this overlaps other then
      val intersection = this intersect other
      val before = start::(intersection.start - 1)
      val after = (intersection.end + 1)::end
      Set(before, intersection, after).filter(_.nonEmpty)
    else Set(this)

  infix def overlaps(other: NumericRange): Boolean =
    intersect(other).nonEmpty

  infix def merge(other: Set[NumericRange]): NumericRange =
    val overlapping = other.filter(this overlaps _)
    if overlapping.isEmpty then this
    else
      val minStart = (overlapping + this).map(_.start).min
      val maxEnd = (overlapping + this).map(_.end).max

      minStart::maxEnd

  def isEmpty: Boolean = 0 >= length
  def nonEmpty: Boolean = !isEmpty

  def toVector: Vector[Map.Number] = Vector((start to end)*)

object NumericRange:
  given Ordering[NumericRange] =
    Ordering.fromLessThan(_.start < _.start)

  def between(start: Map.Number, end: Map.Number): NumericRange =
    new NumericRange(start, end - start + 1)

extension (x: Map.Number)
  def ~(y: Map.Number): NumericRange = NumericRange(x, y)
  def ::(y: Map.Number): NumericRange = NumericRange.between(x, y)

extension (ranges: Set[NumericRange])
  def simplify: Set[NumericRange] =
    val combined: Set[NumericRange] =
      ranges.foldLeft(Set.empty[NumericRange]): (acc, range) =>
        acc + range.merge(ranges - range)
    if combined == ranges then ranges
    else combined.simplify

  // merges this vector of ranges into another vector of ranges by combining
  // intersecting ranges while still keeping the "free" ranges
  infix def merge(other: Set[NumericRange]): Set[NumericRange] =
    (ranges ++ other).simplify

  def log(): Unit = println(ranges.map(_.toVector.mkString(" ")).mkString(" :: "))

  infix def splitRange(other: Set[NumericRange]): Set[NumericRange] =
    other.simplify.foldLeft(ranges.simplify): (acc, range) =>
      acc.flatMap(_ splitRange range)