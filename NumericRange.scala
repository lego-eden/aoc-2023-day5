case class NumericRange(start: Map.Number, length: Map.Number):

  val end = start + length - 1

  infix def contains(value: Map.Number): Boolean =
    nonEmpty && start <= value && value <= start + length

  infix def intersect(other: NumericRange): NumericRange =
    val maxStart = start max other.start
    val minEnd = end min other.end

    NumericRange(maxStart, minEnd - maxStart + 1)

  infix def overlaps(other: NumericRange): Boolean =
    intersect(other).nonEmpty

  infix def merge(other: Set[NumericRange]): NumericRange =
    val overlapping = other.filter(this overlaps _)
    if overlapping.isEmpty then this
    else
      val minStart = (overlapping + this).map(_.start).min
      val maxEnd = (overlapping + this).map(_.end).max
      // (other -- overlapping) + NumericRange(minStart, maxEnd - minStart + 1)
      NumericRange(minStart, maxEnd - minStart + 1)

  def isEmpty: Boolean = 0 >= length
  def nonEmpty: Boolean = !isEmpty

  def toVector: Vector[Map.Number] = Vector((start to end)*)

extension (x: Map.Number)
  def ~(y: Map.Number): NumericRange = NumericRange(x, y)

extension (ranges: Set[NumericRange])
  def simplify: Set[NumericRange] =
    val combined: Set[NumericRange] =
      ranges.foldLeft(Set()): (acc, range) =>
        acc + range.merge(ranges - range)
    if combined == ranges then ranges
    else combined.simplify

  // merges this vector of ranges into another vector of ranges by combining
  // intersecting ranges while still keeping the "free" ranges
  infix def merge(other: Set[NumericRange]): Set[NumericRange] =
    (ranges ++ other).simplify
