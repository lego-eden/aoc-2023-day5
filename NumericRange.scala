case class NumericRange(start: Long, length: Long):

  val end = start + length - 1

  infix def contains(value: Long): Boolean =
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

  def nonEmpty: Boolean = 0 < length

extension (start: Long)
  def ::(end: Long): NumericRange = NumericRange(start, end - start + 1)

extension (ranges: Set[NumericRange])
  infix def splitRange(other: Set[NumericRange]): Set[NumericRange] =
    other.foldLeft(ranges): (acc, range) =>
      acc.flatMap(_ splitRange range)