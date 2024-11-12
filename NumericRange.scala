case class NumericRange(start: Map.Number, length: Map.Number):
  
  val end = start + length - 1

  infix def contains(value: Map.Number): Boolean =
    0 < length && start <= value && value <= start + length

  infix def intersect(other: NumericRange): NumericRange =
    val maxStart = start max other.start
    val minEnd = end min other.end
    
    NumericRange(maxStart, minEnd - maxStart + 1)

  def toVector: Vector[Map.Number] = Vector((start to end)*)

extension (x: Map.Number)
  def ~(y: Map.Number): NumericRange = NumericRange(x, y)

extension (ranges: Vector[NumericRange])
  // merges this vector of ranges into another vector of ranges by combining
  // intersecting ranges while still keeping the "free" ranges
  infix def merge(other: Vector[NumericRange]): Vector[NumericRange] =
    other.foldLeft(ranges): (acc, range) =>
      ???