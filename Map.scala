case class Map private (
    val mappings: Vector[(NumericRange, Map.Number)]
):
  import Map.Number

  def apply(key: Number): Number =
    mappings.find(_._1 contains key) match
      case Some((mapping, offset)) => key + offset
      case None                    => key
  
  def apply(keys: Vector[NumericRange]): Vector[NumericRange] =
    ???

object Map:
  type Number = Long

  def apply(mappings: (Number, Number, Number)*): Map =
    val parsedMappings =
      for (destination, source, len) <- mappings yield
        val offset = destination - source
        (NumericRange(source, len), offset)

    new Map(parsedMappings.toVector)
