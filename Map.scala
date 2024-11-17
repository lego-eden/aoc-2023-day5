case class Map private (
    val mappings: Set[(NumericRange, Long)]
):
  def get(key: Long): Long =
    mappings.find(_._1 contains key) match
      case Some((mapping, offset)) => key + offset
      case None                    => key

  def apply(keys: Set[NumericRange]): Set[NumericRange] =
    keys.splitRange(mappings.map(_._1))
      .map(range => range.copy(start = get(range.start)))

object Map:
  def apply(mappings: (Long, Long, Long)*): Map =
    val parsedMappings =
      for (destination, source, len) <- mappings yield
        val offset = destination - source
        (NumericRange(source, len), offset)

    new Map(Set(parsedMappings*))
