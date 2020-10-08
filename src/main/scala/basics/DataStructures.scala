package basics

object DataStructures {

  def sortConsideringEqualValues[T](map: Map[T, Int]): List[(Set[T], Int)] =
    map
      .groupBy { case (_, intValue) => intValue }
      .map { case (intKey, genericKeysMap) => (genericKeysMap.keySet, intKey) }
      .toList
      .sortBy { case (_, intId) => intId }
  
  case class NotImplemented(value: Int)
}
