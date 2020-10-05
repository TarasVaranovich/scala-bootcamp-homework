package basics

import scala.util.Try

object DataStructures {

  def sortConsideringEqualValues[T](map: Map[_ <: T, Int]): List[(Set[_ <: T], Int)] = {
    Try(map.groupBy(_._2)
      .map(entry => (entry._2.keySet, entry._1))
      .toList
      .sortBy(_._2))
      .getOrElse(List.empty)
  }

  case class NotImplemented(value: Int)
}
