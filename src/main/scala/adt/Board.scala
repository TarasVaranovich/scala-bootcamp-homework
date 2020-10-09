package adt

final case class Board private(cards: collection.immutable.Set[Card])
object Board {
  def fromCards(cards: collection.immutable.Set[Card]): Option[Board] =
    if (cards.size == 5) Some(Board(cards)) else None
}