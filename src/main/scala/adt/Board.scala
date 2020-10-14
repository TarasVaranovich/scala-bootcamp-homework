package adt

final case class Board private(cards: Set[Card])
object Board {
  def fromCards(cards: Set[Card]): Option[Board] =
    if (cards.size == 5) Some(Board(cards)) else None
}