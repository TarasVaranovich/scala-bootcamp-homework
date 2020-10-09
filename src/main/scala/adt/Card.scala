package adt

final case class Card(rank: Rank, suit: Suit) extends Ordered[Card] {
  override def compare(that: Card): Int = this.rank.weight.compareTo(that.rank.weight)
}