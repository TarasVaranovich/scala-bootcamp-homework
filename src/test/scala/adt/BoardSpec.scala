package adt

import org.scalatest.flatspec.AnyFlatSpec

class BoardSpec extends AnyFlatSpec {

  "board" should "be successfully created from set" in {
    val cards: Set[Card] = Set(
      Card(Rank.Ace, Suit.Hearts),
      Card(Rank.Jack, Suit.Clubs),
      Card(Rank.King, Suit.Diamonds),
      Card(Rank.Five, Suit.Spades),
      Card(Rank.Six, Suit.Spades))

    assert(Board.fromCards(cards).isDefined)
  }

  "board" should "not be created" in {
    val cards: Set[Card] = Set(
      Card(Rank.Ace, Suit.Hearts),
      Card(Rank.Jack, Suit.Clubs),
      Card(Rank.King, Suit.Diamonds),
      Card(Rank.Five, Suit.Spades))

    assert(Board.fromCards(cards).isEmpty)
  }
}
