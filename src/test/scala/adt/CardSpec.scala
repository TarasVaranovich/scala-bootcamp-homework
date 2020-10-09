package adt

import org.scalatest.flatspec.AnyFlatSpec

class CardSpec extends AnyFlatSpec {

  "cards" should "be equal" in {
    assert(Card(Rank.Ace, Suit.Clubs) == Card(Rank.Ace, Suit.Clubs))
  }

  "cards" should "not be equal by suit" in {
    assert(Card(Rank.Ace, Suit.Clubs) != Card(Rank.Ace, Suit.Diamonds))
  }

  "cards" should "not be equal by rank" in {
    assert(Card(Rank.King, Suit.Clubs) != Card(Rank.Ace, Suit.Clubs))
  }

  "cards" should "be ordered by rank" in {
    val cards: Set[Card] = Set(
      Card(Rank.Ace, Suit.Hearts),
      Card(Rank.Jack, Suit.Clubs),
      Card(Rank.King, Suit.Diamonds),
      Card(Rank.Five, Suit.Spades))

    val orderedCards: List[Card] = List(
      Card(Rank.Five, Suit.Spades),
      Card(Rank.Jack, Suit.Clubs),
      Card(Rank.King, Suit.Diamonds),
      Card(Rank.Ace, Suit.Hearts)
    )

    assert(cards.toList.sorted == orderedCards)
  }
}
