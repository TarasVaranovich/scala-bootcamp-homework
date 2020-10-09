package adt

import org.scalatest.flatspec.AnyFlatSpec

class HandSpec extends AnyFlatSpec {

  val fiveCards: Set[Card] = Set(
    Card(Rank.Ace, Suit.Hearts),
    Card(Rank.Jack, Suit.Clubs),
    Card(Rank.King, Suit.Diamonds),
    Card(Rank.Five, Suit.Spades),
    Card(Rank.Six, Suit.Spades),
    Card(Rank.Seven, Suit.Spades))

  "texas hand" should "be successfully created from set" in {
    val cards: Set[Card] = Set(
      Card(Rank.Ace, Suit.Hearts),
      Card(Rank.Jack, Suit.Clubs))

    assert(Hand.Texas.fromCards(cards).isDefined)
  }

  "texas hand" should "not be created from set" in {

    assert(Hand.Texas.fromCards(fiveCards).isEmpty)
  }

  "omaha hand" should "be successfully created from set" in {
    val cards: Set[Card] = Set(
      Card(Rank.Ace, Suit.Hearts),
      Card(Rank.Jack, Suit.Clubs),
      Card(Rank.King, Suit.Diamonds),
      Card(Rank.Five, Suit.Spades))

    assert(Hand.Omaha.fromCards(cards).isDefined)
  }

  "omaha hand" should "not be created from set" in {

    assert(Hand.Omaha.fromCards(fiveCards).isEmpty)
  }
}