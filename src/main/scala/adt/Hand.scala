package adt

sealed trait Hand {
  def asList: List[Card]
}

object Hand {
  sealed trait HandBuilder {
    def fromCards(cards: Set[Card]): Option[Hand]
  }

  final case class Texas private(first: Card, second: Card) extends Hand {
    override def asList: List[Card] = first :: second :: Nil
  }
  object Texas extends HandBuilder {
    override def fromCards(cards: Set[Card]): Option[Texas] = {
      val cardList = cards.toList
      if (cardList.size == 2) Some(Texas(cardList.head, cardList.last)) else None
    }
  }

  final case class Omaha private(first: Card, second: Card, third: Card, forth: Card) extends Hand {
    override def asList: List[Card] = first :: second :: third :: forth :: Nil
  }
  object Omaha extends HandBuilder {
    override def fromCards(cards: Set[Card]): Option[Omaha] = {
      val cardList = cards.toList
      if (cardList.size == 4) Some(Omaha(cardList.head, cardList(1), cardList(2), cardList.last)) else None
    }
  }
}