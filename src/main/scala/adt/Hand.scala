package adt

sealed trait Hand {
  def asList: List[Card]
}

object Hand {

  private def fromCardsFunction(handSize: Int, create: (List[Card]) => Hand): (Set[Card]) => Option[Hand] =
    (cards: Set[Card]) => if (cards.size == handSize) Some(create.apply(cards.toList)) else None

  final case class Texas private(first: Card, second: Card) extends Hand {
    override def asList: List[Card] = first :: second :: Nil
  }
  object Texas {
    private val create = (cardList: List[Card]) => Texas(cardList.head, cardList.last)
    def rules = fromCardsFunction(2, create)
  }

  final case class Omaha private(first: Card, second: Card, third: Card, forth: Card) extends Hand {
    override def asList: List[Card] = first :: second :: third :: forth :: Nil
  }
  object Omaha {
    private val create = (cardList: List[Card]) => Omaha(cardList.head, cardList(1), cardList(2), cardList.last)
    def rules = fromCardsFunction(4, create)
  }
}