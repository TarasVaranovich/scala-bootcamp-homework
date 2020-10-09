package adt

object TestCase {

  def playGame(boardCards: List[Card], handsCards: List[List[Card]]): Map[Hand, PokerCombination] = {
    handsCards
      .flatMap(playerCards => Hand.Texas.fromCards(playerCards.toSet)
        .flatMap(hand => Board.fromCards(boardCards.toSet)
          .flatMap(board => bestCombination.apply(board, hand)
            .flatMap(combination => Option.apply((hand, combination)))
          ))).toMap
  }

  private def bestCombination: PartialFunction[(Board, Hand), Option[PokerCombination]] =
    PokerCombination.StraightFlush.make orElse
      PokerCombination.FourOfKind.make orElse
      PokerCombination.FullHouse.make orElse
      PokerCombination.Flush.make orElse
      PokerCombination.Straight.make orElse
      PokerCombination.ThreeOfKind.make orElse
      PokerCombination.TwoPairs.make orElse
      PokerCombination.Pair.make orElse
      PokerCombination.HighCard.make


  def judgeResult(result: Map[Hand, PokerCombination]): List[Hand] =
    result.toSeq.sortBy { case (_, combination) => combination }
      .map { case (hand, _) => hand }
      .toList
}
