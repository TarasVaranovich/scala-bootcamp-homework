package adt

object GameService {

  def playGame(rules: (Set[Card]) => Option[Hand])
              (boardCards: List[Card], handsCards: List[List[Card]]): Map[Hand, PokerCombination] = {
    (for {
      playCards <- handsCards
      hand <- rules.apply(playCards.toSet)
      board <- Board.fromCards(boardCards.toSet)
      combination <- bestCombination.apply(board, hand)
      gameSet <- Option.apply((hand, combination))
    } yield gameSet).toMap
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