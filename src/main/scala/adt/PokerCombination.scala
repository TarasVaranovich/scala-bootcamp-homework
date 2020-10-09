package adt

/**
 * Comparator subbed with '0'
 * Create method stubbed with 'None'
 * Defined method stubbed with 'true'
 */
sealed trait PokerCombination extends Ordered[PokerCombination] {
  def weight: Byte

  protected def compareByWeight(thisCombination: PokerCombination, thatCombination: PokerCombination): Option[Int] = {
    val result = thisCombination.weight.compareTo(thatCombination.weight)
    if (result == 0) None else Some(result)
  }
}

object PokerCombination {
  sealed trait PokerCombinationBuilder {
    protected def create(board: Board, hand: Hand): Option[PokerCombination]
    protected def defined(board: Board, hand: Hand): Boolean
    def make: PartialFunction[(Board, Hand), Option[PokerCombination]]
  }

  final case class HighCard private(card: Card, remains: collection.immutable.Set[Card])
    extends PokerCombination {
    override def weight: Byte = 1
    override def compare(that: PokerCombination): Int = compareByWeight(this, that)
      .getOrElse(0)
  }
  object HighCard extends PokerCombinationBuilder {
    override protected def create(board: Board, hand: Hand): Option[HighCard] = Option.empty
    override protected def defined(board: Board, hand: Hand): Boolean = true
    def make: PartialFunction[(Board, Hand), Option[PokerCombination]] = {
      case (board, hand) if defined(board, hand) => create(board, hand)
    }
  }

  final case class Pair private(first: Card, second: Card, remains: collection.immutable.Set[Card])
    extends PokerCombination {
    override def weight: Byte = 2
    override def compare(that: PokerCombination): Int = compareByWeight(this, that)
      .getOrElse(0)
  }
  object Pair extends PokerCombinationBuilder {
    override protected def create(board: Board, hand: Hand): Option[PokerCombination] = Option.empty
    override protected def defined(board: Board, hand: Hand): Boolean = true
    def make: PartialFunction[(Board, Hand), Option[PokerCombination]] = {
      case (board, hand) if defined(board, hand) => create(board, hand)
    }
  }

  final case class TwoPairs private(firstPair: Pair, secondPair: Pair, remain: Card)
    extends PokerCombination {
    override def weight: Byte = 3
    override def compare(that: PokerCombination): Int = compareByWeight(this, that)
      .getOrElse(0)
  }
  object TwoPairs extends PokerCombinationBuilder {
    override protected def create(board: Board, hand: Hand): Option[PokerCombination] = Option.empty
    override protected def defined(board: Board, hand: Hand): Boolean = true
    def make: PartialFunction[(Board, Hand), Option[PokerCombination]] = {
      case (board, hand) if defined(board, hand) => create(board, hand)
    }
  }

  final case class ThreeOfKind private(first: Card, second: Card, third: Card, remains: collection.immutable.Set[Card])
    extends PokerCombination {
    override def weight: Byte = 4
    override def compare(that: PokerCombination): Int = compareByWeight(this, that)
      .getOrElse(0)
  }
  object ThreeOfKind extends PokerCombinationBuilder {
    override protected def create(board: Board, hand: Hand): Option[PokerCombination] = Option.empty
    override protected def defined(board: Board, hand: Hand): Boolean = true
    def make: PartialFunction[(Board, Hand), Option[PokerCombination]] = {
      case (board, hand) if defined(board, hand) => create(board, hand)
    }
  }

  final case class Straight private(straight: collection.immutable.Set[Card]) extends PokerCombination {
    override def weight: Byte = 5
    override def compare(that: PokerCombination): Int = compareByWeight(this, that)
      .getOrElse(0)
  }
  object Straight extends PokerCombinationBuilder {
    override protected def create(board: Board, hand: Hand): Option[PokerCombination] = Option.empty
    override protected def defined(board: Board, hand: Hand): Boolean = true
    def make: PartialFunction[(Board, Hand), Option[PokerCombination]] = {
      case (board, hand) if defined(board, hand) => create(board, hand)
    }
  }

  final case class Flush private(flush: collection.immutable.Set[Card]) extends PokerCombination {
    override def weight: Byte = 6
    override def compare(that: PokerCombination): Int = compareByWeight(this, that)
      .getOrElse(0)
  }
  object Flush extends PokerCombinationBuilder {
    override protected def create(board: Board, hand: Hand): Option[PokerCombination] = Option.empty
    override protected def defined(board: Board, hand: Hand): Boolean = true
    def make: PartialFunction[(Board, Hand), Option[PokerCombination]] = {
      case (board, hand) if defined(board, hand) => create(board, hand)
    }
  }

  final case class FullHouse private(pair: Pair, threeOfKind: ThreeOfKind) extends PokerCombination {
    override def weight: Byte = 7
    override def compare(that: PokerCombination): Int = compareByWeight(this, that)
      .getOrElse(0)
  }
  object FullHouse extends PokerCombinationBuilder {
    override protected def create(board: Board, hand: Hand): Option[PokerCombination] = Option.empty
    override protected def defined(board: Board, hand: Hand): Boolean = true
    def make: PartialFunction[(Board, Hand), Option[PokerCombination]] = {
      case (board, hand) if defined(board, hand) => create(board, hand)
    }
  }

  final case class FourOfKind private(first: Card, second: Card, third: Card, fourth: Card, remain: Card)
    extends PokerCombination {
    override def weight: Byte = 8
    override def compare(that: PokerCombination): Int = compareByWeight(this, that).
      getOrElse(0)
  }
  object FourOfKind extends PokerCombinationBuilder {
    override protected def create(board: Board, hand: Hand): Option[PokerCombination] = Option.empty
    override protected def defined(board: Board, hand: Hand): Boolean = true
    def make: PartialFunction[(Board, Hand), Option[PokerCombination]] = {
      case (board, hand) if defined(board, hand) => create(board, hand)
    }
  }

  final case class StraightFlush private(straightFlush: collection.immutable.Set[Card]) extends PokerCombination {
    override def weight: Byte = 9
    override def compare(that: PokerCombination): Int = compareByWeight(this, that)
      .getOrElse(0)
  }
  object StraightFlush extends PokerCombinationBuilder {
    override protected def create(board: Board, hand: Hand): Option[PokerCombination] = Option.empty
    override protected def defined(board: Board, hand: Hand): Boolean = true
    def make: PartialFunction[(Board, Hand), Option[PokerCombination]] = {
      case (board, hand) if defined(board, hand) => create(board, hand)
    }
  }
}