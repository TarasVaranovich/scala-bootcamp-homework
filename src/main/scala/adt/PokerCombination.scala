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

  private def makeCombination(defined: (Board, Hand) => Boolean,
                              create: (Board, Hand) => Option[PokerCombination]):
  PartialFunction[(Board, Hand), Option[PokerCombination]] = {
    case (board, hand) if defined.apply(board, hand) => create.apply(board, hand)
  }

  final case class HighCard private(card: Card, remains: Set[Card])
    extends PokerCombination {
    override def weight: Byte = 1
    override def compare(that: PokerCombination): Int = compareByWeight(this, that)
      .getOrElse(0)
  }
  object HighCard {
    private val create = (_: Board, _: Hand) => Option.empty
    private val defined = (_: Board, _: Hand) => true
    def make: PartialFunction[(Board, Hand), Option[PokerCombination]] = makeCombination(defined, create)
  }

  final case class Pair private(first: Card, second: Card, remains: Set[Card])
    extends PokerCombination {
    override def weight: Byte = 2
    override def compare(that: PokerCombination): Int = compareByWeight(this, that)
      .getOrElse(0)
  }
  object Pair {
    private val create = (_: Board, _: Hand) => Option.empty
    private val defined = (_: Board, _: Hand) => true
    def make: PartialFunction[(Board, Hand), Option[PokerCombination]] = makeCombination(defined, create)
  }

  final case class TwoPairs private(firstPair: Pair, secondPair: Pair, remain: Card)
    extends PokerCombination {
    override def weight: Byte = 3
    override def compare(that: PokerCombination): Int = compareByWeight(this, that)
      .getOrElse(0)
  }
  object TwoPairs {
    private val create = (_: Board, _: Hand) => Option.empty
    private val defined = (_: Board, _: Hand) => true
    def make: PartialFunction[(Board, Hand), Option[PokerCombination]] = makeCombination(defined, create)
  }

  final case class ThreeOfKind private(first: Card, second: Card, third: Card, remains: Set[Card])
    extends PokerCombination {
    override def weight: Byte = 4
    override def compare(that: PokerCombination): Int = compareByWeight(this, that)
      .getOrElse(0)
  }
  object ThreeOfKind {
    private val create = (_: Board, _: Hand) => Option.empty
    private val defined = (_: Board, _: Hand) => true
    def make: PartialFunction[(Board, Hand), Option[PokerCombination]] = makeCombination(defined, create)
  }

  final case class Straight private(straight: Set[Card]) extends PokerCombination {
    override def weight: Byte = 5
    override def compare(that: PokerCombination): Int = compareByWeight(this, that)
      .getOrElse(0)
  }
  object Straight {
    private val create = (_: Board, _: Hand) => Option.empty
    private val defined = (_: Board, _: Hand) => true
    def make: PartialFunction[(Board, Hand), Option[PokerCombination]] = makeCombination(defined, create)
  }

  final case class Flush private(flush: Set[Card]) extends PokerCombination {
    override def weight: Byte = 6
    override def compare(that: PokerCombination): Int = compareByWeight(this, that)
      .getOrElse(0)
  }
  object Flush {
    private val create = (_: Board, _: Hand) => Option.empty
    private val defined = (_: Board, _: Hand) => true
    def make: PartialFunction[(Board, Hand), Option[PokerCombination]] = makeCombination(defined, create)
  }

  final case class FullHouse private(pair: Pair, threeOfKind: ThreeOfKind) extends PokerCombination {
    override def weight: Byte = 7
    override def compare(that: PokerCombination): Int = compareByWeight(this, that)
      .getOrElse(0)
  }
  object FullHouse {
    private val create = (_: Board, _: Hand) => Option.empty
    private val defined = (_: Board, _: Hand) => true
    def make: PartialFunction[(Board, Hand), Option[PokerCombination]] = makeCombination(defined, create)
  }

  final case class FourOfKind private(first: Card, second: Card, third: Card, fourth: Card, remain: Card)
    extends PokerCombination {
    override def weight: Byte = 8
    override def compare(that: PokerCombination): Int = compareByWeight(this, that).
      getOrElse(0)
  }
  object FourOfKind {
    private val create = (_: Board, _: Hand) => Option.empty
    private val defined = (_: Board, _: Hand) => true
    def make: PartialFunction[(Board, Hand), Option[PokerCombination]] = makeCombination(defined, create)
  }

  final case class StraightFlush private(straightFlush: Set[Card]) extends PokerCombination {
    override def weight: Byte = 9
    override def compare(that: PokerCombination): Int = compareByWeight(this, that)
      .getOrElse(0)
  }
  object StraightFlush {
    private val create = (_: Board, _: Hand) => Option.empty
    private val defined = (_: Board, _: Hand) => true
    def make: PartialFunction[(Board, Hand), Option[PokerCombination]] = makeCombination(defined, create)
  }
}