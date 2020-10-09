package adt

sealed trait Suit
object Suit {
  case object Clubs extends Suit
  case object Diamonds extends Suit
  case object Hearts extends Suit
  case object Spades extends Suit
}