package adt

sealed trait Rank {
  def weight: Short
}
object Rank {
  case object Two extends Rank {
    override def weight: Short = 2
  }

  case object Three extends Rank {
    override def weight: Short = 3
  }

  case object Four extends Rank {
    override def weight: Short = 4
  }

  case object Five extends Rank {
    override def weight: Short = 5
  }

  case object Six extends Rank {
    override def weight: Short = 6
  }

  case object Seven extends Rank {
    override def weight: Short = 7
  }

  case object Eight extends Rank {
    override def weight: Short = 8
  }

  case object Nine extends Rank {
    override def weight: Short = 9
  }

  case object Ten extends Rank {
    override def weight: Short = 10
  }

  case object Jack extends Rank {
    override def weight: Short = 11
  }

  case object Queen extends Rank {
    override def weight: Short = 12
  }

  case object King extends Rank {
    override def weight: Short = 13
  }

  case object Ace extends Rank {
    override def weight: Short = 14
  }
}