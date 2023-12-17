object Model:

  def fold(f: (Int, Int) => Int, l: List[Int], a: Int)(using
      DecreaseState
  ): Int = {
    decreases(l.size):
      l match {
        case Nil      => a
        case hd :: tl => f(hd, fold(f, tl, a))
      }
  }

  def max(lst: List[Int])(using
      DecreaseState
  ): Int = {
    lst match {
      case Nil => -1
      case hd :: tl =>
        fold(
          (x, y) => if (x > y) x else y,
          lst,
          hd
        )
    }
  }
