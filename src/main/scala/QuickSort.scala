object QuickSort {

  def isSorted(list: List[BigInt]): Boolean = {
    list match {
      case x::xs => x <= xs.head && isSorted(xs)
      case _ => true
    }
  }

  def appendSorted(l1: List[BigInt], l2: List[BigInt]): List[BigInt] = {
    l1 match {
      case Nil => l2
      case x::xs => x +: appendSorted(xs, l2)
    }
  } 

  def quickSort(list: List[BigInt])(using
      DecreaseState
  ): List[BigInt] = {
    decreases(list.size)
    list match {
      case Nil => Nil
      case x::xs => par(x, Nil, Nil, xs)
    }
  }

  def par(x: BigInt, l: List[BigInt], r: List[BigInt], ls: List[BigInt])(using
      DecreaseState
  ): List[BigInt] = {
    decreases(l.size + r.size + ls.size, ls.size + 1)

    ls match {
      case Nil => appendSorted(quickSort(l), x +: quickSort(r))
      case x2::xs2 => if (x2 <= x) par(x, x2 +: l, r, xs2) else par(x, l, x2 +: r, xs2)
    }
  }
}
