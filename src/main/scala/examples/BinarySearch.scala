object BinarySearch:

  def search(arr: Array[Int], x: Int, lo: Int, hi: Int)(using
      DecreaseState
  ): Boolean =
    require(0 <= lo && hi < arr.length && lo <= hi + 1)
    decreases(hi - lo + 1):
      if lo <= hi then
        val i = lo + (hi - lo) / 2
        val y = arr(i)
        if x == y then true
        else if x < y then search(arr, x, lo, i - 1)
        else search(arr, x, i + 1, hi)
      else false
