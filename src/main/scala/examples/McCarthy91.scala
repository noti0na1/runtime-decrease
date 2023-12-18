object McCarthy91:
  def rank(n: BigInt): BigInt = {
    if (n <= 100)
      101 - n
    else
      BigInt(0)
  } ensuring (_ >= 0)

  def M(n: BigInt)(using DecreaseState): BigInt =
    decreases(rank(n)) {
      if (n > 100)
        n - 10
      else
        M(M(n + 11))
    } ensuring (res => res == (if (n <= 100) BigInt(91) else n - 10))

  def M1(n: BigInt)(using DecreaseState): BigInt =
    decreases(n) { // fails run-time check
      if (n > 100)
        n - 10
      else
        M1(M1(n + 11))
    } ensuring (res => res == (if (n <= 100) BigInt(91) else n - 10))
    