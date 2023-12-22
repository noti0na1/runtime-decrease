object HOTest:

  // uses higher order function
  def hoRecur(x: BigInt, f: BigInt => BigInt)(using DecreaseState): BigInt =
    decreases(abs(x)):
      if x <= 0 then BigInt(0)
      else
        hoRecur(f(x), f)

  def abs(x: BigInt): BigInt =
    if x < 0 then -x else x
