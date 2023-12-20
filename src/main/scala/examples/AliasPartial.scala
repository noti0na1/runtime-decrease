object AliasPartial:
  def f(x: BigInt)(y: BigInt)(using DecreaseState): BigInt =
    decreases(x):
      if x > 0 then f(x - 1)(y)
      else lambda(y)

  def lambda(x: BigInt)(using DecreaseState): BigInt = x + 1

  def g(x: BigInt)(using DecreaseState) = f(1)(x)
