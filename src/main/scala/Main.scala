@main def testDecrease: Unit =
  println(summon[DefaultValue[(Int, Int)]].value)
  // println(getFunctionName(0))

  given DecreaseState = DecreaseState(Map.empty)

  println(DisjunctiveArguments.f(10, 10))

  println(McCarthy91.M(10))
  println(McCarthy91.M1(10))

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

object DisjunctiveArguments:

  def f(x0: Int, y0: Int)(using DecreaseState): (Int, Int) =
    var x = x0
    var y = y0
    val rand = new scala.util.Random

    // No function into the natural numbers exists that suffices to prove
    // termination; instead we must resort to a lexicographic ranking function:
    // (x, y)
    while (x > 0) && (y > 0) do
      loop_decreases("while0", (x, y)):
        println(s"while x: ${x}, y: ${y}")
        if rand.nextInt(2) == 1 then
          x = x - 1
          y = y + 1
          // McCarthy91.M(y)
        else y = y - 1
    (x, y)
