@main def testDecrease: Unit =
  // println(summon[DefaultValue[(Int, Int)]].value)
  // println(getFunctionName(0))

  given DecreaseState = DecreaseState(Map.empty)

  println(Trees.tree1.mapValue(_ + 1))

  // println(YCombinator.factorialY(10))\

  println(YCombinator.quicksortY(List(6, 2, 3, 0, 3, 6, 7, 8, 9, 1)))

  // println(Ackermann.ack(3, 3))

  // println(DisjunctiveArguments.f(10, 10))

  // println(McCarthy91.M(10))
  // println(McCarthy91.M1(10)) // expect error

object YCombinator:
  def fix[A, B](f: (A => B) => A => B): A => B =
    f(fix(f))(_)

  def factorial(
      f: Int => DecreaseState ?=> Int
  )(n: Int)(using DecreaseState): Int =
    decreases(n):
      if n == 0 then 1
      else n * f(n - 1)

  def factorialY: Int => DecreaseState ?=> Int = fix(factorial)

  def fix2[A, B, C, D](
      f: ((A => B), (C => D)) => A => B,
      g: ((A => B), (C => D)) => C => D
  ): (A => B, C => D) =
    (f(fix2(f, g)._1, fix2(f, g)._2)(_), g(fix2(f, g)._1, fix2(f, g)._2)(_))

  def quicksort(
      qs: List[Int] => List[Int],
      p: ((Int, List[Int], List[Int], List[Int])) => List[Int]
  )(xs: List[Int]): List[Int] =
    xs match
      case Nil => Nil
      case x :: xs =>
        p(x, Nil, Nil, xs)

  def par(
      qs: List[Int] => List[Int],
      p: ((Int, List[Int], List[Int], List[Int])) => List[Int]
  )(x: Int, l: List[Int], r: List[Int], xs: List[Int]): List[Int] =
    xs match
      case Nil => qs(l) ++ (x :: qs(r))
      case y :: ys =>
        if y < x then p(x, y :: l, r, ys)
        else p(x, l, y :: r, ys)

  def quicksortY: List[Int] => List[Int] = fix2[List[Int], List[
    Int
  ], (Int, List[Int], List[Int], List[Int]), List[Int]](quicksort, par)._1

object Ackermann:
  def ack(m: BigInt, n: BigInt)(using DecreaseState): BigInt =
    decreases((m, n)):
      if m == 0 then n + 1
      else if n == 0 then ack(m - 1, 1)
      else ack(m - 1, ack(m, n - 1))

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
