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

  // def fix2[A, B, C, D](
  //     f: ((A => B), (C => D)) => A => B,
  //     g: ((A => B), (C => D)) => C => D
  // ): (A => B, C => D) =
  //   (f(fix2(f, g)._1, fix2(f, g)._2)(_), g(fix2(f, g)._1, fix2(f, g)._2)(_))

  // def quicksort(
  //     qs: List[Int] => DecreaseState ?=> List[Int],
  //     p: ((Int, List[Int], List[Int], List[Int])) => DecreaseState ?=> List[Int]
  // )(xs: List[Int])(using DecreaseState): List[Int] =
  //   decreases(xs.size):
  //     xs match
  //       case Nil => Nil
  //       case x :: xs =>
  //         p(x, Nil, Nil, xs)

  // def par(
  //     qs: List[Int] => DecreaseState ?=> List[Int],
  //     p: ((Int, List[Int], List[Int], List[Int])) => DecreaseState ?=> List[Int]
  // )(x: Int, l: List[Int], r: List[Int], xs: List[Int])(using DecreaseState): List[Int] =
  //   decreases(l.size + r.size + xs.size, xs.size + 1):
  //     xs match
  //       case Nil => qs(l) ++ (x :: qs(r))
  //       case y :: ys =>
  //         if y < x then p(x, y :: l, r, ys)
  //         else p(x, l, y :: r, ys)

  // def quicksortY: List[Int] => DecreaseState ?=> List[Int] =
  //   fix2(quicksort, par)._1
