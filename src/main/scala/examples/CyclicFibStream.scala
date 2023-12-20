object ZipWithAndFibStream:
  /** An infinite integer stream. Technically, the data structure is *not*
    * infinite but the tail has a higher-order function.
    */
  case class SCons(x: BigInt, tailFun: () => SCons):
    lazy val tail = tailFun()

  /** A generic higher-order `zipWithFun` function.
    */
  private def zipWithFun(f: (BigInt, BigInt) => BigInt, xs: SCons, ys: SCons): SCons =
    (xs, ys) match
      case (SCons(x, _), SCons(y, _)) =>
        SCons(f(x, y), () => zipWithFun(f, xs.tail, ys.tail))

  def nthElem(n: BigInt, s: SCons)(using DecreaseState): BigInt =
    require(n >= 0)
    decreases(n):
      if n == 0 then s.x
      else
        nthElem(n - 1, s.tail)

  /** Using a `zipWithFun` function to implement a fibonacci stream.
    */
  lazy val fibstream: SCons = SCons(
    0,
    () =>
      SCons(
        1,
        () =>
          zipWithFun(_ + _, this.fibstream, this.fibstream.tail)
      )
  )

  def nthFib(n: BigInt)(using DecreaseState) =
    require(n >= 0)
    nthElem(n, fibstream)
