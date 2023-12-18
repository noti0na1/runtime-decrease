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
        else y = y - 1
    (x, y)
