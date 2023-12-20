object While:

  def foo()(using
      DecreaseState
  ): Int =
    var a = 0
    var i = 0
    while_decreases(i < 10, 10 - i):
      a = a + i
      i = i + 1
    a
