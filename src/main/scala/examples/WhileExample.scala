object While {

  def foo()(using
      DecreaseState
  ): Int = {
    var a = 0
    var i = 0
    (while(i < 10) {
      decreases(10 - i)
      a = a + i
      i = i + 1
    })
    a
  }
}