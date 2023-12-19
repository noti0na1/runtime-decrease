import scala.util.boundary, boundary.break

object FindIndex:

  def findIndex[T](a: Array[T], t: T)(using
      DecreaseState
  ): Int =
    var i: Int = 0
    boundary:
      while_decreases(i < a.length, a.length - i) {
        if a(i) == t then break(i)
        i += 1
      }
    i
