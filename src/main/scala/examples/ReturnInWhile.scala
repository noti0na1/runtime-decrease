import scala.util.boundary, boundary.break
object ReturnInWhile:
  def return5()(using DecreaseState): Int =
    boundary {
      while_decreases(true, 0) {
        break(5)
      }
      assert(false, "unreachable code")
    }

  def returnN(n: Int)(using DecreaseState): Int =
    require(n >= 0)
    var i = 0
    boundary {
      while_decreases(true, n - i) {
        if n > 100 then break(n)
        else if i == n then break(i)
        else i += 1
      }
      assert(false, "unreachable code")
    }

  def return10()(using DecreaseState): Int =
    boundary {
      while_decreases(true, 0) {
        def f: Int = return 20
        break(10)
      }
      assert(false, "unreachable code")
    }

  def return20()(using DecreaseState): Int =
    var x = 0
    boundary {
      while_decreases(true, 0) {
        def f: Int = return 20
        x = f
        break(x)
      }
      assert(false, "unreachable code")
    }
