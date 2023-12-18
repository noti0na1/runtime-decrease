import scala.util.boundary, boundary.break
object ReturnInWhile {
    def return5()(using DecreaseState): Int = {
        boundary {
            while(true) {
                loop_decreases("while0", 0) {
                    break(5)
                }
            }
        }
        assert(false, "unreachable code")
        0
    }

    def returnN(n: Int)(using DecreaseState): Int = {
        require(n >= 0)
        var i = 0
        boundary {
            while(true) {
                loop_decreases("while0", n - i) {
                    if (i == n) break(i)
                    else i += 1
                }
            }
        }
        assert(false, "unreachable code")
        0
    }
}