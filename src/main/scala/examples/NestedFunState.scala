object NestedFunState {

    def sum(n: BigInt)(using DecreaseState): BigInt = {
        require(n > 0)
        var i = BigInt(0)
        var res = BigInt(0)

        def iter()(using DecreaseState): Unit = {
            require(res >= i && i >= 0 && n >= i)
            decreases(n - i) {
                if(i < n) {
                    i += 1
                    res += i
                    iter()
                }
            }
        }

        iter()
        res
    }

    def counterN(n: Int)(using DecreaseState): Int = {
        require(n > 0)

        var counter = 0

        def inc(): Unit = {
            counter += 1
        }

        var i = 0
        while (i < n) {
            loop_decreases("while0", n - i)
            inc()
            i += 1
        }

        counter
    }

    def foo()(using DecreaseState): Int = {
        var i = 0
        def getI = i

        def rec()(using DecreaseState): Unit = {
            require(getI >= 0 && getI <= 10)
            decreases(10 - i) {
                if (i < 10) {
                    i += 1
                    rec()
                }
            }
        }

        rec()

        i
    }
}
