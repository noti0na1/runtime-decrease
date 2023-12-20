object OddEven {

    def isOdd(n: BigInt)(using DecreaseState): Boolean = {
        require(n >= 0)
        decreases(n) {
            if (n == 0) false
            else isEven(n-1)
        }
    }

    def isEven(n: BigInt)(using DecreaseState): Boolean = {
        require(n >= 0)
        decreases(n) {
            if (n == 0) true
            else isOdd(n-1)
        }
    }

    def isOdd2(n: BigInt)(using DecreaseState): Boolean = {
        require(n >= 0)
        decreases(n, 0) {
            if (n == 0) false
            else isEven2(n-1)
        }
    }

    def isEven2(n: BigInt)(using DecreaseState): Boolean = {
        require(n >= 0)
        decreases(n, 1) {
            if (n == 0) true
            else isOdd2(n-1)
        }
    }
}


