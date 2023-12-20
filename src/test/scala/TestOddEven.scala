import org.scalacheck.Properties
import org.scalacheck.Prop.*
import org.scalacheck.Gen
import org.scalacheck.Arbitrary

object TestOddEven extends Properties("Test OddEven") {
    import EmptyDecreaseState.given

    property("correctness of isOdd") = forAll { (n: BigInt) =>
        n >= 0 ==> {
            OddEven.isOdd(n) == ((n % 2) == 1)
        }
    }

    property("correctness of isEven") = forAll { (n: BigInt) =>
        n >= 0 ==> {
            OddEven.isEven(n) == ((n % 2) == 0)
        }
    }

    property("correctness of isOdd2") = forAll { (n: BigInt) =>
        n >= 0 ==> {
            OddEven.isOdd2(n) == ((n % 2) == 1)
        }
    }

    property("correctness of isEven2") = forAll { (n: BigInt) =>
        n >= 0 ==> {
            OddEven.isEven2(n) == ((n % 2) == 0)
        }
    }
}
