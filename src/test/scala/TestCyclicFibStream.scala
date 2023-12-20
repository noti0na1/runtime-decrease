import org.scalacheck.Properties
import org.scalacheck.Prop.*

object TestCyclicFibStream extends Properties("Test CyclicFibStream"):
  import EmptyDecreaseState.given
  property("cyclic fib stream") = forAll { (n: Int) =>
    (n < 50 && n >= 0) ==> {
      ZipWithAndFibStream.nthFib(n) >= 0
    }
  }
