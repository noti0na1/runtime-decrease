import org.scalacheck.Properties
import org.scalacheck.Prop.*
import org.scalacheck.Gen
import org.scalacheck.Arbitrary

object TestHOTest extends Properties("Test HOTest") {
  import EmptyDecreaseState.given

  property("correctness of higher order function recursion") = forAll { (arr: Array[Int]) =>
    !arr.isEmpty ==> {
      HOTest.hoRecur(arr.length, (x: BigInt) => x - 1) == BigInt(0)
    }
  }
}
