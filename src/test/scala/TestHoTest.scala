import org.scalacheck.Properties
import org.scalacheck.Prop.*
import org.scalacheck.Gen
import org.scalacheck.Arbitrary

object TestHOTest extends Properties("Test HOTest"):
  import EmptyDecreaseState.given

  property("correctness of higher order function recursion") =
    HOTest.hoRecur(10, (x: BigInt) => x - 1) == BigInt(0)

  property("correctness of higher order function recursion where no decrease") =
    try
      HOTest.hoRecur(10, (x: BigInt) => x) == BigInt(0)
      false
    catch
      case e: IllegalArgumentException => true
      case _                           => false
