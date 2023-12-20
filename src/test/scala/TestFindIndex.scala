import org.scalacheck.Properties
import org.scalacheck.Prop.*
import org.scalacheck.Gen
import org.scalacheck.Arbitrary

object TestFindIndex extends Properties("Test FindIndex"):
  import EmptyDecreaseState.given

  property("correctness of search") = forAll { (arr: Array[Int]) =>
    !arr.isEmpty ==> {
      val x = arr(scala.util.Random.nextInt(arr.length))
      FindIndex.findIndex(arr, x) == arr.indexOf(x)
    }
  }
