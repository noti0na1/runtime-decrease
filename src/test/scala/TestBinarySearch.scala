import org.scalacheck.Properties
import org.scalacheck.Prop.*
import org.scalacheck.Gen
import org.scalacheck.Arbitrary

object TestBinarySearch extends Properties("Test BinarySearch"):
  import EmptyDecreaseState.given

  property("correctness of search") = forAll { (arr: Array[Int]) =>
    !arr.isEmpty ==> {
      val x = arr.head
      val sorted = arr.sorted
      BinarySearch.search(sorted, x, 0, arr.length - 1)
    }
  }
