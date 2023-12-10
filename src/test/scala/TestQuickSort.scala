import org.scalacheck.Properties
import org.scalacheck.Prop.*
import org.scalacheck.Gen
import org.scalacheck.Arbitrary

object TestQuickSort extends Properties("Test TestQuickSort") {
  import EmptyDecreaseState.given

  property("correctness of quicksort") = forAll { (arr: Array[Int]) =>
    val list = arr.toList.map(BigInt(_))
    val sorted = QuickSort.quickSort(list)
    sorted == list.sorted
  }
}
