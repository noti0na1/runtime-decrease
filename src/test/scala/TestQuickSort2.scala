import org.scalacheck.Properties
import org.scalacheck.Prop.*
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import scala.concurrent.ExecutionContext
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object TestQuickSort2 extends Properties("Test QuickSort2"):

  import EmptyDecreaseState.given
  import ExecutionContext.Implicits.global

  property("correctness of quicksort2") = forAll { (arr: Array[Int]) =>
    val list = arr.toList.map(BigInt(_))
    val sorted = QuickSort2.quickSort(list)
    Await.result(sorted, Duration.Inf) == list.sorted
  }
