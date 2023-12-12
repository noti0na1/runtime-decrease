import org.scalacheck.Properties
import org.scalacheck.Prop.*
import org.scalacheck.Gen
import org.scalacheck.Arbitrary

object TestModel extends Properties("Test Model") {
  import EmptyDecreaseState.given

  property("correctness of max operator") = forAll { (arr: List[Int]) =>
    !arr.isEmpty ==> {
        
      Model.max(arr) == arr.max
    }
  }
}
