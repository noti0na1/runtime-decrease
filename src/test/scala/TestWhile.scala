import org.scalacheck.Properties
import org.scalacheck.Prop.*
import org.scalacheck.Gen
import org.scalacheck.Arbitrary

object TestWhile extends Properties("Test While") {
  import EmptyDecreaseState.given

  property("correctness of while decrease") = While.foo() == 45
}