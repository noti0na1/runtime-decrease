import org.scalacheck.Properties
import org.scalacheck.Prop.*

object TestAliasPartial extends Properties("Test Alias Partial"):
  import EmptyDecreaseState.given
  property("alias partial") = forAll { (x: BigInt) =>
    AliasPartial.g(x)
    true
  }
