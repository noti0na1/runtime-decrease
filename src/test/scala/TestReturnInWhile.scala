import org.scalacheck.Properties
import org.scalacheck.Prop.*
import org.scalacheck.Gen
import org.scalacheck.Arbitrary

object TestReturnInWhile extends Properties("Test ReturnInWhile") {
    import EmptyDecreaseState.given

    property("correctness of return5") = ReturnInWhile.return5() == 5

    property("correctness of returnN") = forAll { (n: Int) =>
        n >= 0 ==> {
            ReturnInWhile.returnN(n) == n
        }    
    }

    property("correctness of return10") = ReturnInWhile.return10() == 10

    property("correctness of return20") = ReturnInWhile.return20() == 20
}