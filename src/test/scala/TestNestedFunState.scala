import org.scalacheck.Properties
import org.scalacheck.Prop.*
import org.scalacheck.Gen
import org.scalacheck.Arbitrary

object TestNestedFunState extends Properties("Test NestedFunState") {
    import EmptyDecreaseState.given

    val lowerInts = for (n <- Gen.choose(0, 1000)) yield n

    property("correctness of sum") = forAll(lowerInts){ (n: Int) => 
        n > 0 ==> {
            NestedFunState.sum(n) >= n
        }
    }

    property("correctness of counterN") = forAll(lowerInts) { (n: Int) =>
        n > 0 ==> {
            NestedFunState.counterN(n) == n
        }    
    }

    property("correctness of foo") = NestedFunState.foo() == 10
}
