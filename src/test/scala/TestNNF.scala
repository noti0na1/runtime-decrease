import org.scalacheck.Properties
import org.scalacheck.Prop.*
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import NNF.*

// import io.github.martinhh.derived.scalacheck.given

object TestNNF extends Properties("Test NNF"):
  import EmptyDecreaseState.given

  implicit val arbitraryFormula: Arbitrary[Formula] = Arbitrary(Gen.sized(genFormula(_)))

  // Generate an arbitrary Formula with a bounded size
  def genFormula(maxSize: Int): Gen[Formula] =
    if maxSize <= 0 then genLiteral
    else
      Gen.oneOf(
        genAnd(maxSize - 1),
        genOr(maxSize - 1),
        genImplies(maxSize - 1),
        genNot(maxSize - 1),
        genLiteral
      )

  // Generate an arbitrary And formula with a bounded size
  def genAnd(maxSize: Int): Gen[And] =
    for
      lhs <- genFormula(maxSize / 2)
      rhs <- genFormula(maxSize / 2)
    yield And(lhs, rhs)

  // Generate an arbitrary Or formula with a bounded size
  def genOr(maxSize: Int): Gen[Or] =
    for
      lhs <- genFormula(maxSize / 2)
      rhs <- genFormula(maxSize / 2)
    yield Or(lhs, rhs)

  // Generate an arbitrary Implies formula with a bounded size
  def genImplies(maxSize: Int): Gen[Implies] =
    for
      lhs <- genFormula(maxSize / 2)
      rhs <- genFormula(maxSize / 2)
    yield Implies(lhs, rhs)

  // Generate an arbitrary Not formula with a bounded size
  def genNot(maxSize: Int): Gen[Not] =
    for
      f <- genFormula(maxSize - 1)
    yield Not(f)

  // Generate an arbitrary Literal formula
  def genLiteral: Gen[Literal] =
    for
      id <- Gen.choose(Long.MinValue, Long.MaxValue)
    yield Literal(BigInt(id))

  property("simplify correctness") = forAll { (formula: Formula) =>
    val simplified = simplify(formula)
    isSimplified(simplified)
  }

  property("simplify evaluation") = forAll { (formula: Formula) =>
    val simplified = simplify(formula)
    eval(formula) == eval(simplified)
  }

  property("nnf correctness") = forAll { (formula: Formula) =>
    val _nnf = nnf(formula)
    isNNF(_nnf)
  }

  property("nnf evaluation") = forAll { (formula: Formula) =>
    // val res = eval(formula)
    // res || !res
    eval(formula) == eval(nnf(formula))
  }
