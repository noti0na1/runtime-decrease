object NNF:
  sealed abstract class Formula
  case class And(lhs: Formula, rhs: Formula) extends Formula
  case class Or(lhs: Formula, rhs: Formula) extends Formula
  case class Implies(lhs: Formula, rhs: Formula) extends Formula
  case class Not(f: Formula) extends Formula
  case class Literal(id: BigInt) extends Formula

  def size(formula: Formula): Int = formula match
    case And(lhs, rhs)     => 1 + size(lhs) + size(rhs)
    case Or(lhs, rhs)      => 1 + size(lhs) + size(rhs)
    case Implies(lhs, rhs) => 1 + size(lhs) + size(rhs)
    case Not(f)            => 1 + size(f)
    case Literal(_)        => 1

  def simplify(f: Formula)(using DecreaseState): Formula =
    decreases(size(f)) {
      f match
        case And(lhs, rhs)     => And(simplify(lhs), simplify(rhs))
        case Or(lhs, rhs)      => Or(simplify(lhs), simplify(rhs))
        case Implies(lhs, rhs) => Or(Not(simplify(lhs)), simplify(rhs))
        case Not(f)            => Not(simplify(f))
        case Literal(_)        => f
    }

  def isSimplified(f: Formula)(using DecreaseState): Boolean =
    decreases(size(f)) {
      f match
        case And(lhs, rhs) => isSimplified(lhs) && isSimplified(rhs)
        case Or(lhs, rhs)  => isSimplified(lhs) && isSimplified(rhs)
        case Implies(_, _) => false
        case Not(f)        => isSimplified(f)
        case Literal(_)    => true
    }

  def nnf(formula: Formula)(using DecreaseState): Formula =
    decreases(size(formula)) {
      formula match
        case And(lhs, rhs)          => And(nnf(lhs), nnf(rhs))
        case Or(lhs, rhs)           => Or(nnf(lhs), nnf(rhs))
        case Implies(lhs, rhs)      => Or(nnf(Not(lhs)), nnf(rhs))
        case Not(And(lhs, rhs))     => Or(nnf(Not(lhs)), nnf(Not(rhs)))
        case Not(Or(lhs, rhs))      => And(nnf(Not(lhs)), nnf(Not(rhs)))
        case Not(Implies(lhs, rhs)) => And(nnf(lhs), nnf(Not(rhs)))
        case Not(Not(f))            => nnf(f)
        case Not(Literal(_))        => formula
        case Literal(_)             => formula
    }

  def isNNF(f: Formula)(using DecreaseState): Boolean =
    decreases(size(f)) {
      f match
        case And(lhs, rhs)     => isNNF(lhs) && isNNF(rhs)
        case Or(lhs, rhs)      => isNNF(lhs) && isNNF(rhs)
        case Implies(lhs, rhs) => false
        case Not(Literal(_))   => true
        case Not(_)            => false
        case Literal(_)        => true
    }

  def evalLit(id: BigInt): Boolean = (id == 42) // could be any function

  def eval(f: Formula)(using DecreaseState): Boolean =
    decreases(size(f)) {
      f match
        case And(lhs, rhs)     => eval(lhs) && eval(rhs)
        case Or(lhs, rhs)      => eval(lhs) || eval(rhs)
        case Implies(lhs, rhs) => !eval(lhs) || eval(rhs)
        case Not(f)            => !eval(f)
        case Literal(id)       => evalLit(id)
    }
