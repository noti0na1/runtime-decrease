enum MyList[+A]:
  case Cons(head: A, tail: MyList[A])
  case Empty

  def size: Int = this match
    case Cons(_, tail) => 1 + tail.size
    case Empty         => 0

  def map_recur[B](f: A => DecreaseState ?=> B)(using
      DecreaseState
  ): MyList[B] =
    decreases(ds.get_degree("MyList.map").get, this.size):
      this match
        case Cons(head, tail) => Cons(f(head), tail.map_recur(f))
        case Empty            => Empty

  def map[B](degree: Int, f: A => DecreaseState ?=> B)(using
      DecreaseState
  ): MyList[B] =
    map_recur(f)(using ds.set_degree("MyList.map", degree))

  def foldLeft_recur[B](z: B)(f: (B, A) => DecreaseState ?=> B)(using
      DecreaseState
  ): B =
    decreases(ds.get_degree("MyList.foldLeft").get, this.size):
      this match
        case Cons(head, tail) => tail.foldLeft_recur(f(z, head))(f)
        case Empty            => z

  def foldLeft[B](degree: Int, z: B)(f: (B, A) => DecreaseState ?=> B)(using
      DecreaseState
  ): B =
    foldLeft_recur(z)(f)(using ds.set_degree("MyList.foldLeft", degree))

case class Tree(v: Int, height: Int, children: MyList[Tree]):
  def mapValue(f: Int => Int)(using DecreaseState): Tree =
    decreases(height):
      Tree(f(v), height, children.map(height, _.mapValue(f)))

object Trees:
  import MyList.*
  val tree1 = Tree(
    1,
    3,
    Cons(
      Tree(2, 2, Cons(Tree(3, 1, Empty), Cons(Tree(3, 1, Empty), Empty))),
      Empty
    )
  )
