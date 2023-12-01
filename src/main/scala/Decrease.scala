case class DecreaseState(stack: Map[String, Any]) {
  def apply(key: String): Option[Any] = stack.get(key)
  def set(key: String, value: Any): DecreaseState = DecreaseState(
    stack + (key -> value)
  )
}

trait WellOrdering[T: Ordering]:
  def zero: T

given WellOrdering[Int] with
  def zero = 0

given WellOrdering[BigInt] with
  def zero = BigInt(0)

import Tuple.*

given Ordering[EmptyTuple] with
  def compare(x: EmptyTuple, y: EmptyTuple): Int = 0

given [T <: NonEmptyTuple](using
    headOrd: Ordering[Head[T]],
    tailOrd: Ordering[Tail[T]]
): Ordering[T] with
  def compare(x: T, y: T): Int =
    val cmp = headOrd.compare(x.head, y.head)
    if cmp == 0 then tailOrd.compare(x.tail, y.tail)
    else cmp

given WellOrdering[EmptyTuple] with
  def zero = EmptyTuple

given [T <: NonEmptyTuple](using
    headOrd: Ordering[Head[T]],
    headWO: WellOrdering[Head[T]],
    tailOrd: Ordering[Tail[T]],
    tailWO: WellOrdering[Tail[T]]
): WellOrdering[T] with
  def zero: T = (headWO.zero *: tailWO.zero).asInstanceOf[T]

import math.Ordering.Implicits.infixOrderingOps

inline def decreases[V: Ordering, T](name: String, x: V)(using
    state: DecreaseState,
    ord: WellOrdering[V]
)(inline body: DecreaseState ?=> T) =
  if x < ord.zero then
    throw IllegalArgumentException(
      s"decrease called with negative measure: ${x}"
    )
  else
    state(name) match
      case Some(last) =>
        // println(s"last: ${last}, x: ${x}")
        if x >= last.asInstanceOf[V] then
          throw IllegalArgumentException(
            s"decrease measure not decreased: ${last} <= ${x}"
          )
      case None =>
      // println(s"first: ${x}")
    body(using state.set(name, x))
