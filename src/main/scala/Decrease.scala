case class DecreaseState(stack: Map[String, Any]) {
  def apply(key: String): Option[Any] = stack.get(key)
  def set(key: String, value: Any): DecreaseState = DecreaseState(
    stack + (key -> value)
  )
}

trait DefaultValue[T]:
  def value: T

given DefaultValue[Int] with
  def value = 0

given DefaultValue[BigInt] with
  def value = BigInt(0)

import Tuple.*

given DefaultValue[EmptyTuple] with
  def value = EmptyTuple

given [H, T <: Tuple](using
    head: DefaultValue[H],
    tail: DefaultValue[T]
): DefaultValue[H *: T] with
  def value: H *: T = head.value *: tail.value

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

import math.Ordering.Implicits.infixOrderingOps

def getFunctionName(offset: Int = 0): String = {
  val stackTrace = Thread.currentThread.getStackTrace
  stackTrace(offset + 2).getMethodName
}

def decreases[V: Ordering, T](x: V)(using
    state: DecreaseState,
    default: DefaultValue[V]
)(body: DecreaseState ?=> T) =
  genericDecreases(
    getFunctionName(1),
    x
  )(body)

def loop_decreases[V: Ordering, T](label: String, x: V)(using
    state: DecreaseState,
    default: DefaultValue[V])(body: DecreaseState ?=> T) =
  genericDecreases(
    getFunctionName(1) + "-" + label,
    x
  )(body)

def genericDecreases[V: Ordering, T](name: String, x: V)(using
    state: DecreaseState,
    default: DefaultValue[V]
)(body: DecreaseState ?=> T) =
  // println(s"decrease: ${name}, ${x}")
  if x < default.value then
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
