case class DecreaseState(
    stack: Map[String, Any] = Map.empty,
    recur_degrees: Map[String, Int] = Map.empty,
    id: Int = 0
):
  def apply(key: String): Option[Any] = stack.get(key)
  def set(key: String, value: Any): DecreaseState =
    this.copy(stack = stack + (key -> value))

  def get_degree(key: String): Option[Int] = recur_degrees.get(key)
  def set_degree(key: String, value: Int): DecreaseState =
    this.copy(recur_degrees = recur_degrees + (key -> value))

  def id_inc: DecreaseState = this.copy(id = id + 1)

def ds(using state: DecreaseState): DecreaseState = state

object EmptyDecreaseState:
  given DecreaseState = DecreaseState(Map.empty)

trait ZeroValue[T]:
  def value: T

def zero[T](using z: ZeroValue[T]): T = z.value

given ZeroValue[Int] with
  def value = 0

given ZeroValue[BigInt] with
  def value = BigInt(0)

import Tuple.*

given ZeroValue[EmptyTuple] with
  def value = EmptyTuple

given [H, T <: Tuple](using
    head: ZeroValue[H],
    tail: ZeroValue[T]
): ZeroValue[H *: T] with
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

def getFunctionName(offset: Int = 0): String =
  val stackTrace = Thread.currentThread.getStackTrace
  val elem = stackTrace(offset + 2)
  elem.getClassName() + "." + elem.getMethodName

def decreases[V: Ordering: ZeroValue, T](x: V)(using DecreaseState)(body: DecreaseState ?=> T) =
  genericDecreases(getFunctionName(1), x)(body)

def while_decreases[V: Ordering: ZeroValue, T](cond: => Boolean, x: => V)(using
    DecreaseState
)(body: DecreaseState ?=> Unit): Unit =
  val name = getFunctionName(1) + "$while" + ds.id
  def recur(using DecreaseState): Unit =
    if cond then genericDecreases(name, x) { body; recur }
  recur(using ds.id_inc)

def genericDecreases[V: Ordering: ZeroValue, T](name: String, x: V)(using DecreaseState)(body: DecreaseState ?=> T) =
  import math.Ordering.Implicits.infixOrderingOps
  // println(s"decrease: ${name}, ${x}")
  if x < zero then
    throw IllegalArgumentException(
      s"decrease called with negative measure: ${x} at ${name}"
    )
  else
    ds(name) match
      case Some(last) =>
        // println(s"last: ${last}, x: ${x}")
        if x >= last.asInstanceOf[V] then
          throw IllegalArgumentException(
            s"decrease measure not decreased: ${last} <= ${x} at ${name}"
          )
      case None =>
      // println(s"first: ${x}")
    body(using ds.set(name, x))
