case class DecreaseState(
    stack: Map[String, Any] = Map.empty
):
  def apply(key: String): Option[Any] = stack.get(key)
  def set(key: String, value: Any): DecreaseState =
    DecreaseState(stack + (key -> value))

def ds(using state: DecreaseState): DecreaseState = state

object EmptyDecreaseState:
  given DecreaseState = DecreaseState(Map.empty)

class NegativeMeasureException(msg: String) extends IllegalArgumentException(msg)

class NonDecreasingMeasureException(msg: String) extends IllegalArgumentException(msg)

def decreases[V: Ordering: ZeroValue, T](x: V)(using
    s: DecreaseState,
    path: sourcecode.Enclosing
)(body: DecreaseState ?=> T) =
  genericDecreases(path.value, x)(body)

def while_decreases[V: Ordering: ZeroValue, T](cond: => Boolean, x: => V)(using
    s: DecreaseState,
    path: sourcecode.Enclosing,
    line: sourcecode.Line
)(body: DecreaseState ?=> Unit): Unit =
  val name = path.value + "$while:" + line.value
  def recur(using DecreaseState): Unit =
    if cond then genericDecreases(name, x) { body; recur }
  recur

def genericDecreases[V: Ordering: ZeroValue, T](name: String, x: V)(using DecreaseState)(body: DecreaseState ?=> T) =
  import math.Ordering.Implicits.infixOrderingOps
  // println(s"decrease: ${name}, ${x}")
  if x < zero then
    throw NegativeMeasureException(
      s"decrease called with negative measure: ${x} at ${name}"
    )
  else
    ds(name) match
      case Some(last) =>
        // println(s"last: ${last}, x: ${x}")
        if x >= last.asInstanceOf[V] then
          throw NonDecreasingMeasureException(
            s"decrease measure not decreased: ${last} <= ${x} at ${name}"
          )
      case None =>
      // println(s"first: ${x}")
    body(using ds.set(name, x))
