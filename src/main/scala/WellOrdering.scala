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
