class X {
  type v
}
trait Y
class U[H]
class Test[O] {
  type Z = List[Double]
  private[this] val x: X { type v = U[Y]; val a: v;  val b: Z; val c: O } = null
}
