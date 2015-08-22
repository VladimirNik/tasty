class X {
  type z
}
trait Y
trait Z
class Test {
  self: X with Y =>
  private[this] val a: X with Y with Z = null
  private[this] val b: X with Z { val c: Int; type z = String; val d: z } = null
}
