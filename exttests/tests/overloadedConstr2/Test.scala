class Y[V](x: V) {
  val n: V = null.asInstanceOf[V]
  def this(x: V, y: Int) = this(x)
  def this() = this(null.asInstanceOf[V])
}
class Test[T1](y: T1) extends Y(y, 5) { self => }