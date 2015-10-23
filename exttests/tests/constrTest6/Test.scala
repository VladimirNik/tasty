class Test[T1, T2](x: T1, z: T2)(f: String) {
  def this(x: T1)(z: T2) = this(x, z)("gg")
  def this() = this(5.asInstanceOf[T1])(42.0.asInstanceOf[T2])
}