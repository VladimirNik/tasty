class Test[T1, T2](x: Int, z: Double)(f: String) {
  def this(x: Int)(z: Double) = this(x, z)("gg")
  def this() = this(5)(42.0)
}