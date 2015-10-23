class Test[T](k: Int, m: Int)(j: String) extends Object {
  def this(k: Int)(j: String)(f: Float) = { this(k, k)(j); println() }
  def this() = this(5)("jj")(42.0F)
}