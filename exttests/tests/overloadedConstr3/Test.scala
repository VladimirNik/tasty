class Y(x: String) {
  def this(x: Double, y: String) = this(y)
}
class Test(y: String) extends Y(5.0, y) { self =>
  def this(x: Int, y: String, z: Double) = this("ff")
  def this() = this("gg")
  def this(y: String, z: Double) = this(5, y, z)
}