trait X
class Y(x: Int) extends Object { self =>
  def this(x: Int, y: Int) = this(x)
}
class Test(y: Int) extends Y(y, 5) with X { self => }