class L
class Test[X, M/* <: X*/, U <: L] extends Object {
  type Y = String
  type Z
  type Q <: X
  type W >: L
  def this(x: Int) = {this(); null.asInstanceOf[X]; }
  def test = null.asInstanceOf[X]
  def this(x: Int, y: Int) = { this(y); null.asInstanceOf[X]; }
}

