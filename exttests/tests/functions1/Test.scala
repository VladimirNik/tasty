class Test extends Object {
  def test(x: Int) = x
  test _ == 5
  private[this] val a = (x: Int) => (y: Double) => x + y
  private[this] val b = (y: String, g: String) => y.toString + g
  private[this] val c: List[Int] = null
  private[this] val d = c map { _.toString }
  private[this] val e = () => "rrr"
  private[this] var x = 0
  private[this] val f = (y: Int) => {x = y}
}
