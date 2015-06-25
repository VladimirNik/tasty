trait TestTrait
trait TestTrait2
class Test(y: Int, yy: String)(implicit zx: Double) extends T(5) with TestTrait with TestTrait2{
  class B { def ggg(y: String) = y }
  val r = "ttt"
  val t = "rrr"
  val bbb = new B
  bbb ggg r
}
class T(x: Int)
