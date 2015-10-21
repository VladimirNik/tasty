class X { def a = 5 }
trait Y extends X { override def a = 7 }
class Test extends X with Y {
  override def a = { 7 + super.a }
  def b = super[X].a
  def c = super[Y].a
}