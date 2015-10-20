import scala.runtime.ScalaRunTime
class Test extends AnyRef with Product with Serializable {
  def copy(): Test = new Test();
  override def productPrefix: String = "Test";
  def productArity: Int = 0;
  def productElement(x$1: Int): Any = x$1 match {
    case _ => ???
  };
  override def productIterator: Iterator[Any] = runtime.ScalaRunTime.typedProductIterator[Any](Test.this);
  def canEqual(x$1: Any): Boolean = x$1.isInstanceOf[Test];
  override def hashCode(): Int = ScalaRunTime._hashCode(Test.this);
  override def toString(): String = ScalaRunTime._toString(Test.this);
  override def equals(x$1: Any): Boolean = (x$1 match {
    case (_: Test) => true
    case _ => false
  }).&&(x$1.asInstanceOf[Test].canEqual(Test.this))
};
object Test extends scala.runtime.AbstractFunction0[Test] with Serializable {
  final override def toString(): String = "Test";
  def apply(): Test = new Test();
  def unapply(x$0: Test): Boolean = if (x$0.==(null))
    false
  else
    true;
  private def readResolve(): Object = Test
}