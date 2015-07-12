package scala.tasty.internal.dotc.util

class DotClass {
  def unsupported(methodName: String): Nothing =
    throw new UnsupportedOperationException(s"$getClass.$methodName")
}
