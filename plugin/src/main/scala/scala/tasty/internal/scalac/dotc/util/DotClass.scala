package scala.tasty.internal.scalac.dotc
package util

/** Adds standard functionality to a class.
 *  For now: Just the `unsupported` method.
 */
class DotClass {

  /** Throws an `UnsupportedOperationException` with the given method name. */
  def unsupported(methodName: String): Nothing =
    throw new UnsupportedOperationException(s"$getClass.$methodName")

}
