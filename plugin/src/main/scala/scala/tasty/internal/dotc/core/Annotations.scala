package scala.tasty.internal
package dotc
package core

trait TAnnotations {
  self: API =>
    
  import Symbols._, Types._, util.Positions._, Contexts._, Constants._, ast.tpd._
  import StdNames._

  object Annotations {

    abstract class Annotation {
      def tree: Tree
      def symbol: Symbol =
        if (tree.symbol.isConstructor) tree.symbol.owner
        else tree.tpe.typeSymbol
    }
  }
}
