package scala.tasty.internal.dotc
package core

import Symbols._, Types._, util.Positions._, Contexts._, Constants._, ast.tpd._
import StdNames._

object Annotations {

  abstract class Annotation {
    def tree(implicit ctx: Context): Tree
    def symbol(implicit ctx: Context): Symbol =
      if (tree.symbol.isConstructor) tree.symbol.owner
      else tree.tpe.typeSymbol
  }
}
