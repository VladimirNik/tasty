package scala.tasty.internal.scalac
package dotc
package ast

import core._
import util.Positions._, Types._, Contexts._, Constants._, Names._, Flags._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._, Symbols._
import Denotations._, Decorators._
import config.Printers._
import collection.mutable

import scala.annotation.tailrec

/** Some creators for typed trees */
object tpd extends Trees.Instance[Type] /* with TypedTreeInfo */{

  /** A function def
   *
   *    vparams => expr
   *
   *  gets expanded to
   *
   *    { def $anonfun(vparams) = expr; Closure($anonfun) }
   *
   *  where the closure's type is the target type of the expression (FunctionN, unless
   *  otherwise specified).
   */
//  def Closure(meth: TermSymbol, rhsFn: List[List[Tree]] => Tree, targs: List[Tree] = Nil, targetType: Type = NoType)(implicit ctx: Context): Block = {
//    val targetTpt = if (targetType.exists) TypeTree(targetType) else EmptyTree
//    val call =
//      if (targs.isEmpty) Ident(TermRef(NoPrefix, meth))
//      else TypeApply(Ident(TermRef(NoPrefix, meth)), targs)
//    Block(
//      DefDef(meth, rhsFn) :: Nil,
//      Closure(Nil, call, targetTpt))
//  }
}

