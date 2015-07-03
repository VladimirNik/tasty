package scala.tasty.internal.scalac
package dotc
package typer

import ast._
import core._
import Trees._
import Types._
import Contexts._, Decorators._, Denotations._, Symbols._
import Flags._
import util.Positions._
import printing.Showable

object ErrorReporting {

  import tpd._

  def errorTree(tree: untpd.Tree, msg: => String)(implicit ctx: Context): tpd.Tree =
    tree withType errorType(msg, tree.pos)

  def errorType(msg: => String, pos: Position)(implicit ctx: Context): ErrorType = {
    ctx.error(msg, pos)
    ErrorType
  }

  class Errors(implicit ctx: Context) {

    /** An explanatory note to be added to error messages
     *  when there's a problem with abstract var defs */
    def abstractVarMessage(sym: Symbol): String =
      if (sym.underlyingSymbol.is(Mutable))
        "\n(Note that variables need to be initialized to be defined)"
      else ""

    def anonymousTypeMemberStr(tpe: Type) = {
      val kind = tpe match {
          case _: TypeBounds => "type with bounds"
          case _: PolyType | _: MethodType => "method"
          case _ => "value of type"
        }
        d"$kind $tpe"
    }

    def overloadedAltsStr(alts: List[SingleDenotation]) =
      d"overloaded alternatives of ${denotStr(alts.head)} with types\n" +
      d" ${alts map (_.info)}%\n %"

    def denotStr(denot: Denotation): String =
      if (denot.isOverloaded) overloadedAltsStr(denot.alternatives)
      else if (denot.symbol.exists) denot.symbol.showLocated
      else anonymousTypeMemberStr(denot.info)

    def refStr(tp: Type): String = tp match {
      case tp: NamedType => denotStr(tp.denot)
      case _ => anonymousTypeMemberStr(tp)
    }

    def exprStr(tree: Tree): String = refStr(tree.tpe)

    def patternConstrStr(tree: Tree): String = ???

    /** A subtype log explaining why `found` does not conform to `expected` */
    def whyNoMatchStr(found: Type, expected: Type) =
      if (ctx.settings.explaintypes.value)
        "\n" + ctx.typerState.show + "\n" + TypeComparer.explained((found <:< expected)(_))
      else
        ""
  }

  def err(implicit ctx: Context): Errors = new Errors

  /** The d string interpolator works like the i string interpolator, but marks nonsensical errors
   *  using `<nonsensical>...</nonsensical>` tags.
   *  Note: Instead of these tags, it would be nicer to return a data structure containing the message string
   *  and a boolean indicating whether the message is sensical, but then we cannot use string operations
   *  like concatenation, stripMargin etc on the values returned by d"...", and in the current error
   *  message composition methods, this is crucial.
   */
  implicit class DiagnosticString(val sc: StringContext) extends AnyVal {
    import DiagnosticString._
    def d(args: Any*)(implicit ctx: Context): String = {
      def isSensical(arg: Any): Boolean = arg match {
        case l: Seq[_] => l.forall(isSensical(_))
        case tpe: Type if tpe.isErroneous => false
        case NoType => false
        case sym: Symbol if sym.isCompleted =>
          sym.info != ErrorType && sym.info != TypeAlias(ErrorType) && sym.info != NoType
        case _ => true
      }

      val s = new StringInterpolators(sc).i(args : _*)
      if (args.forall(isSensical(_))) s else nonSensicalStartTag + s + nonSensicalEndTag
    }
  }

  object DiagnosticString {
    final val nonSensicalStartTag = "<nonsensical>"
    final val nonSensicalEndTag = "</nonsensical>"
  }

}
