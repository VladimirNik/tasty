package scala.tasty.internal
package dotc
package core

import SymDenotations.{ SymDenotation, ClassDenotation, NoDenotation }
import Contexts.{Context}
import Names.{Name, PreName}
import Names.TypeName
import Symbols.NoSymbol
import Symbols._
import Types._
import Flags._
import Decorators._
import printing.Texts._
import collection.mutable.ListBuffer
import Decorators.SymbolIteratorDecorator

object Denotations {
  abstract class Denotation(val symbol: Symbol) extends util.DotClass {
    def info: Type
    def isType: Boolean
    def isTerm: Boolean = !isType
    def signature: Signature
    def exists: Boolean = true
    final def alternatives: List[SingleDenotation] = ??? //altsWith(alwaysTrue)
  }

  abstract class SingleDenotation(symbol: Symbol) extends Denotation(symbol) with PreDenotation {
    final def signature: Signature = {
      if (isType) Signature.NotAMethod // don't force info if this is a type SymDenotation
      else info match {
        case info: MethodicType =>
          try info.signature
          catch { // !!! DEBUG
            case scala.util.control.NonFatal(ex) =>
              println(s"cannot take signature of ${info/*.show*/}")
              throw ex
          }
        case _ => Signature.NotAMethod
      }
    }

    def typeRef: TypeRef = ???
    def termRef: TermRef = ???
  }

  trait PreDenotation {
    def exists: Boolean
  }
}
