package scala.tasty.internal
package dotc
package core

trait TDenotations {
  self: API =>
  import SymDenotations.{ SymDenotation, ClassDenotation, NoDenotation }
  import Contexts.{ Context }
  import Names.{ Name, PreName }
  import Names.TypeName
  import Symbols.NoSymbol
  import Symbols._
  import Types._
  import Flags._
  import Decorators._
  import printing.Texts._
  import collection.mutable.ListBuffer

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
      private[this] var mySignature = Signature.NotAMethod
      final def signature: Signature = {
        //TODO if initGSymbol == g.NoSymbol - construct signature based on passed params
        if (isType) Signature.NotAMethod // don't force info if this is a type SymDenotation
        else Signature(symbol.initGSymbol.tpe)
//        else info match {
//          case info: MethodicType =>
//            try info.signature
//            catch { // !!! DEBUG
//              case scala.util.control.NonFatal(ex) =>
//                println(s"cannot take signature of ${info /*.show*/ }")
//                throw ex
//            }
//          case _ => Signature.NotAMethod
//        }
      }

      def typeRef: TypeRef =
        TypeRef(symbol.owner.thisType, symbol.name.asTypeName, this)

      def termRef: TermRef =
        TermRef(symbol.owner.thisType, symbol.name.asTermName, this)

      def valRef: TermRef =
        TermRef.withSigAndDenot(symbol.owner.thisType, symbol.name.asTermName, Signature.NotAMethod, this)

      def termRefWithSig: TermRef =
        TermRef.withSigAndDenot(symbol.owner.thisType, symbol.name.asTermName, signature, this)

      def namedType: NamedType =
        if (isType) typeRef else termRefWithSig

      type AsSeenFromResult = SingleDenotation
      //TODO - rewrite if required
      protected def computeAsSeenFrom(pre: Type): SingleDenotation = this
    }

    trait PreDenotation {
      def exists: Boolean

      type AsSeenFromResult <: PreDenotation

      final def asSeenFrom(pre: Type): AsSeenFromResult =
        computeAsSeenFrom(pre)

      protected def computeAsSeenFrom(pre: Type): AsSeenFromResult
    }
  }
}
