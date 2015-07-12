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

/** Denotations represent the meaning of symbols and named types.
 *  The following diagram shows how the principal types of denotations
 *  and their denoting entities relate to each other. Lines ending in
 *  a down-arrow `v` are member methods. The two methods shown in the diagram are
 *  "symbol" and "deref". Both methods are parameterized by the current context,
 *  and are effectively indexed by current period.
 *
 *  Lines ending in a horizontal line mean subtying (right is a subtype of left).
 *
 *  NamedType------TermRefWithSignature
 *    |                    |                     Symbol---------ClassSymbol
 *    |                    |                       |                |
 *    | denot              | denot                 | denot          | denot
 *    v                    v                       v                v
 *  Denotation-+-----SingleDenotation-+------SymDenotation-+----ClassDenotation
 *             |                      |
 *             +-----MultiDenotation  |
 *                                    |
 *                                    +--UniqueRefDenotation
 *                                    +--JointRefDenotation
 *
 *  Here's a short summary of the classes in this diagram.
 *
 *  NamedType                A type consisting of a prefix type and a name, with fields
 *                              prefix: Type
 *                              name: Name
 *                           It has two subtypes: TermRef and TypeRef
 *  TermRefWithSignature     A TermRef that has in addition a signature to select an overloaded variant, with new field
 *                              sig: Signature
 *  Symbol                   A label for a definition or declaration in one compiler run
 *  ClassSymbol              A symbol representing a class
 *  Denotation               The meaning of a named type or symbol during a period
 *  MultiDenotation          A denotation representing several overloaded members
 *  SingleDenotation         A denotation representing a non-overloaded member or definition, with main fields
 *                              symbol: Symbol
 *                              info: Type
 *  UniqueRefDenotation      A denotation referring to a single definition with some member type
 *  JointRefDenotation       A denotation referring to a member that could resolve to several definitions
 *  SymDenotation            A denotation representing a single definition with its original type, with main fields
 *                              name: Name
 *                              owner: Symbol
 *                              flags: Flags
 *                              privateWithin: Symbol
 *                              annotations: List[Annotation]
 *  ClassDenotation          A denotation representing a single class definition.
 */
object Denotations {

  /** A denotation is the result of resolving
   *  a name (either simple identifier or select) during a given period.
   *
   *  Denotations can be combined with `&` and `|`.
   *  & is conjunction, | is disjunction.
   *
   *  `&` will create an overloaded denotation from two
   *  non-overloaded denotations if their signatures differ.
   *  Analogously `|` of two denotations with different signatures will give
   *  an empty denotation `NoDenotation`.
   *
   *  A denotation might refer to `NoSymbol`. This is the case if the denotation
   *  was produced from a disjunction of two denotations with different symbols
   *  and there was no common symbol in a superclass that could substitute for
   *  both symbols. Here is an example:
   *
   *  Say, we have:
   *
   *    class A { def f: A }
   *    class B { def f: B }
   *    val x: A | B = if (test) new A else new B
   *    val y = x.f
   *
   *  Then the denotation of `y` is `SingleDenotation(NoSymbol, A | B)`.
   *
   *  @param symbol  The referencing symbol, or NoSymbol is none exists
   */
  abstract class Denotation(val symbol: Symbol) extends util.DotClass {

    /** The type info of the denotation, exists only for non-overloaded denotations */
    def info(implicit ctx: Context): Type

    /** Is this a reference to a type symbol? */
    def isType: Boolean

    /** Is this a reference to a term symbol? */
    def isTerm: Boolean = !isType

    /** The signature of the denotation. */
    def signature(implicit ctx: Context): Signature

    /** Is this denotation different from NoDenotation or an ErrorDenotation? */
    def exists: Boolean = true

    /** The set of alternative single-denotations making up this denotation */
    final def alternatives: List[SingleDenotation] = ??? //altsWith(alwaysTrue)
  }

  /** A non-overloaded denotation */
  abstract class SingleDenotation(symbol: Symbol) extends Denotation(symbol) with PreDenotation {
    final def signature(implicit ctx: Context): Signature = {
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

    // ------ Forming types -------------------------------------------

    /** The TypeRef representing this type denotation at its original location. */
    def typeRef(implicit ctx: Context): TypeRef = ???

    /** The TermRef representing this term denotation at its original location. */
    def termRef(implicit ctx: Context): TermRef = ???
  }

  // --------------- PreDenotations -------------------------------------------------

  /** A PreDenotation represents a group of single denotations
   *  It is used as an optimization to avoid forming MultiDenotations too eagerly.
   */
  trait PreDenotation {
    /** A denotation in the group exists */
    def exists: Boolean
  }
}

