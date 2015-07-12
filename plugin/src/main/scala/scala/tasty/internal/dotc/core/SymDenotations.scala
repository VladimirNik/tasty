package scala.tasty.internal.dotc
package core

import Contexts._, Symbols._, Denotations._, Names._, NameOps._, Annotations._
import Types._, Flags._, Decorators._, /*DenotTransformers._,*/ StdNames._
import NameOps._
//import Scopes.Scope
import collection.mutable
import collection.immutable.BitSet
import scala.reflect.io.AbstractFile
import Decorators.SymbolIteratorDecorator
import ast._
import annotation.tailrec
import config.Printers._

trait L { this: Context =>
  import SymDenotations._

  /** Factory method for SymDenotion creation. All creations
   *  should be done via this method.
   */
  def SymDenotation(
    symbol: Symbol,
    owner: Symbol,
    name: Name,
    initFlags: FlagSet,
    initInfo: Type,
    initPrivateWithin: Symbol = NoSymbol)(implicit ctx: Context): SymDenotation = ???
}

object SymDenotations {
  /** A sym-denotation represents the contents of a definition
   *  during a period.
   */
  class SymDenotation private[SymDenotations] (
    symbol: Symbol,
    ownerIfExists: Symbol,
    final val name: Name,
    initFlags: FlagSet,
    initInfo: Type,
    initPrivateWithin: Symbol = NoSymbol) extends SingleDenotation(symbol) {

    // ------ Getting and setting fields -----------------------------

    private[this] var myFlags: FlagSet = ??? //adaptFlags(initFlags)

    /** The owner of the symbol; overridden in NoDenotation */
    def owner: Symbol = ??? //ownerIfExists

    /** The flag set */
    final def flags(implicit ctx: Context): FlagSet = ???

    /** Has this denotation one of the flags in `fs` set? */
    final def is(fs: FlagSet)(implicit ctx: Context) = {
      (if (fs <= FromStartFlags) myFlags else flags) is fs
    }

    /** The type info.
     *  The info is an instance of TypeType iff this is a type denotation
     *  Uncompleted denotations set myInfo to a LazyType.
     */
    final def info(implicit ctx: Context): Type = ???

    /** The privateWithin boundary, NoSymbol if no boundary is given.
     */
    final def privateWithin(implicit ctx: Context): Symbol = ???

    /** The annotations of this denotation */
    final def annotations(implicit ctx: Context): List[Annotation] = ???

    // ----- Tests -------------------------------------------------

    /** Is this denotation a type? */
    override def isType: Boolean = name.isTypeName

    /** Is this denotation a class? */
    final def isClass: Boolean = isInstanceOf[ClassDenotation]

    /** Is this symbol the root class or its companion object? */
    final def isRoot: Boolean =
      (name.toTermName == nme.ROOT || name == nme.ROOTPKG) && (owner eq NoSymbol)

    /** Is this symbol the empty package class or its companion object? */
    final def isEmptyPackage(implicit ctx: Context): Boolean =
      name.toTermName == nme.EMPTY_PACKAGE && owner.isRoot

    /** Is this symbol the empty package class or its companion object? */
      //*
    final def isEffectiveRoot(implicit ctx: Context) = isRoot || isEmptyPackage

    /** Is this a setter? */
    final def isSetter(implicit ctx: Context): Boolean = ???

    /** Is this the constructor of a trait or a class */
    final def isConstructor = name.isConstructorName

    override def typeRef(implicit ctx: Context): TypeRef = ???

    override def termRef(implicit ctx: Context): TermRef = ???

    override def toString = {
      val kindString =
        if (myFlags is ModuleClass) "module class"
        else if (isClass) "class"
        else if (isType) "type"
        else if (myFlags is Module) "module"
        else if (myFlags is Method) "method"
        else "val"
      s"$kindString $name"
    }
  }

  /** The contents of a class definition during a period
   */
  class ClassDenotation private[SymDenotations] (
    symbol: Symbol,
    ownerIfExists: Symbol,
    name: Name,
    initFlags: FlagSet,
    initInfo: Type,
    initPrivateWithin: Symbol/*,
    initRunId: RunId*/)
    extends SymDenotation(symbol, ownerIfExists, name, initFlags, initInfo, initPrivateWithin)

  /** The denotation of a package class.
   *  It overrides ClassDenotation to take account of package objects when looking for members
   */
  class PackageClassDenotation private[SymDenotations] (
    symbol: Symbol,
    ownerIfExists: Symbol,
    name: Name,
    initFlags: FlagSet,
    initInfo: Type,
    initPrivateWithin: Symbol /*,
    initRunId: RunId*/)
    extends ClassDenotation(symbol, ownerIfExists, name, initFlags, initInfo, initPrivateWithin/*, initRunId*/) {
  }

  class NoDenotation extends SymDenotation(
    NoSymbol, NoSymbol, "<none>".toTermName, Permanent, NoType) {
    override def exists = false
    override def isTerm = false
    override def isType = false
    override def owner: Symbol = throw new AssertionError("NoDenotation.owner")
  }

  val NoDenotation = new NoDenotation
}
