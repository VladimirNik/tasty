package scala.tasty.internal.scalac.phase

import scala.tools.nsc.Global

trait TastyPhaseUtils {
  val global: Global
  import global.{ Tree, ClassSymbol, PackageDef, TypeDef, ClassDef, ModuleDef, ValDef, treeCopy }
  /**
   * The top level classes in this tree, including only those module classes that
   *  are not a linked class of some other class in the result.
   */
  def topLevelClasses(tree: Tree): List[ClassSymbol] = tree match {
    case PackageDef(_, stats)                  => stats.flatMap(topLevelClasses)
    case cdef: ClassDef if cdef.symbol.isClass => cdef.symbol.asClass :: Nil
    case mdef: ModuleDef => mdef.symbol.moduleClass.asClass :: Nil
    case _                                     => Nil
  }

  /** The tree containing only the top-level classes and objects matching either `cls` or its companion object */
  def sliceTopLevel(tree: Tree, cls: ClassSymbol): List[Tree] = tree match {
    case PackageDef(pid, stats) =>
      treeCopy.PackageDef(tree, pid, stats.flatMap(sliceTopLevel(_, cls))) :: Nil
    case cdef: ClassDef =>
      val sym = cdef.symbol
      assert(sym.isClass)
      // TODO - in Dotty cls == sym.linkedClass (check that they are similar)
      if (cls == sym || cls == sym.linkedClassOfClass) cdef :: Nil
      else Nil
    case mdef: ModuleDef =>
      val sym = mdef.symbol
      assert(sym.isModule)
      if (cls == sym.companionClass || cls == sym.moduleClass) mdef :: Nil
      else Nil
//    case vdef: ValDef =>
//      val sym = vdef.symbol
//      assert(sym.isModule)
//      if (cls == sym.companionClass || cls == sym.moduleClass) vdef :: Nil
//      else Nil
    case tree =>
      tree :: Nil
  }
}