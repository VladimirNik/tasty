package scala.tasty.internal
package convert

trait ModifierConverter {
  self: API =>

  import self.{ Symbols => t }
  import dotc.core.Flags._

  def convertModifiers(sym: g.Symbol): FlagSet = {
    var newFlags = EmptyFlags
    def setFlags(flags: FlagSet) = newFlags = (newFlags | flags)

    //TODO - What to do with Flags.BindDefinedType, Flags.ExpandedName?
    if (sym.isParameter) setFlags(Param)
    if (sym.isParamAccessor) setFlags(ParamAccessor)
    if (sym.isPrivate) setFlags(Private)
    if (sym.isProtected) setFlags(Protected)
    if (sym.isFinal) setFlags(Final)
    if (sym.isCase) setFlags(Case)
    if (sym.isOverride) setFlags(Override)
    if (sym.isModule) setFlags(Module)
    if (sym.hasLocalFlag) setFlags(Local)
    if (sym.isSynthetic) setFlags(Synthetic)
    if (sym.isArtifact) setFlags(Artifact)
    //type parameters inside TypeDef (classes, traits) have expanded name
    //TODO - pass new name and check if this name contains '$$'
    //TODO - or move this flag generation to SymbolConverter
    if (isExpandedSym(sym)) setFlags(ExpandedName)
    if (sym.isTerm) {
      if (sym.isImplicit) setFlags(Implicit)
      if (sym.isLazy) setFlags(Lazy)
      if (sym.isAbstractOverride) { setFlags(Abstract); setFlags(Override) }
      if (sym.isMutable) setFlags(Mutable)
      if (sym.isAccessor) setFlags(Accessor)
      if (sym.isCaseAccessor) setFlags(CaseAccessor)
    } else {
      if (sym.isSealed) setFlags(Sealed)
      //Don't set Abstract flag to trait and abstract type (in Scala traits have the abstract flag)
      if (sym.isAbstract && !sym.isTrait && !sym.isAbstractType) setFlags(Abstract)
      if (sym.isTrait) setFlags(Trait)
      if (sym.isCovariant) setFlags(Covariant)
      if (sym.isContravariant) setFlags(Contravariant)
    }
    newFlags
  }
}