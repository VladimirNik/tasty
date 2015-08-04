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
    if (sym.isPrivate) setFlags(Private)
    if (sym.isProtected) setFlags(Protected)
    if (sym.isFinal) setFlags(Final)
    if (sym.isCase) setFlags(Case)
    if (sym.isOverride) setFlags(Override)
    if (sym.isModule) setFlags(Module)
    if (sym.hasLocalFlag) setFlags(Local)
    if (sym.isSynthetic) setFlags(Synthetic)
    if (sym.isArtifact) setFlags(Artifact)
    if (sym.isTerm) {
      if (sym.isImplicit) setFlags(Implicit)
      if (sym.isLazy) setFlags(Lazy)
      if (sym.isAbstractOverride) { setFlags(Abstract); setFlags(Override) }
      if (sym.isMutable) setFlags(Mutable)
      if (sym.isAccessor) setFlags(Accessor)
      if (sym.isCaseAccessor) setFlags(CaseAccessor)
    } else {
      if (sym.isSealed) setFlags(Sealed)
      if (sym.isAbstract) setFlags(Abstract)
      if (sym.isTrait) setFlags(Trait)
      if (sym.isCovariant) setFlags(Covariant)
      if (sym.isContravariant) setFlags(Contravariant)
    }
    newFlags
  }
}