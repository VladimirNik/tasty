package scala.tasty.internal
package convert

trait SymbolConverter {
  self: API =>

  import self.GlobalToTName._
  import self.{ Symbols => t }
  import dotc.util.{ Positions => tp }
  import scala.collection.JavaConversions._

  val symCache = new java.util.IdentityHashMap[g.Symbol, t.Symbol]();
  val oneToManyCache = new java.util.IdentityHashMap[g.Symbol, scala.collection.mutable.Set[t.Symbol]]

  def convertSymbols(symbols: List[g.Symbol]): List[t.Symbol] = symbols map convertSymbol

  //If passed symbol is a type param of class or trait
  def isClassTypeParam(sym: g.Symbol): Boolean = sym.isTypeParameter && sym.owner.isClass

  def isSymRefInsideConstructor(sym: g.Symbol): Boolean =
    scopeStack.exists { scope =>
      scope.isConstructor && scope.owner == sym.owner
    }

  //Expanded sym (type param) is for class/trait type params that are used outside of constructor
  def isExpandedSym(sym: g.Symbol) = isClassTypeParam(sym) && !isSymRefInsideConstructor(sym)

  def convertSymbol(sym: g.Symbol): t.Symbol = {
    //TODO - add here other cases when one Scala symbol represents several Dotty symbols
    def isOneToManySym(sym: g.Symbol): Boolean = isClassTypeParam(sym)
    def processOneToManySyms(gSym: g.Symbol, tSyms: scala.collection.mutable.Set[t.Symbol]): t.Symbol = {
      tSyms match {
        case _ if isClassTypeParam(gSym) =>
          val constructor = scopeStack.find { scope =>
            scope.isConstructor && scope.owner == gSym.owner
          }
          constructor match {
            case None =>
              val res = tSyms.find { _.owner.isClass } getOrElse { convertScalaClassTypeParameter(gSym, gSym.owner) }
              tSyms += res
              res
            case Some(constr) =>
              val convertedConstr = convertSymbol(constr)
              val res = tSyms.find { _.owner == convertedConstr } getOrElse { convertScalaClassTypeParameter(gSym, constr) }
              tSyms += res
              res
          }
        //TODO - add here other cases for one to many processing
        case _ => throw new Exception(s"Unintended invocation of oneToManyCache during convertion of $gSym")
      }
    }
    sym match {
      case _ if sym == null => null
      case _ if isOneToManySym(sym) =>
        val foundSymbol = mapAsScalaMap(oneToManyCache).get(sym) match {
          case None =>
            val newSet = collection.mutable.Set[t.Symbol]()
            oneToManyCache += (sym -> newSet)
            processOneToManySyms(sym, newSet)
          case Some(syms) =>
            processOneToManySyms(sym, syms)
        }
        foundSymbol
      case _ =>
        //if sym is null - return null
        //if sym is in the symCache map - just return the value from the map
        //is sym is not in the map - write IncompleteSymbol to the map, convert symbol, update the value in the map
        //if resSymbol is incomplete symbol throw new Exception
        symCache.getOrElse(sym,
          sym match {
            case _ =>
              symCache += (sym -> t.IncompleteSymbol)
              val convertedSym = convertSymImpl(sym)
              symCache += (sym -> convertedSym)
              convertedSym
          }) match {
            case t.IncompleteSymbol => throw new Exception(s"IncompleteSymbol is found while converting $sym")
            case res => res
          }
    }
  }

  def convertScalaClassTypeParameter(sym: g.Symbol, owner: g.Symbol) = {
    val flags = convertModifiers(sym)
    convertTypeParameter(sym, owner, flags)
  }

  def convertTypeParameter(sym: g.Symbol, owner: g.Symbol, flags: dotc.core.Flags.FlagSet): t.Symbol = {
    val tOwner = convertSymbol(owner)
    //TODO fix privateWithin
    val bufName = convertToTypeName(sym.name)
    val newName = if (owner.isMethod /*|| !isExpandedSym(sym)*/ ) bufName else expandedName(tOwner, bufName).toTypeName
    newTypeParamSymbol(tOwner, newName, flags, sym)
  }

  def convertSymImpl(sym: g.Symbol, directParam: Boolean = false): t.Symbol = {
    //TODO - fix flags
    val flags = convertModifiers(sym)
    val pos: tp.Position = sym.pos
    val coord: tp.Coord = pos
    val resSym = sym match {
      case g.NoSymbol =>
        t.NoSymbol
      case _ if sym.isRoot => newClassSymbol(t.NoSymbol, dotc.core.StdNames.tpnme.ROOT, flags, sym)
      case _ if sym.hasPackageFlag && sym.isPackageClass =>
        val tOwner = convertSymbol(sym.owner)
        val tName = convertToTypeName(sym.name)
        newClassSymbol(tOwner, tName, dotc.core.Flags.PackageCreationFlags | flags, sym)
      case _ if sym.hasPackageFlag =>
        val tOwner = convertSymbol(sym.owner)
        val tName = convertToTermName(sym.name)
        newPackageSymbol(tOwner, tName, flags, sym)
      // if sym.isModuleClass its name should be changed: '$'originalName
      case _ if sym.isModuleClass =>
        val tOwner = convertSymbol(sym.owner)
        //TODO fix privateWithin
        import dotc.core.Flags
        newClassSymbol(tOwner, convertToTypeName(syntheticName(sym.name)), flags | Flags.Module, sym, privateWithin = t.NoSymbol, coord, sym.associatedFile)
      //This case is for def type parameters (except for constructors). Class, trait, constructor type params should be processed with processOneToManySyms
      case _ if sym.isTypeParameter =>
        convertTypeParameter(sym, sym.owner, flags)
      case _ if sym.isClass =>
        val tOwner = convertSymbol(sym.owner)
        //TODO fix privateWithin
        newClassSymbol(tOwner, convertToTypeName(sym.name), flags, sym, privateWithin = t.NoSymbol, coord, sym.associatedFile)
      case _ if sym.isConstructor =>
        val tOwner = convertSymbol(sym.owner)
        //TODO fix privateWithin
        newConstructor(tOwner, flags, sym, privateWithin = t.NoSymbol, coord)
      // In Dotty:
      // class X(x: Int) extends Y(x) - reference to x from Y(x) is param accessor symbol
      case _ if !directParam && isPrimaryConstrParameter(sym) =>
        val paramAccessorSymbol = sym.enclClass.info.decl(sym.name)
        assert(paramAccessorSymbol.isParamAccessor, s"Incorrect conversion for parameter of primary constructor: ${g.showRaw(sym, printKinds = true)}")
        convertSymbol(paramAccessorSymbol)
      case _ =>
        generalSymbolConversion(sym, flags, coord)
    }
    resSym
  }

  def isPrimaryConstrParameter(sym: g.Symbol) = sym.isParameter && sym.owner.isPrimaryConstructor
  def generalSymbolConversion(sym: g.Symbol, flags: dotc.core.Flags.FlagSet, coord: tp.Coord) = {
    val tOwner = convertSymbol(sym.owner)
    //TODO fix privateWithin
    newSymbol(tOwner, convertToName(sym.name), flags, sym, privateWithin = t.NoSymbol, coord)
  }
}