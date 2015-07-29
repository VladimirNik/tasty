package scala.tasty.internal
package convert

trait SymbolConverter {
  self: API =>

  import self.GlobalToTName._
  import self.{ Symbols => t }
  import dotc.util.{ Positions => tp }
  import scala.collection.JavaConversions._

  val symCache = new java.util.IdentityHashMap[g.Symbol, t.Symbol]();

  def convertSymbols(symbol: List[g.Symbol]): List[t.Symbol] = symbol map convertSymbol

  def convertSymbol(sym: g.Symbol): t.Symbol = {
    //if sym is null - return null
    //if sym is in the symCache map - just return the value from the map
    //is sym is not in the map - write IncompleteSymbol to the map, convert symbol, update the value in the map
    //if resSymbol is incomplete symbol throw new Exception

    symCache.getOrElse(sym,
      sym match {
        case _ if sym ne null => 
          symCache += (sym -> t.IncompleteSymbol)
          val convertedSym = convertSymImpl(sym)
          symCache += (sym -> convertedSym)
          convertedSym
        case _ => null
      }) match {
      case t.IncompleteSymbol => throw new Exception(s"IncompleteSymbol is found while converting $sym")
      case res => res
    }
  }
  
  def convertSymImpl(sym: g.Symbol): t.Symbol = {
    //TODO - fix flags
    val flags = dotc.core.Flags.EmptyFlags
    val pos: tp.Position = sym.pos
    val coord: tp.Coord = pos
    val resSym = sym match {
      case g.NoSymbol => 
        t.NoSymbol
      case _ if sym.hasPackageFlag =>
        val tOwner = convertSymbol(sym.owner)
        val tName = convertToTermName(sym.name)
        newPackageSymbol(tOwner, tName, flags, sym)
      case _ if sym.isClass => 
        val tOwner = convertSymbol(sym.owner)
        //TODO fix privateWithin
        newClassSymbol(tOwner, convertToTypeName(sym.name), flags, sym, privateWithin = t.NoSymbol, coord, sym.associatedFile)
      case _ if sym.isConstructor =>
        val tOwner = convertSymbol(sym.owner)
        newConstructor(tOwner, flags, sym, privateWithin = t.NoSymbol, coord)
      case _ =>
        val tOwner = convertSymbol(sym.owner)
        newSymbol(tOwner, convertToName(sym.name), flags, sym, privateWithin = t.NoSymbol, coord)
    }
    resSym
  }
}