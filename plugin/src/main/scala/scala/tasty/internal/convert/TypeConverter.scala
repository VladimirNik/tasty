package scala.tasty.internal
package convert

trait TypeConverter {
  self: API =>

  import self.GlobalToTName._
  import self.{ Types => t }
  import self.{ Symbols => tasty }
  import dotc.util.{ Positions => tp }
  import scala.collection.JavaConversions._

  //Types that have no one to one correspondence in Scala and Dotty
  //For example, TermRef in Dotty is not directly represented in Scala
  private val generatedTypes = new java.util.IdentityHashMap[tasty.Symbol, t.Type]

  private def getRefType(sym: tasty.Symbol)(fn: tasty.Symbol => t.Type): t.Type = {
    generatedTypes.getOrElse(sym,
      {
        val genTp = fn(sym)
        generatedTypes += (sym -> genTp)
        genTp
      })
  }

  def getTermRef(sym: tasty.Symbol): t.Type =
    getRefType(sym)(_.termRef)

  def getTermRef(sym: g.Symbol): t.Type = {
    val tSymbol = convertSymbol(sym)
    getTermRef(tSymbol)
  }

  def getTypeRef(sym: tasty.Symbol): t.Type =
    getRefType(sym)(_.typeRef)

  def getTypeRef(sym: g.Symbol): t.Type = {
    val tSymbol = convertSymbol(sym)
    getTypeRef(tSymbol)
  }

  val typeCache = new java.util.IdentityHashMap[g.Type, t.Type]();

  def convertTypes(tps: List[g.Type]): List[t.Type] = tps map convertType

  def convertType(tp: g.Type): t.Type = {
    //if sym is null - return null
    //if sym is in the symCache map - just return the value from the map
    //is sym is not in the map - write IncompleteSymbol to the map, convert symbol, update the value in the map
    //if resSymbol is incomplete symbol throw new Exception

    typeCache.getOrElse(tp,
      tp match {
        case _ if tp ne null => 
          typeCache += (tp -> t.IncompleteType)
          val convertedTp = convertTypeImpl(tp)
          typeCache += (tp -> convertedTp)
          convertedTp
        case _ => null
      }) match {
      case t.IncompleteType => throw new Exception(s"IncompleteType is found while converting $tp")
      case res => res
    }
  }

  private def convertScalaTypeRef(tr: g.TypeRef): t.Type = {
    val g.TypeRef(pre, sym, args) = tr
    val tPre = convertType(pre)
    val tSym = convertSymbol(sym)
    if (sym.isType)
      t.TypeRef(tPre, tSym.name.asTypeName, tSym.asType)
    else t.TermRef(tPre, tSym.name.asTermName, tSym.asTerm)
  }

  def convertTypeAlias(tp: g.Type): t.Type = {
    val tAlias = convertType(tp)
    t.TypeAlias(tAlias) withGType tp
  } 

  def convertTypeImpl(tp: g.Type): t.Type = {
    val resTp = tp match {
      case g.ConstantType(value) =>
        convertType(value.tpe)
      case tpe @ g.TypeRef(pre, sym, args) =>
        tpe match {
          case _ =>
            convertScalaTypeRef(tpe)
        }
      case tpe @ g.SingleType(pre, sym) =>
        val tPre = convertType(pre)
        val tSym = convertSymbol(sym)
        if (sym.isType)
          t.TypeRef(tPre, tSym.asType)
        else t.TermRef(tPre, tSym.asTerm)
      case tpe @ g.ThisType(sym) =>
        sym match {
          case _ if sym.isRoot => t.NoPrefix
          case _ =>
            val underlying = tpe.underlying.widen
            val tUnderlying =
              convertType(underlying).asInstanceOf[t.TypeRef]
            t.ThisType.raw(tUnderlying)
        }
      case g.SuperType(thisTp, superTp) =>
        val tThisTp = convertType(thisTp)
        val tSuperTp = convertType(superTp)
        t.SuperType(tThisTp, tSuperTp)
      case g.TypeBounds(lo, ho) =>
        val tlo = convertType(lo)
        val tho = convertType(ho)
        t.TypeBounds(tlo, tho)
      case tpe: g.AnnotatedType =>
        throw new Exception(s"unimplemented conversion for AnnotatedType: $tp")
      case mt @ g.MethodType(params, resultType) =>
        val tParamNames: List[dotc.core.Names.TermName] = mt.params map { sym => convertToTermName(sym.name) }
        val tParamTypes = convertTypes(mt.paramTypes)
        val tResultType = convertType(mt.resultType)
        t.MethodType(tParamNames, tParamTypes, tResultType)
      case g.PolyType(typeParams, resultType) =>
        throw new Exception(s"unimplemented conversion for PolyType: $tp")
        //TODO move g.ClassInfoType to separate method (to be sure that this tpe is not evaluated during conversion - before pickling)
      case g.ClassInfoType(parents, decls, typeSymbol) =>
        //TODO - check prefix
        val tPrefix = convertType(typeSymbol.tpe.prefix)
        val tCls = convertSymbol(typeSymbol).asClass
        val tParents = convertTypes(parents) map (_.asInstanceOf[t.TypeRef])
        val tDecls = convertSymbols(decls.toList)
        //TODO - fix it
        val tClsInfo = t.NoType
        t.ClassInfo(tPrefix, tCls, tParents, tDecls, tClsInfo)
      case g.NoPrefix =>
        t.NoPrefix
      case g.NoType =>
        t.NoType
      case _ => throw new Exception(s"unimplemented conversion for type: $tp")
    }
    resTp withGType tp
  }

  def getConstantTpe(constTag: Int, gConstTpe: g.Type = g.NoType): t.Type = {
    import self.Constants._
    import g.{ definitions => d }
    constTag match {
      case UnitTag    => convertType(d.UnitTpe)
      case BooleanTag => convertType(d.BooleanTpe)
      case ByteTag    => convertType(d.ByteTpe)
      case ShortTag   => convertType(d.ShortTpe)
      case CharTag    => convertType(d.CharTpe)
      case IntTag     => convertType(d.IntTpe)
      case LongTag    => convertType(d.LongTpe)
      case FloatTag   => convertType(d.FloatTpe)
      case DoubleTag  => convertType(d.DoubleTpe)
      case StringTag  => convertType(d.StringTpe)
      case NullTag    => convertType(d.NullTpe)
      case ClazzTag | EnumTag   => convertType(gConstTpe)
    }
  }
}