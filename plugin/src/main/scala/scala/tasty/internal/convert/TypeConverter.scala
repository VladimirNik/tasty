package scala.tasty.internal
package convert

trait TypeConverter {
  self: API =>

  import self.GlobalToTName._
  import self.{ Types => t }
  import dotc.util.{ Positions => tp }
  import scala.collection.JavaConversions._

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

  def convertTypeImpl(tp: g.Type): t.Type = {
    val resTp = tp match {
      case g.ConstantType(value) =>
        convertType(value.tpe)
      case tpe @ g.TypeRef(pre, sym, args) =>
        tpe match {
          case _ =>
            val tPre = convertType(pre)
            val tSym = convertSymbol(sym)
            if (sym.isType) 
              t.TypeRef(tPre, tSym.asType)
            else t.TermRef(tPre, tSym.asTerm)
        }
      case tpe @ g.SingleType(pre, sym) =>
        val tPre = convertType(pre)
        val tSym = convertSymbol(sym)
        if (sym.isType)
          t.TypeRef(tPre, tSym.asType)
        else t.TermRef(tPre, tSym.asTerm)
      case tpe @ g.ThisType(sym) =>
        val underlying = tpe.underlying.widen
        println(s"underlying: ${underlying}")
        println(s"showRaw(underlying): ${g.showRaw(underlying)}")
        val tUnderlying =
          convertType(underlying).asInstanceOf[t.TypeRef]
        println(s"tUnderlying: $tUnderlying")
        t.ThisType.raw(tUnderlying)
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
        throw new Exception(s"unimplemented conversion for MethodType: $tp")
      case g.PolyType(typeParams, resultType) =>
        throw new Exception(s"unimplemented conversion for PolyType: $tp")
      case g.NoType =>
        t.NoType
      case g.NoPrefix =>
        t.NoPrefix
      case _ => throw new Exception(s"unimplemented conversion for type: $tp")
    }
    resTp
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