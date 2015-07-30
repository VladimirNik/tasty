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
      case g.NoType => 
        t.NoType
      case g.NoPrefix =>
        t.NoPrefix
      case _ =>
        ???
    }
    resTp
  }

  def convertConstantTpe(const: g.Constant): t.Type = {
    import self.Constants._
    import g.{definitions => d}
    const.tag match {
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
      case ClazzTag   => convertType(d.ClassType(const.typeValue))
      case EnumTag    => convertType(d.EnumType(const.symbolValue))
    }
  }
}