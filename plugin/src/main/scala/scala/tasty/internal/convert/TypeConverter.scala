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

    //types of type parameters shouldn't be cached
    //TODO - rewrite typeCache
    if (!tp.typeSymbol.isTypeParameter) {
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
          case res              => res
        }
    } else {
      convertTypeImpl(tp)
    }
  }

  private def convertScalaTypeRef(tr: g.TypeRef): t.Type = {
    val g.TypeRef(pre, sym, args) = tr
    tr match {
      case _ if g.definitions.isByNameParamType(tr) =>
        if (args.size == 1)
          t.ExprType(convertType(args(0)))
        else throw new Exception(s"not implemented: ByNameParamType has ${args.size} args")
      case _ =>
        val tPre = convertType(pre)
        //TODO - fix deSkolemize here, example def test[U, L <: U] = ... , hi TypeBounds U is Skolem in Scala
        //but not in Dotty
        val tSym = convertSymbol(sym.deSkolemize)
        val basedType = sym match {
          case _ if sym.isType => t.TypeRef(tPre, tSym.name.asTypeName, tSym.asType)
          case _               => t.TermRef(tPre, tSym.name.asTermName, tSym.asTerm)
        }
        if (args.nonEmpty) {
          val tSym = convertSymbol(sym)
          args.zip(sym.typeParams).foldLeft[t.Type](basedType) {
            (basedType, argTypeWithTypeParam) =>
              val argType = argTypeWithTypeParam._1
              val typeParamSymbol = argTypeWithTypeParam._2
              val tParamName = convertToTypeName(typeParamSymbol.name)
              val name = expandedName(tSym, tParamName)
              t.RefinedType(basedType, name, t.TypeAlias(convertType(argType)))
          }
        } else basedType
    }
  }

  def convertTypeAlias(tp: g.Type): t.Type = {
    val tAlias = convertType(tp)
    t.TypeAlias(tAlias) withGType tp
  }

  def convertClassInfoType(parents: List[g.Type], decls: Iterable[g.Symbol], typeSymbol: g.Symbol): t.ClassInfo = {
    //TODO - check prefix
    val tPrefix = convertType(typeSymbol.tpe.prefix)
    val tCls = convertSymbol(typeSymbol).asClass
    //In Dotty parents have form of TypeRef(pre, sym), that's why typeConstructor is used here
    val tParents = parents map {parent => convertType(parent.typeConstructor)} map {
      case tr: t.TypeRef => tr
      case tr => throw new Exception(s"Converted parent (${g.showRaw(tr)}) of $typeSymbol is not t.TypeRef")
    }

    val tDecls = convertSymbols(decls.toList)
    val selfType = typeSymbol.selfType
    val tClsInfo = selfType match {
      case st if st =:= typeSymbol.tpe => t.NoType
      //TODO remove typeSymbol.tpe from selfType (class Test {self: this.Test with X with Y} - this.Test should be removed)
      //invoke convertRefinedType directly if passed type is RefinedType (excluding typeSymbol.tpe)
      case _ => convertType(selfType)
    }
    t.ClassInfo(tPrefix, tCls, tParents, tDecls, tClsInfo)
  }

  def convertClassInfoType(cit: g.ClassInfoType): t.ClassInfo = {
    val g.ClassInfoType(parents, decls, typeSymbol) = cit
    //TODO - check prefix
    convertClassInfoType(parents, decls, typeSymbol)
  }

  def convertTypeImpl(tp: g.Type): t.Type = {
    val resTp = tp match {
      case g.ConstantType(value) =>
        convertType(value.tpe)
      case tpe @ g.TypeRef(pre, sym, args) =>
        tpe match {
          case _ if sym.isTypeParameter =>
            val tSym = convertSymbol(sym)
            if (sym.isType) tSym.typeRef else tSym.termRef
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
              convertType(underlying.typeConstructor) match {
                case tr: t.TypeRef => tr
                case tr => throw new Exception(s"Converted underlying ($tr) of ${g.showRaw(tpe)} is not t.TypeRef")
              }
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
      case rt @ g.RefinedType(parents, decls) =>
        val basedType = parents match {
          case List(parent) => convertType(parent)
          case list :+ last => list.foldRight[t.Type](convertType(last)){
            (tp1, tp2) => t.AndType.make(convertType(tp1), tp2)
          }
        }
        decls.foldLeft[t.Type](basedType) {
          (basedType, decl) =>
            val name = decl.name
            val resTpe = decl.tpe.resultType
            val resSym = resTpe match {
              case tr: g.TypeRef => tr.sym
              //TODO - check this case (typeSymbol returns normalized symbol)
              case _ => resTpe.typeSymbol
            }
            //references to typeParams of enclosing class shouldn't be dealiased, only types defined in RefinedType should be
            val declTpe = if (resSym.owner.isStructuralRefinement) resTpe.dealias else resTpe
            val tDeclTpe = if (decl.isTerm) {
              convertType(declTpe)
            } else {
              val tTpe = convertType(declTpe)
              t.TypeAlias(tTpe) withGType declTpe
            }
            t.RefinedType(basedType, name, tDeclTpe)
        }
      case tpe: g.AnnotatedType =>
        throw new Exception(s"unimplemented conversion for AnnotatedType: $tp")
      case nmt @ g.NullaryMethodType(resultType) =>
        val tResultType = convertType(resultType)
        t.MethodType(Nil, Nil, tResultType)
      case mt @ g.MethodType(params, resultType) =>
        val tParamNames: List[dotc.core.Names.TermName] = mt.params map { sym => convertToTermName(sym.name) }
        val tParamTypes = convertTypes(mt.paramTypes)
        val tResultType = convertType(mt.resultType)
        t.MethodType(tParamNames, tParamTypes, tResultType)
      case pt @ g.PolyType(typeParams, resultType) =>
        resultType match {
          case g.ClassInfoType(parents, decls, typeSymbol) =>
            convertClassInfoType(parents, typeParams ::: decls.toList, typeSymbol)
          case _ =>
            val typeParamsWithBounds = typeParams map { typeParam =>
              (typeParam, typeParam.tpe.bounds)
            }
            val tTypeParams = typeParamsWithBounds map {
              case (typeParam, typeBounds) =>
                (convertSymbol(typeParam), convertType(typeBounds).asInstanceOf[t.TypeBounds])
            }
            val tResultType = convertType(resultType)

            t.PolyType.fromSymbols(tTypeParams, tResultType)
        }
      //TODO invoke g.ClassInfoType conversion in a separate method (to be sure that this tpe is not evaluated during conversion - before pickling)
      case cit@g.ClassInfoType(parents, decls, typeSymbol) =>        
        convertClassInfoType(cit)
      case g.NoPrefix =>
        t.NoPrefix
      case g.NoType =>
        t.NoType
      case _ => throw new Exception(s"unimplemented conversion for type: ${g.showRaw(tp)}")
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