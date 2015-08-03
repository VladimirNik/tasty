package scala.tasty.internal
package convert

import scala.tasty.internal.dotc.core.{ Names => t }
import scala.language.implicitConversions
import scala.tasty.internal.dotc.core.StdNames
  import scala.collection.JavaConversions._

trait NameConverter {
  self: API =>

  val nameCache = new java.util.IdentityHashMap[g.Name, t.Name]();

  private def convertName[TN <: t.Name](name: g.Name)(convertFn: g.Name => TN): TN = {
    nameCache.getOrElse(name,
      {
        val convertedName = convertFn(name)
        nameCache += (name -> convertedName)
        convertedName
      }).asInstanceOf[TN]
  }

  object GlobalToTName {
    //TODO StdNames mapping (between reflect.StdNames and tasty.StdNames

    implicit def convertToName(name: g.Name): t.Name =
      if (name.isTermName) convertToTermName(name) else convertToTypeName(name)

    def convertToTermName(name: g.Name): t.TermName = {
      convertName(name){ (fname: g.Name) =>
        fname match {
          case _ if fname == g.nme.EMPTY_PACKAGE_NAME => StdNames.nme.EMPTY_PACKAGE
          case _ if fname.isEmpty                     => StdNames.nme.EMPTY
          case _ if fname == g.nme.CONSTRUCTOR        => StdNames.nme.CONSTRUCTOR
          case _ if fname == g.nme.scala_             => StdNames.nme.scala_
          case _ if fname == g.nme.USCOREkw           => StdNames.nme.USCOREkw
          case _                                     => t.termName(fname.toString())
        }
      }
    }

    def convertToTypeName(name: g.Name): t.TypeName = {
      convertName(name) { (fname: g.Name) =>
        fname match {
          case _ if name == g.tpnme.AnyRef => StdNames.tpnme.AnyRef
          case _                           => t.typeName(fname.toString())
        }
      }
    }

    implicit def convertTermName(name: g.TermName): t.TermName = convertToTermName(name)

    implicit def convertTypeName(name: g.TypeName): t.TypeName = convertToTypeName(name)
  }
}