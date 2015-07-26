package scala.tasty.internal
package convert

import scala.tasty.internal.dotc.core.{ Names => t }
import scala.language.implicitConversions
import scala.tasty.internal.dotc.core.StdNames

trait NameConverter {
  self: API =>

  object GlobalToTName {
    //TODO StdNames mapping (between reflect.StdNames and tasty.StdNames

    implicit def convertToName(name: g.Name): t.Name =
      if (name.isTermName) convertToTermName(name) else convertToTypeName(name)

    def convertToTermName(name: g.Name): t.TermName =
      name match {
        case _ if name == g.nme.EMPTY_PACKAGE_NAME => StdNames.nme.EMPTY_PACKAGE
        case _ if name.isEmpty => StdNames.nme.EMPTY
        case _ if name == g.nme.CONSTRUCTOR => StdNames.nme.CONSTRUCTOR
        case _ if name == g.nme.scala_ => StdNames.nme.scala_
        case _ if name == g.nme.USCOREkw => StdNames.nme.USCOREkw
        case _ => t.termName(name.toString())
      }

    def convertToTypeName(name: g.Name): t.TypeName =
      name match {
        case _ if name == g.tpnme.AnyRef => StdNames.tpnme.AnyRef
        case _ => t.typeName(name.toString()) 
      }

    implicit def convertTermName(name: g.TermName): t.TermName = convertToTermName(name)

    implicit def convertTypeName(name: g.TypeName): t.TypeName = convertToTypeName(name)
  }
}