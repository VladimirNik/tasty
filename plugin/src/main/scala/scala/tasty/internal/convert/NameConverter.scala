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

    def convertToTermName(name: g.Name): t.TermName = {
      name match {
        case _ if name == g.nme.EMPTY_PACKAGE_NAME => StdNames.nme.EMPTY_PACKAGE
        case _ if name.isEmpty                     => StdNames.nme.EMPTY
        case _                                     => t.termName(name.toChars, name.start, name.length)
      }
    }
    
    def convertToTypeName(name: g.Name): t.TypeName =
      t.typeName(name.toChars, name.start, name.length)
    
    implicit def convertTermName(name: g.TermName): t.TermName = convertToTermName(name)
    
    implicit def convertTypeName(name: g.TypeName): t.TypeName = convertToTypeName(name)
  } 
}