package scala.tasty.internal
package convert

import scala.tasty.internal.dotc.core.{ Names => t }
import scala.language.implicitConversions

trait NameConverter {
  self: API =>
    
  object GlobalToTName {
    def convertToTermName(name: g.Name): t.TermName = t.termName(name.toChars, name.start, name.length)
    
    def convertToTypeName(name: g.Name): t.TypeName = t.typeName(name.toChars, name.start, name.length)
    
    implicit def convertTermName(name: g.TermName): t.TermName = convertToTermName(name)
    
    implicit def convertTypeName(name: g.TypeName): t.TypeName = convertToTypeName(name)
  } 
}