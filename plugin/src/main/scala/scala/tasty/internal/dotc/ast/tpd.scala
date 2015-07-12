package scala.tasty.internal
package dotc
package ast

import core._
import util.Positions._, Types._, Contexts._, Constants._, Names._, Flags._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._, Symbols._
import Denotations._, Decorators._
import config.Printers._
import collection.mutable

import scala.annotation.tailrec

object tpd extends Trees.Instance[Type]

