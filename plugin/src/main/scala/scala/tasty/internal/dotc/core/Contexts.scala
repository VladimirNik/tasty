package scala.tasty.internal
package dotc
package core

import util.Positions.{ Position, NoPosition }

trait TContexts {
  self: API =>

  object Contexts {
    abstract class Context extends { thiscontext =>
      trait TyperState {
        val constraint: Constraint = new Constraint
      }

      class Constraint {
        import Types.{ Type, PolyParam }
        //TODO implement after finding the case when/if this exception arises
        def entry(param: PolyParam): Type = throw new Exception("special case in TreePickler: def entry in scala.tasty.internal.dotc.core.Context#Constraint should be implemented!")
      }

      def typerState: TyperState = throw new Exception("special case in TreePickler: def typerState in scala.tasty.internal.dotc.core.Context should be implemented!")

      def log(msg: => String, pos: Position = NoPosition): Unit = ???
    }
  }
}