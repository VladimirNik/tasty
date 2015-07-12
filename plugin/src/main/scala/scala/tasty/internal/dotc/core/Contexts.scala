package scala.tasty.internal.dotc
package core

import util.Positions.{Position, NoPosition}

object Contexts {

  /**
   * A context is passed basically everywhere in dotc.
   *  This is convenient but carries the risk of captured contexts in
   *  objects that turn into space leaks. To combat this risk, here are some
   *  conventions to follow:
   *
   *    - Never let an implicit context be an argument of a class whose instances
   *      live longer than the context.
   *    - Classes that need contexts for their initialization take an explicit parameter
   *      named `initctx`. They pass initctx to all positions where it is needed
   *      (and these positions should all be part of the intialization sequence of the class).
   *    - Classes that need contexts that survive initialization are instead passed
   *      a "condensed context", typically named `cctx` (or they create one). Condensed contexts
   *      just add some basic information to the context base without the
   *      risk of capturing complete trees.
   *    - To make sure these rules are kept, it would be good to do a sanity
   *      check using bytecode inspection with javap or scalap: Keep track
   *      of all class fields of type context; allow them only in whitelisted
   *      classes (which should be short-lived).
   */
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

    /**
     * Log msg if settings.log contains the current phase.
     *  See [[config.CompilerCommand#explainAdvanced]] for the exact meaning of
     *  "contains" here.
     */
    def log(msg: => String, pos: Position = NoPosition): Unit = ???
  }
}