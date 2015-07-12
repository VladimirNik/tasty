/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tasty.internal.dotc
package util

import core.Names._
import core.Decorators._

/** Provides functions to encode and decode Scala symbolic names.
 *  Also provides some constants.
 */
object NameTransformer {
  // XXX Short term: providing a way to alter these without having to recompile
  // the compiler before recompiling the compiler.
  val MODULE_SUFFIX_STRING = sys.props.getOrElse("SCALA_MODULE_SUFFIX_STRING", "$")
  val NAME_JOIN_STRING     = sys.props.getOrElse("SCALA_NAME_JOIN_STRING", "$")
  val MODULE_INSTANCE_NAME = "MODULE$"

  private val nops = 128
  private val ncodes = 26 * 26

  private class OpCodes(val op: Char, val code: String, val next: OpCodes)

  private val op2code = new Array[String](nops)
  private val code2op = new Array[OpCodes](ncodes)
  private def enterOp(op: Char, code: String) = {
    op2code(op) = code
    val c = (code.charAt(1) - 'a') * 26 + code.charAt(2) - 'a'
    code2op(c) = new OpCodes(op, code, code2op(c))
  }

  /* Note: decoding assumes opcodes are only ever lowercase. */
  enterOp('~', "$tilde")
  enterOp('=', "$eq")
  enterOp('<', "$less")
  enterOp('>', "$greater")
  enterOp('!', "$bang")
  enterOp('#', "$hash")
  enterOp('%', "$percent")
  enterOp('^', "$up")
  enterOp('&', "$amp")
  enterOp('|', "$bar")
  enterOp('*', "$times")
  enterOp('/', "$div")
  enterOp('+', "$plus")
  enterOp('-', "$minus")
  enterOp(':', "$colon")
  enterOp('\\', "$bslash")
  enterOp('?', "$qmark")
  enterOp('@', "$at")

  /** Replace operator symbols by corresponding `\$opname`.
   *
   *  @param name the string to encode
   *  @return     the string with all recognized opchars replaced with their encoding
   */
  def encode[N <: Name](name: N): N = {
    var buf: StringBuilder = null
    val len = name.length
    var i = 0
    while (i < len) {
      val c = name(i)
      if (c < nops && (op2code(c) ne null)) {
        if (buf eq null) {
          buf = new StringBuilder()
          buf.append(name.slice(0, i))
        }
        buf.append(op2code(c))
      /* Handle glyphs that are not valid Java/JVM identifiers */
      }
      else if (!Character.isJavaIdentifierPart(c)) {
        if (buf eq null) {
          buf = new StringBuilder()
          buf.append(name.slice(0, i))
        }
        buf.append("$u%04X".format(c.toInt))
      }
      else if (buf ne null) {
        buf.append(c)
      }
      i += 1
    }
    if (buf eq null) name
    else if (name.isTermName) buf.toString.toTermName.asInstanceOf[N]
    else buf.toString.toTypeName.asInstanceOf[N]
  }
}
