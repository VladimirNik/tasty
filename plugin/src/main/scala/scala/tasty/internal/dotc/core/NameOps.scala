package scala.tasty.internal.dotc
package core

import java.security.MessageDigest
import scala.annotation.switch
import scala.io.Codec
import Names._, StdNames._, Contexts._, Symbols._, Flags._
import Decorators.StringDecorator

object NameOps {
  class PrefixNameExtractor(pre: TermName) {
    def apply(name: TermName): TermName = pre ++ name
    def unapply(name: TermName): Option[TermName] =
      if (name startsWith pre) Some(name.drop(pre.length).asTermName) else None
  }

  object SuperAccessorName extends PrefixNameExtractor(nme.SUPER_PREFIX)
  object InitializerName extends PrefixNameExtractor(nme.INITIALIZER_PREFIX)

  implicit class NameDecorator[N <: Name](val name: N) extends AnyVal {
    import nme._

    def likeTyped(n: Name): N =
      (if (name.isTermName) n.toTermName else n.toTypeName).asInstanceOf[N]

    def isConstructorName = name == CONSTRUCTOR || name == TRAIT_CONSTRUCTOR
    def isExceptionResultName = name startsWith EXCEPTION_RESULT_PREFIX
    def isImplClassName = name endsWith IMPL_CLASS_SUFFIX
    def isLocalDummyName = name startsWith LOCALDUMMY_PREFIX
    def isLoopHeaderLabel = (name startsWith WHILE_PREFIX) || (name startsWith DO_WHILE_PREFIX)
    def isProtectedAccessorName = name startsWith PROTECTED_PREFIX
    def isReplWrapperName = name containsSlice INTERPRETER_IMPORT_WRAPPER
    def isSetterName = name endsWith SETTER_SUFFIX
    def isSingletonName = name endsWith SINGLETON_SUFFIX
    def isAvoidClashName = name endsWith AVOID_CLASH_SUFFIX
    def isImportName = name startsWith IMPORT
    def isFieldName = name endsWith LOCAL_SUFFIX
    //*
    def isShadowedName = name.length > 0 && name.head == '(' && name.startsWith(nme.SHADOWED)
    def isDefaultGetterName = name.isTermName && name.asTermName.defaultGetterIndex >= 0
    def isScala2LocalSuffix = name.endsWith(" ")
    def isModuleVarName(name: Name): Boolean =
      name.stripAnonNumberSuffix endsWith MODULE_VAR_SUFFIX

    /** Is name a variable name? */
    def isVariableName: Boolean = name.length > 0 && {
      val first = name.head
      (((first.isLower && first.isLetter) || first == '_')
        && (name != false_)
        && (name != true_)
        && (name != null_))
    }

    /** Is this the name of a higher-kinded type parameter of a Lambda? */
    def isLambdaArgName =
      name.length > 0 &&
      name.head == tpnme.LAMBDA_ARG_PREFIXhead &&
      name.startsWith(tpnme.LAMBDA_ARG_PREFIX) && {
        val digits = name.drop(tpnme.LAMBDA_ARG_PREFIX.length)
        digits.length <= 4 && digits.forall(_.isDigit)
      }

    /** The index of the higher-kinded type parameter with this name.
     *  Pre: isLambdaArgName.
     */
    def lambdaArgIndex: Int =
      name.drop(tpnme.LAMBDA_ARG_PREFIX.length).toString.toInt

    /** If the name ends with $nn where nn are
      * all digits, strip the $ and the digits.
      * Otherwise return the argument.
      */
    def stripAnonNumberSuffix: Name = {
      var pos = name.length
      while (pos > 0 && name(pos - 1).isDigit)
        pos -= 1

      if (pos > 0 && pos < name.length && name(pos - 1) == '$')
        name take (pos - 1)
      else
        name
    }

    /** Append a suffix so that this name does not clash with another name in the same scope */
    def avoidClashName: TermName = (name ++ AVOID_CLASH_SUFFIX).toTermName

    /** If name ends in "avoid clash" suffix, drop it */
    def stripAvoidClashSuffix: Name =
      if (isAvoidClashName) name dropRight AVOID_CLASH_SUFFIX.length else name

    def shadowedName: N = likeTyped(nme.SHADOWED ++ name)

    //*
    def revertShadowed: N = likeTyped(name.drop(nme.SHADOWED.length))

    def implClassName: N = likeTyped(name ++ tpnme.IMPL_CLASS_SUFFIX)

    /** Translate a name into a list of simple TypeNames and TermNames.
     *  In all segments before the last, type/term is determined by whether
     *  the following separator char is '.' or '#'.  The last segment
     *  is of the same type as the original name.
     *
     *  Examples:
     *
     *  package foo {
     *    object Lorax { object Wog ; class Wog }
     *    class Lorax  { object Zax ; class Zax }
     *  }
     *
     *  f("foo.Lorax".toTermName)  == List("foo": Term, "Lorax": Term) // object Lorax
     *  f("foo.Lorax".toTypeName)  == List("foo": Term, "Lorax": Type) // class Lorax
     *  f("Lorax.Wog".toTermName)  == List("Lorax": Term, "Wog": Term) // object Wog
     *  f("Lorax.Wog".toTypeName)  == List("Lorax": Term, "Wog": Type) // class Wog
     *  f("Lorax#Zax".toTermName)  == List("Lorax": Type, "Zax": Term) // object Zax
     *  f("Lorax#Zax".toTypeName)  == List("Lorax": Type, "Zax": Type) // class Zax
     *
     *  Note that in actual scala syntax you cannot refer to object Zax without an
     *  instance of Lorax, so Lorax#Zax could only mean the type.  One might think
     *  that Lorax#Zax.type would work, but this is not accepted by the parser.
     *  For the purposes of referencing that object, the syntax is allowed.
     */
    def segments: List[Name] = {
      def mkName(name: Name, follow: Char): Name =
        if (follow == '.') name.toTermName else name.toTypeName

      name.indexWhere(ch => ch == '.' || ch == '#') match {
        case -1 =>
          if (name.isEmpty) scala.Nil else name :: scala.Nil
        case idx =>
          mkName(name take idx, name(idx)) :: (name drop (idx + 1)).segments
      }
    }

    /** The name of the generic runtime operation corresponding to an array operation */
    def genericArrayOp: TermName = name match {
      case nme.apply => nme.array_apply
      case nme.length => nme.array_length
      case nme.update => nme.array_update
      case nme.clone_ => nme.array_clone
    }

    /** The name of the primitive runtime operation corresponding to an array operation */
    def primitiveArrayOp: TermName = name match {
      case nme.apply => nme.primitive.arrayApply
      case nme.length => nme.primitive.arrayLength
      case nme.update => nme.primitive.arrayUpdate
      case nme.clone_ => nme.clone_
    }

    def expandedPrefix: N = {
      val idx = name.lastIndexOfSlice(nme.EXPAND_SEPARATOR)
      assert(idx >= 0)
      name.take(idx).asInstanceOf[N]
    }

    def unexpandedName: N = {
      val idx = name.lastIndexOfSlice(nme.EXPAND_SEPARATOR)
      if (idx < 0) name else (name drop (idx + nme.EXPAND_SEPARATOR.length)).asInstanceOf[N]
    }
  }

  // needed???
  private val Boxed = Map[TypeName, TypeName](
    tpnme.Boolean -> jtpnme.BoxedBoolean,
    tpnme.Byte -> jtpnme.BoxedByte,
    tpnme.Char -> jtpnme.BoxedCharacter,
    tpnme.Short -> jtpnme.BoxedShort,
    tpnme.Int -> jtpnme.BoxedInteger,
    tpnme.Long -> jtpnme.BoxedLong,
    tpnme.Float -> jtpnme.BoxedFloat,
    tpnme.Double -> jtpnme.BoxedDouble)

  implicit class TermNameDecorator(val name: TermName) extends AnyVal {
    import nme._

    def setterName: TermName =
      if (name.isFieldName) name.fieldToGetter.setterName
      else name ++ SETTER_SUFFIX

    def getterName: TermName =
      if (name.isFieldName) fieldToGetter
      else setterToGetter

    def fieldName: TermName =
      if (name.isSetterName) getterName.fieldName
      else name ++ LOCAL_SUFFIX

    private def setterToGetter: TermName = {
      assert(name.endsWith(SETTER_SUFFIX), name + " is referenced as a setter but has wrong name format")
      name.take(name.length - SETTER_SUFFIX.length).asTermName
    }

    def fieldToGetter: TermName = {
      assert(name.isFieldName)
      name.take(name.length - LOCAL_SUFFIX.length).asTermName
    }

    /** If this is a default getter, its index (starting from 0), else -1 */
    def defaultGetterIndex: Int = {
      var i = name.length
      while (i > 0 && name(i - 1).isDigit) i -= 1
      if (i > 0 && i < name.length && name.take(i).endsWith(DEFAULT_GETTER))
        name.drop(i).toString.toInt - 1
      else
        -1
    }

    def stripScala2LocalSuffix: TermName =
      if (name.isScala2LocalSuffix) name.init.asTermName else name

    def moduleVarName: TermName =
      name ++ MODULE_VAR_SUFFIX

    /** The name unary_x for a prefix operator x */
    def toUnaryName: TermName = name match {
      case raw.MINUS => UNARY_-
      case raw.PLUS  => UNARY_+
      case raw.TILDE => UNARY_~
      case raw.BANG  => UNARY_!
      case _ => name
    }

    /** The name of a method which stands in for a primitive operation
     *  during structural type dispatch.
     */
    def primitiveInfixMethodName: TermName = name match {
      case OR   => takeOr
      case XOR  => takeXor
      case AND  => takeAnd
      case EQ   => testEqual
      case NE   => testNotEqual
      case ADD  => add
      case SUB  => subtract
      case MUL  => multiply
      case DIV  => divide
      case MOD  => takeModulo
      case LSL  => shiftSignedLeft
      case LSR  => shiftLogicalRight
      case ASR  => shiftSignedRight
      case LT   => testLessThan
      case LE   => testLessOrEqualThan
      case GE   => testGreaterOrEqualThan
      case GT   => testGreaterThan
      case ZOR  => takeConditionalOr
      case ZAND => takeConditionalAnd
      case _    => NO_NAME
    }

    /** Postfix/prefix, really.
     */
    def primitivePostfixMethodName: TermName = name match {
      case UNARY_!    => takeNot
      case UNARY_+    => positive
      case UNARY_-    => negate
      case UNARY_~    => complement
      case `toByte`   => toByte
      case `toShort`  => toShort
      case `toChar`   => toCharacter
      case `toInt`    => toInteger
      case `toLong`   => toLong
      case `toFloat`  => toFloat
      case `toDouble` => toDouble
      case _          => NO_NAME
    }

    def primitiveMethodName: TermName =
      primitiveInfixMethodName match {
        case NO_NAME => primitivePostfixMethodName
        case name => name
      }
  }
}
