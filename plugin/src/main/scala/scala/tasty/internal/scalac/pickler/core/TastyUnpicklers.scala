package scala.tasty.internal.scalac.pickler
package core

import scala.collection.mutable
import PickleFormat._

import scala.tools.nsc.Global

trait TastyUnpicklers {
  self: TastyNames with TastyReaders =>
  import global.{Name, newTermName}

  object TastyUnpickler {

    class UnpickleException(msg: String) extends Exception(msg)

    abstract class SectionUnpickler[R](val name: String) {
      def unpickle(reader: TastyReader, tastyName: TastyName.Table): R
    }

  }

  import TastyUnpickler._

  class TastyUnpickler(reader: TastyReader) {

    import reader._

    private val sectionReader = new mutable.HashMap[String, TastyReader]
    val tastyName = new TastyName.Table

    def check(cond: Boolean, msg: => String) =
      if (!cond) throw new UnpickleException(msg)

    def readString(): String = {
      val TastyName.Simple(name) = tastyName(readNameRef())
      name.toString
    }

    def readName(): TastyName = {
      import TastyName._
      val tag = readByte()
      val length = readNat()
      val start = currentAddr
      val end = start + length
      val result = tag match {
        case UTF8 =>
          skipTo(end)
          //TODO - termName changed to newTermName
          Simple(newTermName(bytes, start.index, length))
        case QUALIFIED =>
          Qualified(readNameRef(), readNameRef())
        case SIGNED =>
          val original = readNameRef()
          val result = readNameRef()
          val params = until(end)(readNameRef())
          Signed(original, params, result)
        case EXPANDED =>
          Expanded(readNameRef())
        case MODULECLASS =>
          ModuleClass(readNameRef())
        case SUPERACCESSOR =>
          SuperAccessor(readNameRef())
        case DEFAULTGETTER =>
          DefaultGetter(readNameRef(), readNat())
      }
      assert(currentAddr == end, s"bad name $result $start $currentAddr $end")
      result
    }

    locally {
      val magic = readBytes(8)
      check(magic.map(_.toChar).mkString == header, "not a TASTy file")
      val major = readNat()
      val minor = readNat()
      check(major == MajorVersion && (major != 0 || minor == MinorVersion),
        s"""TASTy signature has wrong version.
         | expected: $MajorVersion.$MinorVersion
         | found   : $major.$minor""".stripMargin)
      until(readEnd()) {
        tastyName.add(readName())
      }
      while (!atEnd) {
        val secName = readString()
        val secEnd = readEnd()
        sectionReader(secName) = new TastyReader(bytes, currentAddr, secEnd)
        skipTo(secEnd)
      }
    }

    def unpickled[R](sec: SectionUnpickler[R]): Option[R] =
      for (reader <- sectionReader.get(sec.name)) yield
        sec.unpickle(reader, tastyName)
  }

}
