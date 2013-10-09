package code.satyagraha.line_endings

import org.smach._

object TextScanner {

  case class LineIteratee() extends Iteratee[Char, String] {
    import Iteratee._

    private val CR = '\r'
    private val LF = '\n'

    trait Position extends State.Continuation[Char, String] {
      val lineNo: Int

      def decorate(message: String) = {
        message + " at line " + lineNo
      }

      def terminate(message: String): Halt[Char, String] = {
        Halt.fatal(decorate(message))
      }
    }

    case class Unknown extends Position {
      val lineNo = 0

      def apply(ch: Char) = {
        ch match {
          case CR => Continue(UnknownAfterCr(1))
          case LF => Continue(LinuxAfterLf(2))
          case _ => Continue(UnknownLine())
        }
      }

      def apply(eoi: EndOfInput) = {
        Succeed("empty")
      }
    }

    case class UnknownLine extends Position {
      val lineNo = 0

      def apply(ch: Char) = {
        ch match {
          case CR => Continue(UnknownAfterCr(1))
          case LF => Continue(LinuxAfterLf(2))
          case _ => Continue(UnknownLine())
        }
      }

      def apply(eoi: EndOfInput) = {
        terminate("no line terminator")
      }
    }

    case class UnknownAfterCr(lineNo: Int) extends Position {

      def apply(ch: Char) = {
        ch match {
          case CR => Continue(MacAfterCr(lineNo + 1))
          case LF => Continue(WindowsAfterCrLf(lineNo))
          case _ => Continue(MacLine(lineNo))
        }
      }

      def apply(eoi: EndOfInput) = {
        Succeed("Mac")
      }
    }

    case class MacAfterCr(lineNo: Int) extends Position {

      def apply(ch: Char) = {
        ch match {
          case CR => Continue(MacAfterCr(lineNo + 1))
          case LF => terminate("Mac: unexpected lf")
          case _ => Continue(MacLine(lineNo))
        }
      }

      def apply(eoi: EndOfInput) = {
        Succeed("Mac")
      }
    }

    case class MacLine(lineNo: Int) extends Position {

      def apply(ch: Char) = {
        ch match {
          case CR => Continue(MacAfterCr(lineNo + 1))
          case LF => terminate("Mac: unexpected lf")
          case _ => Continue(MacLine(lineNo))
        }
      }

      def apply(eoi: EndOfInput) = {
        terminate(decorate("Mac: missing cr"))
      }
    }

    case class WindowsAfterCrLf(lineNo: Int) extends Position {

      def apply(ch: Char) = {
        ch match {
          case CR => Continue(WindowsAfterCr(lineNo))
          case LF => terminate("Windows: missing cr before lf")
          case _ => Continue(WindowsLine(lineNo))
        }
      }

      def apply(eoi: EndOfInput) = {
        Succeed("Windows")
      }
    }

    case class WindowsLine(lineNo: Int) extends Position {

      def apply(ch: Char) = {
        ch match {
          case CR => Continue(WindowsAfterCr(lineNo))
          case LF => terminate("Windows: missing cr befire lf")
          case _ => Continue(WindowsLine(lineNo))
        }
      }

      def apply(eoi: EndOfInput) = {
        terminate("Windows: missing crlf")
      }
    }

    case class WindowsAfterCr(lineNo: Int) extends Position {

      def apply(ch: Char) = {
        ch match {
          case CR => terminate("Windows: missing lf after cr")
          case LF => Continue(WindowsAfterCrLf(lineNo + 1))
          case _ => terminate("Windows: missing lf after cr")
        }
      }

      def apply(eoi: EndOfInput) = {
        terminate("Windows: missing lf")
      }
    }

    case class LinuxAfterLf(lineNo: Int) extends Position {

      def apply(ch: Char) = {
        ch match {
          case CR => terminate("Linux: unexpected cr")
          case LF => Continue(LinuxAfterLf(lineNo + 1))
          case _ => Continue(LinuxLine(lineNo))
        }
      }

      def apply(eoi: EndOfInput) = {
        Succeed("Linux")
      }
    }

    case class LinuxLine(lineNo: Int) extends Position {

      def apply(ch: Char) = {
        ch match {
          case CR => terminate("Linux: unexpected cr")
          case LF => Continue(LinuxAfterLf(lineNo + 1))
          case _ => Continue(LinuxLine(lineNo))
        }
      }

      def apply(eoi: EndOfInput) = {
        terminate("Linux: missing lf")
      }
    }

    override def s0 = new Unknown()
  }

  def getLineTermination(text: Seq[Char]): String = {
    val li = LineIteratee()

    val finalState = text.toEnumerator connect li run

    val res = finalState match {
      case c: Iteratee.Continue[Unit, String] => "incomplete: " + c
      case s: Iteratee.Succeed[Unit, String] => s.state.value
      case h: Iteratee.Halt[Unit, String] => h.state.issues(0).message
    }

    res
  }

}

object Main {

  val inp = List('\n', 'a', 'b', 'c')
  println("res: " + TextScanner.getLineTermination(inp))

  def main(args: Array[String]): Unit = {

  }

}