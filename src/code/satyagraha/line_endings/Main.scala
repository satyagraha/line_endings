package code.satyagraha.line_endings

import org.smach._

case class LineIteratee() extends Iteratee[Char, String] {
  import Iteratee._

  private val CR = '\r'
  private val LF = '\n'

  case class Unknown extends State.Continuation[Char, String] {

    def apply(ch: Char) = {
      ch match {
        case CR => Continue(UnknownAfterCr())
        case LF => Continue(LinuxAfterLf())
        case _ => Continue(UnknownLine())
      }
    }

    def apply(eoi: EndOfInput) = {
      Succeed("empty")
    }
  }

  case class UnknownLine extends State.Continuation[Char, String] {

    def apply(ch: Char) = {
      ch match {
        case CR => Continue(UnknownAfterCr())
        case LF => Continue(LinuxAfterLf())
        case _ => Continue(UnknownLine())
      }
    }

    def apply(eoi: EndOfInput) = {
      Halt.fatal("no line terminator")
    }
  }

  case class UnknownAfterCr extends State.Continuation[Char, String] {

    def apply(ch: Char) = {
      ch match {
        case CR => Continue(MacAfterCr())
        case LF => Continue(WindowsAfterCrLf())
        case _ => Continue(MacLine())
      }
    }

    def apply(eoi: EndOfInput) = {
      Succeed("Mac")
    }
  }

  case class MacAfterCr extends State.Continuation[Char, String] {

    def apply(ch: Char) = {
      ch match {
        case CR => Continue(MacAfterCr())
        case LF => Halt.fatal("Mac: unexpected lf")
        case _ => Continue(MacLine())
      }
    }

    def apply(eoi: EndOfInput) = {
      Succeed("Mac")
    }
  }

  case class MacLine extends State.Continuation[Char, String] {

    def apply(ch: Char) = {
      ch match {
        case CR => Continue(MacAfterCr())
        case LF => Halt.fatal("Mac: unexpected lf")
        case _ => Continue(MacLine())
      }
    }

    def apply(eoi: EndOfInput) = {
      Halt.fatal("Mac: missing cr")
    }
  }

  case class WindowsAfterCrLf extends State.Continuation[Char, String] {

    def apply(ch: Char) = {
      ch match {
        case CR => Continue(WindowsAfterCr())
        case LF => Halt.fatal("Windows: missing cr before lf")
        case _ => Continue(WindowsLine())
      }
    }

    def apply(eoi: EndOfInput) = {
      Succeed("Windows")
    }
  }

  case class WindowsLine extends State.Continuation[Char, String] {

    def apply(ch: Char) = {
      ch match {
        case CR => Continue(WindowsAfterCr())
        case LF => Halt.fatal("Windows: missing cr befire lf")
        case _ => Continue(WindowsLine())
      }
    }

    def apply(eoi: EndOfInput) = {
      Halt.fatal("Windows: missing crlf")
    }
  }

  case class WindowsAfterCr extends State.Continuation[Char, String] {

    def apply(ch: Char) = {
      ch match {
        case CR => Halt.fatal("Windows: missing lf after cr")
        case LF => Continue(WindowsAfterCrLf())
        case _ => Halt.fatal("Windows: missing lf after cr")
      }
    }

    def apply(eoi: EndOfInput) = {
      Halt.fatal("Windows: missing lf")
    }
  }

  case class LinuxAfterLf extends State.Continuation[Char, String] {

    def apply(ch: Char) = {
      ch match {
        case CR => Halt.fatal("Linux: unexpected cr")
        case LF => Continue(LinuxAfterLf())
        case _ => Continue(LinuxLine())
      }
    }

    def apply(eoi: EndOfInput) = {
      Succeed("Linux")
    }
  }

  case class LinuxLine extends State.Continuation[Char, String] {

    def apply(ch: Char) = {
      ch match {
        case CR => Halt.fatal("Linux: unexpected cr")
        case LF => Continue(LinuxAfterLf())
        case _ => Continue(LinuxLine())
      }
    }

    def apply(eoi: EndOfInput) = {
      Halt.fatal("Linux: missing lf")
    }
  }

  override def s0 = Unknown()
}

object Main {

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

  val inp = List('\n', 'a', 'b', 'c')
  println("res: " + getLineTermination(inp))

  def main(args: Array[String]): Unit = {

  }

}