package code.satyagraha.line_endings

object Main {
  import java.io.File
  import java.io.IOException

  import Function.tupled

  import PathTraversal._
  import TextScanner._

  def main(args: Array[String]): Unit = {

    val dirBlacklist = Set("target", ".settings", ".prefs")

    def isOkDir(file: File): Boolean = {
      !(file.isHidden() || dirBlacklist.contains(file.getName()))
    }

    val extBlacklist = Set("gif", "png", "zip", "jar", "class", "ispace", "ucls")

    val rootDir = new File(args(0))
    println("scanning: " + rootDir)
    println
    
    val filesInfoStream = for {
      file <- getFileTree(rootDir, isOkDir)
      if (file.isFile())
      ext = extension(file)
      if (!extBlacklist.contains(ext))
    } yield (file, ext, getLineTermination(getCharSeq(file, "UTF-8")))

    val filesInfo = filesInfoStream.toList
    val extensions = filesInfo.map(tupled((file, ext, termination) => ext)).toSet
    filesInfo foreach { case (file, ext, termination) =>
//        if (!Set("Windows", "Linux").contains(termination)) {
          println(termination + " - " + file)
//        }
    }
    println
    println("extensions: " + extensions.toList.sorted.mkString(", "))
  }

}