package code.satyagraha.line_endings

object PathTraversal {
  import java.io.File

  def getFileTree(file: File, descend: (File) => Boolean): Stream[File] = {
    if (file.isDirectory && descend(file)) {
      file #:: file.listFiles().toStream.flatMap(child => getFileTree(child, descend))
    } else {
      file #:: Stream.empty
    }
  }

  def getCharSeq(file: File, encoding: String): Seq[Char] = {
    val source = scala.io.Source.fromFile(file, encoding)
    source.toSeq
  }

  def extension(file: File): String = {
    val name = file.getName()
    val dotPos = name.lastIndexOf(".")
    if (dotPos > 0) {
      name.substring(dotPos + 1)
    } else {
      ""
    }
  }

}

