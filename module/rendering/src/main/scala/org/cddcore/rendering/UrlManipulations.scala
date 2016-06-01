package org.cddcore.rendering

import java.io.{FileWriter, _}


trait UrlManipulations {

  def makeUrl(urlBase: String, idPathResult: String): String

  def makeFile(url: String, text: String)

  def populateInitialFiles(referenceFilesUrlBase: String)
}

class WebsiteUrlManipulators extends UrlManipulations {

  def makeUrl(urlBase: String, idPathResult: String) = urlBase + idPathResult

  def makeFile(url: String, text: String) = ???

  def populateInitialFiles(referenceFilesUrlBase: String) = ???

}

class FileUrlManipulations extends UrlManipulations {

  private def transfer(in: InputStream, out: OutputStream) {
    val BufferSize = 8192
    val buffer = new Array[Byte](BufferSize)
    def read() {
      val byteCount = in.read(buffer)
      if (byteCount >= 0) {
        out.write(buffer, 0, byteCount)
        read()
      }
    }
    read()
  }

  def copyFromClassPathToFile(resourceId: String, file: File): Unit = {
    def useClosable[S <: Closeable, X](makeS: => S)(useS: S => X) = {
      val s = makeS
      try {
        useS(s)
      } finally {
        s.close()
      }
    }
    file.getParentFile.mkdirs()
    file.createNewFile()
    useClosable(getClass.getClassLoader.getResourceAsStream(resourceId))(
      inputStream => useClosable(new FileOutputStream(file))(outputStream =>
        transfer(inputStream, outputStream)))
  }

  def makeUrl(urlBase: String, idPathResult: String) = new File(urlBase + "/" + idPathResult + ".html").getCanonicalFile.toString

  def makeFile(url: String, text: String): Unit = {
    val file = new File(url)
    //    println("Make file: " + file.getAbsoluteFile)
    file.getParentFile.mkdirs()
    val writer = new FileWriter(file)
    try {
      writer.write(text)
    } finally (writer.close)
  }

  private val initialFiles = List("images/engine.png", "images/scenario.png", "images/errorScenario.png", "images/usecase.png", "images/document.png", "images/cdd.png", "stylesheets/css.css")

  def populateInitialFiles(referenceFilesUrlBase: String) = {
    initialFiles.foreach(f => copyFromClassPathToFile(f, new File(referenceFilesUrlBase + "/" + f)))
  }
}
