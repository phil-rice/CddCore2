/** Copyright (c) 2016, Phil Rice. Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
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
//        println("Make file: " + file.getAbsoluteFile)
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
