package ch1

import scala.io.Source

object FileUtils {

  def readStopWords(filename: String): List[String] = {
    val file = Source.fromResource(filename)
    val stopWords = file.getLines()
      .take(1)
      .flatMap(line =>
        line.split(",")
      ).toArray
    file.close()
    stopWords.toList
  }

}
