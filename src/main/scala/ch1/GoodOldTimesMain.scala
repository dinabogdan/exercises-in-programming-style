package ch1

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths, StandardCopyOption}
import java.util

import scala.collection.convert.ImplicitConversionsToScala.`iterable AsScalaIterable`
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.control.Breaks._

object GoodOldTimesMain {

  def read(filename: String): util.List[String] = {
    val path = Paths.get(".").normalize().toAbsolutePath.resolve("words-freq.txt")
    if (Files.exists(path))
      Files.readAllLines(path)
    else new util.ArrayList[String]()
  }

  def updateCounter(filename: String, word: String, counter: Int): Unit = {
    val f2 = new File(s"$filename.tmp")
    val w = new PrintWriter(f2)
    val f1 = read(filename)
    if (counter != 1) {
      f1.map(line => if (line.matches(s"$word \\d*")) s"$word $counter" else line)
        .foreach(line => w.println(line))
      w.close()
      Files.move(f2.toPath, new File(filename).toPath, StandardCopyOption.REPLACE_EXISTING)
      ()
    } else {
      f1.foreach(line => w.println(line))
      w.println(s"$word $counter")
      w.close()
      Files.move(f2.toPath, new File(filename).toPath, StandardCopyOption.REPLACE_EXISTING)
      ()
    }
  }

  def main(args: Array[String]): Unit = {
    var data = mutable.ListBuffer[Any]()

    data.addOne(FileUtils.readStopWords("stop-words.txt")) // add stop words array to the data variable on pos 0
    data.addOne("") // the line that is read from the input file on pos 1
    data.addOne(Array[String]()) // the line that is splitted by , on pos 2
    data.addOne(mutable.ListBuffer[String]()) // all the words in a line on pos 3
    data.addOne("") // the line in the word-freq file on pos 4
    data.addOne("") // the word from an iteration from word-freq file on pos 5
    data.addOne(Int.box(1)) // the counter for the word from an iteration from word-freq file on pos 6
    data.addOne(Boolean.box(false)) // the flag which indicates if the word was found in the word-freq file on pos 7

    val f = Source.fromResource("input1.txt").getLines().iterator

    while (f.hasNext) {
      data(1) = f.next
      println(s"Read: ${data(1)}")

      data(2) = (data(1).asInstanceOf[String]).split(",")
      for (d <- data(2).asInstanceOf[Array[String]]) {
        for (d1 <- d.trim.split(" ")) {
          if (d1.length > 2 && !data(0).asInstanceOf[List[String]].contains(d1.toLowerCase))
            data(3).asInstanceOf[mutable.ListBuffer[String]].addOne(d1.toLowerCase)
        }
      }

      for (word <- data(3).asInstanceOf[ListBuffer[String]]) {
        if (word.matches("^[a-zA-Z0-9]*$")) {
          val g = read("word-freq.txt").iterator()
          while (g.hasNext) {
            data(4) = g.next()
            data(5) = data(4).asInstanceOf[String].split(" ")(0)
            data(6) = Int.box(data(4).asInstanceOf[String].split(" ")(1).toInt)
            if (data(5).asInstanceOf[String] == word) {
              data(7) = Boolean.box(true)
              data(6) = Int.box(data(6).asInstanceOf[Int] + 1)
              updateCounter("C:\\DevTools\\workspace\\exercises-in-programming-style\\words-freq.txt", word, data(6).asInstanceOf[Int])
            }
          }
          if (!data(7).asInstanceOf[Boolean]) {
            updateCounter("C:\\DevTools\\workspace\\exercises-in-programming-style\\words-freq.txt", word, 1)
          }
          data(7) = Boolean.box(false)
        }
      }
      data(3).asInstanceOf[mutable.ListBuffer[String]].clear()
    }
    data.clear()
    0 to 24 foreach { _ => data.addOne(mutable.ListBuffer[Any]()) }
    data.addOne("") // word/ word freq on pos 25
    data.addOne(0) // freq on pos 26
    data.addOne(0) // index on pos 27
    val h = read("word_freq.txt").iterator()
    while (h.hasNext) {
      data(25) = h.next()
      data(26) = data(25).asInstanceOf[String].split(" ")(1).toInt
      data(25) = data(25).asInstanceOf[String].split(" ")(0)
      data(27) = 0
      breakable {
        while (data(27).asInstanceOf[Int] < 25) {
          if (data(data(27).asInstanceOf[Int]).asInstanceOf[mutable.ListBuffer[Any]].isEmpty ||
            data(data(27).asInstanceOf[Int]).asInstanceOf[mutable.ListBuffer[Any]](1).asInstanceOf[Int] < data(26).asInstanceOf[Int]
          ) {
            data(data(27).asInstanceOf[Int]) = mutable.ListBuffer(data(25), data(26))
//            data.remove(25)
            break()
          }
          data(27) = data(27).asInstanceOf[Int] + 1
        }
      }
    }
    val res = data.take(25)
      .map(x => x.asInstanceOf[ListBuffer[Any]])
      .filter(x => x.nonEmpty)
      .map(x => Tuple2(x(0).asInstanceOf[String], x(1).asInstanceOf[Int]))
      .sortBy(t => t._2)
      .toMap

    println(res)
  }
}
