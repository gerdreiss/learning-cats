package lectures.datamanipulation

import cats.data.Writer

object Writers extends App:

  val writer: Writer[List[String], Int] = Writer(List("processing started..."), 0)

  val incWriter = writer.map(_ + 1) // value increased, logs remains unchanged
  val logWriter = writer.mapWritten(_ :+ "processing running...") // value remains unchanged, log entry added
  val bothWriter = writer.bimap(_ :+ "value incremented", _ + 1) // value increased, log entry added
  val desiredValue = writer.value
  val logs = writer.written
  val (v, l) = writer.run

  println(desiredValue)
  println(logs)

  val writerA = Writer(Vector("starting processing A..."), 10)
  val writerB = Writer(Vector("starting processing B..."), 10)
  val compositeWriter =
    for
      valueA <- writerA.mapWritten(_ :+ "retrieving value A...")
      valueB <- writerB.mapWritten(_ :+ "retrieving value B...")
    yield valueA * valueB

  println(compositeWriter.run)

  val emptyWriter = writer.reset // clear the logs, resets the value
  println(emptyWriter.run)
