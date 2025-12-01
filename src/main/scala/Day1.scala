import scala.annotation.tailrec
import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.io.Source

object Day1:

  def day1A(): Int =
    val lines = Source.fromResource("day1/input.txt").getLines().toSeq

    @tailrec
    def countAccumulatedZeroes(directionAndNumbers: Seq[String], dialRotation: Int = 50, totalZeroes: Int = 0): Int =
      directionAndNumbers match
        case Nil => totalZeroes
        case directionAndNumber :: tail =>
          val value = directionAndNumber.splitAt(1)._2.toInt % 100
          val newTotal = if directionAndNumber.startsWith("L") then
            if (dialRotation - value) < 0 then 100 - Math.abs((dialRotation - value) % 100) else dialRotation - value
          else if (dialRotation + value) > 99 then 0 + Math.abs((dialRotation + value) % 100) else dialRotation + value
          val newTotalZeroes = if (newTotal == 0) totalZeroes + 1 else totalZeroes
          countAccumulatedZeroes(tail, newTotal, newTotalZeroes)

    countAccumulatedZeroes(lines)

  @main
  def one(): Unit =
    println(s"1A: ${day1A()}")
