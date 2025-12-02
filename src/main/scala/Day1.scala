import scala.annotation.tailrec
import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.io.Source

object Day1:
  val lines = Source.fromResource("day1/test-input.txt").getLines().toSeq

  def day1A(): Int =

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

  def day1B(): Int =

    @tailrec
    def countAccumulatedZeroes(directionAndNumbers: Seq[String], dialRotation: Int = 50, totalClicks: Int = 0): Int =
      directionAndNumbers match
        case Nil => totalClicks
        case directionAndNumber :: tail =>
          // add value > 1000 to clicks
          val extraClicks = directionAndNumber.splitAt(1)._2.toInt % 1000 / 100
          val value = directionAndNumber.splitAt(1)._2.toInt % 100
          val (newRotation, clicks) = if directionAndNumber.startsWith("L") then
            if (dialRotation - value) <= 0 then {
              println(value)
              val clicks = if(value > 99) 1 + value / 100 else 1
              (100 - Math.abs((dialRotation - value) % 100), clicks)
            } else (dialRotation - value, 0)
          else if (dialRotation + value) > 99 then
            val clicks = if(value > 99) 1 + value / 100 else 1
            (0 + Math.abs((dialRotation + value) % 100), clicks)
          else (dialRotation + value, 0)

          val clicksToAdd = if((dialRotation == 0 || dialRotation == 100) && clicks > 0) clicks - 1 else clicks
          println(s"startRotation: $dialRotation Direction: $directionAndNumber, Pointed at:$newRotation, new clicks: $clicksToAdd, totalClicks: ${totalClicks + clicksToAdd}")
          countAccumulatedZeroes(tail, newRotation, totalClicks + clicksToAdd)

    countAccumulatedZeroes(lines)

  @main
  def one(): Unit =
    println(s"1A: ${day1A()}")
    println(s"1B: ${day1B()}")
