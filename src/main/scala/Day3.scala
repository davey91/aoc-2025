import Day3.A.Bank

import scala.annotation.tailrec
import scala.io.Source

object Day3:
  val lines = Source.fromResource("day3/input.txt").getLines().toSeq

  object A:
    case class Bank(batteries: String):
      @tailrec
      final def findLargestJoltage(depth: Int, batteriesLeft: String = batteries.take(batteries.length - 1), batteryBank: String = ""): BigDecimal =
        if depth == 0 then
          val lastBattery = s"$batteriesLeft${batteries.last}".foldLeft(0)((highestBattery, newBattery) =>
            if newBattery.asDigit > highestBattery then newBattery.asDigit else highestBattery
          )
          BigDecimal(s"$batteryBank$lastBattery")
        else
          val (highest, index) = batteriesLeft
            .zipWithIndex
            .foldRight((0, 0))({ case ((newBattery, index), highestBatteryAndIndex) =>
              if newBattery.asDigit >= highestBatteryAndIndex._1 && batteriesLeft.splitAt(index + 1)._2.length + 1 >= depth then (newBattery.asDigit, index)
              else highestBatteryAndIndex
            })
          val updatedBank = s"$batteryBank$highest"
          findLargestJoltage(depth - 1, batteriesLeft.splitAt(index + 1)._2, updatedBank)

  def day3A(): BigDecimal =
    lines.map(Bank).map(_.findLargestJoltage(1)).sum

  def day3B(): BigDecimal =
    lines.map(Bank).map(_.findLargestJoltage(11)).sum


  @main
  def three(): Unit =
    println(s"3A: ${day3A()}")
    println(s"3B: ${day3B()}")
