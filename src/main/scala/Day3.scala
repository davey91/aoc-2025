import Day3.A.Bank

import scala.annotation.tailrec
import scala.io.Source

object Day3:
  val lines = Source.fromResource("day3/input.txt").getLines().toSeq

  //17113
  object A:
    case class Bank(batteries: String):
      @tailrec
      final def findLargest(depth: Int, batteriesLeft: String = batteries.take(batteries.length - 1), batteryBank: String = ""): BigDecimal =
        //if batteriesLeft.length <= depth then s"$batteryBank$batteriesLeft"
        if depth == 0 then
          val lastBattery = s"$batteriesLeft${batteries.last}".foldLeft(0)((highestBattery, newBattery) =>
            //println(s"new: $newBattery, highest: ${highestBattery}")
            if newBattery.asDigit > highestBattery then newBattery.asDigit else highestBattery
          )
          BigDecimal(s"$batteryBank$lastBattery")
        else
          val (highest, index) = batteriesLeft
            .zipWithIndex
            .foldRight((0, 0))({ case ((newBattery, index), highestBatteryAndIndex) =>
              //println(s"new: $newBattery, highest: ${highestBatteryAndIndex._1}")
              //println(s"${batteriesLeft.splitAt(index + 1)}, ${batteriesLeft.splitAt(index + 1)._2}, $depth")
              if newBattery.asDigit >= highestBatteryAndIndex._1 && batteriesLeft.splitAt(index + 1)._2.length + 1 >= depth then (newBattery.asDigit, index)
              else highestBatteryAndIndex
            })
          //println(highest)
          //println()
          val updatedBank = s"$batteryBank$highest"
          findLargest(depth - 1, batteriesLeft.splitAt(index + 1)._2, updatedBank)

      def firstLargestBatteryAndIndex: (Int, Int) =
        batteries.take(batteries.length - 1)
          .zipWithIndex
          .foldRight((0, 0))({ case ((newBattery, index), highestBatteryAndIndex) =>
            //println(s"new: $newBattery, highest: ${highestBatteryAndIndex._1}")
            if newBattery.asDigit >= highestBatteryAndIndex._1 then (newBattery.asDigit, index)
            else highestBatteryAndIndex
          })

      def secondLargestBattery(indexFirst: Int): Int =
        val optimisedBatteries = batteries.splitAt(indexFirst + 1)._2
        optimisedBatteries.foldLeft(0)((highestBattery, newBattery) =>
          //println(s"new: $newBattery, highest: ${highestBattery}")
          if newBattery.asDigit > highestBattery then newBattery.asDigit else highestBattery
        )


      def largestJoltage: Int =
        val (firstBattery, index) = firstLargestBatteryAndIndex
        val secondBattery = secondLargestBattery(index)
        //println(s"first:$firstBattery, second: $secondBattery")
        s"$firstBattery$secondBattery".toInt


  def day3A(): BigDecimal =
    lines.map(Bank).map(_.findLargest(1)).sum

  def day3B(): BigDecimal =
    lines.map(Bank).map(_.findLargest(11)).sum


  @main
  def three(): Unit =
    println(s"3A: ${day3A()}")
    //println(s"3B: ${day3B()}")
