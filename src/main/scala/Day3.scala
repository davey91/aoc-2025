import Day3.A.Bank

import scala.io.Source

object Day3:
  val lines = Source.fromResource("day3/input.txt").getLines().toSeq

  object A:
    case class Bank(batteries: String):
      def firstLargestBatteryAndIndex: (Int, Int) =
        batteries.take(batteries.length - 1)
          .zipWithIndex
          .foldRight((0, 0))({ case ((newBattery, index), highestBatteryAndIndex) =>
            //println(s"new: $newBattery, highest: ${highestBatteryAndIndex._1}")
            if newBattery.asDigit >= highestBatteryAndIndex._1 then (newBattery.asDigit, index)
            else highestBatteryAndIndex
          })

      def secondLargestBattery(indexFirst: Int): Int =
        val optimisedBatteries = batteries.splitAt(indexFirst+1)._2
        optimisedBatteries.foldLeft(0)((highestBattery, newBattery) =>
          //println(s"new: $newBattery, highest: ${highestBattery}")
          if newBattery.asDigit > highestBattery then newBattery.asDigit else highestBattery
        )


      def largestJoltage: Int =
        val (firstBattery, index) = firstLargestBatteryAndIndex
        val secondBattery = secondLargestBattery(index)
        //println(s"first:$firstBattery, second: $secondBattery")
        s"$firstBattery$secondBattery".toInt


  def day3A(): BigInt =
    lines.map(Bank).map(_.largestJoltage).sum


  @main
  def three(): Unit =
    println(s"3A: ${day3A()}")
