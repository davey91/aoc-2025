import Day4.A.{ObjectInGrid}
import Day4.A.ObjectType.{Empty, RollOfPaper}

import scala.annotation.tailrec
import scala.io.Source

object Day4:
  val lines = Source.fromResource("day4/input.txt").getLines().toSeq

  object A:
    enum ObjectType:
      case Empty, RollOfPaper

    object ObjectInGrid:
      def apply(char: Char, x: Int, y: Int): ObjectInGrid =
        val objectType = char match {
          case '.' => Empty
          case '@' => RollOfPaper
        }
        ObjectInGrid(objectType, y, x)

    case class ObjectInGrid(objectType: ObjectType, y: Int, x: Int):
      private val (northY, northX) = (y-1, x)
      private val (northEastY, northEastX) = (y-1, x+1)
      private val (eastY, eastX) = (y, x+1)
      private val (southEastY, southEastX) = (y+1, x+1)
      private val (southY, southX) = (y+1, x)
      private val (southWestY, southWestX) = (y+1, x-1)
      private val (westY, westX) = (y, x-1)
      private val (northWestY, northWestX) = (y-1, x-1)

      private val allAdjacent = Seq((northY, northX),(northEastY, northEastX), (eastY, eastX), (southEastY, southEastX), (southY, southX), (southWestY, southWestX), (westY, westX), (northWestY, northWestX))

      def isMe(yCheck: Int, xCheck: Int): Boolean = y == yCheck && x == xCheck

      def canBeAccessed(objects: Seq[ObjectInGrid]): Boolean =
        objects.filter(o => allAdjacent.exists(o.isMe)).count(o => o.objectType == RollOfPaper) < 4

  def day4A(): BigDecimal = {
    val objects = lines.zipWithIndex.flatMap: (line, y) =>
      line.zipWithIndex.map: (char, x) =>
        ObjectInGrid(char, x, y)

    objects.count(o => o.objectType == RollOfPaper && o.canBeAccessed(objects.diff(Seq(o))))
  }

  object B:
    @tailrec
    final def remove(objects: Seq[ObjectInGrid], count: Int = 0): Int = {
      val thisCount = objects.count(o => o.objectType == RollOfPaper && o.canBeAccessed(objects.diff(Seq(o))))
      println(s"current: $thisCount")
      if thisCount == 0 then count
      else
        val updated = objects.map: o =>
          if o.objectType == RollOfPaper && o.canBeAccessed(objects.diff(Seq(o))) then
            o.copy(objectType = Empty)
          else o
        remove(updated, count + thisCount)
    }


  def day4B(): BigDecimal =
    import B.*
    val objects = lines.zipWithIndex.flatMap: (line, y) =>
      line.zipWithIndex.map: (char, x) =>
        ObjectInGrid(char, x, y)

    remove(objects)

  @main
  def four(): Unit =
    println(s"4A: ${day4A()}")
    println(s"4B: ${day4B()}")
