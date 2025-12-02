import scala.io.Source

object Day2:
  val lines = Source.fromResource("day2/input.txt").getLines().toSeq.mkString.split(",")

  object A:
    object UnevaluatedRange:
      def apply(unsplitString: String): UnevaluatedRange =
        val split = unsplitString.split("-")
        UnevaluatedRange(BigInt(split.head), BigInt(split.last))

    case class UnevaluatedRange(from: BigInt, to: BigInt):
      def isInvalidId(id: BigInt): Boolean =
        val idString = id.toString
        val (first, second) = idString.splitAt(idString.length / 2)
        first == second


      def evaluateInvalidIds: BigInt =
        (from to to).filter(isInvalidId).sum


  def day2A(): BigInt =
    import A.*
    lines.map(UnevaluatedRange.apply).map(_.evaluateInvalidIds).sum

  @main
  def two(): Unit =
    println(s"2A: ${day2A()}")
