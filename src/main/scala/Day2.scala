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

  object B:
    object UnevaluatedRange:
      def apply(unsplitString: String): UnevaluatedRange =
        val split = unsplitString.split("-")
        UnevaluatedRange(BigInt(split.head), BigInt(split.last))

    case class UnevaluatedRange(from: BigInt, to: BigInt):
      def isInvalidId(id: BigInt): Boolean =
        val idCharArray = id.toString.toCharArray
        val idString = id.toString
        val slidingMaxSize = idString.length / 2

        if idString.length <= 2 then
          val (first, second) = idString.splitAt(slidingMaxSize)
          //if(first == second) then
            //println(s"$id, ${first == second}")
          first == second
        else (1 to slidingMaxSize).exists: slidingSize =>
          //if(idString.sliding(slidingSize, slidingSize).distinct.size == 1) then
            //println(s"id: $id, slidingSize: $slidingMaxSize")
            //println(s"slider: ${idString.sliding(slidingSize, slidingSize).toSeq}")
            //println(s"invalid: ${idString.sliding(slidingSize, slidingSize).distinct.size == 1}")
          idString.sliding(slidingSize, slidingSize).distinct.size == 1

      def evaluateInvalidIds: BigInt =
        (from to to).filter(isInvalidId).sum

  def day2A(): BigInt =
    import A.*
    lines.map(UnevaluatedRange.apply).map(_.evaluateInvalidIds).sum

  def day2B(): BigInt =
    import B.*
    lines.map(UnevaluatedRange.apply).map(_.evaluateInvalidIds).sum

  @main
  def two(): Unit =
    println(s"2A: ${day2A()}")
    println(s"2B: ${day2B()}")
