import Day4.A.ObjectInGrid
import Day4.A.ObjectType.{Empty, RollOfPaper}

import scala.annotation.tailrec
import scala.io.Source

object Day5:
  val lines = Source.fromResource("day5/input.txt").getLines().toSeq

  object A:
    val ingredientRegex = """(\d{1,})-(\d{1,})""".r

    object IngredientsRange:
      def apply(raw: String): IngredientsRange =
        val rawRegex = ingredientRegex.findAllIn(raw)
        IngredientsRange(BigInt(rawRegex.group(1)), BigInt(rawRegex.group(2)))

    case class IngredientsRange(from: BigInt, to: BigInt):
      def isFresh(ingredient: Ingredient): Boolean =
        ingredient.identifier >= from && ingredient.identifier <= to

    object Ingredient:
      def apply(raw: String): Ingredient =
        Ingredient(BigInt(raw))

    case class Ingredient(identifier: BigInt)

  def day5A(): BigInt =
    import A.*
    val (ingredientRanges: Seq[IngredientsRange], ingredients: Seq[Ingredient]) = lines
      .flatMap(l => if l.contains("-") then Some(IngredientsRange(l)) else if l.nonEmpty then Some(Ingredient(l)) else None)
      .partition(_.isInstanceOf[IngredientsRange])

    ingredients.count(ingredient => ingredientRanges.exists(_.isFresh(ingredient)))

  @main
  def five(): Unit =
    println(s"5A: ${day5A()}")
