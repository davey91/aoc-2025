import java.time.Instant
import scala.io.Source

object Day5:
  val lines = Source.fromResource("day5/input.txt").getLines().toSeq

  val ingredientRegex = """(\d{1,})-(\d{1,})""".r

  object IngredientsRange:
    def apply(raw: String): IngredientsRange =
      val rawRegex = ingredientRegex.findAllIn(raw)
      IngredientsRange(BigInt(rawRegex.group(1)), BigInt(rawRegex.group(2)))

  case class IngredientsRange(from: BigInt, to: BigInt):
    def isFresh(ingredient: Ingredient): Boolean =
      ingredient.identifier >= from && ingredient.identifier <= to

    /** range a 5 - 8 range b 3 - 6
      */
    def overlapsFrom(other: IngredientsRange): Boolean =
      other.from <= from && other.to >= from && other.to <= to

    /** range a 5 - 8 range b 7 - 10
      */
    def overlapsTo(other: IngredientsRange): Boolean =
      other.to >= to && other.from >= from && other.from <= to

    /** range a 1 - 8 range b 5 - 7
      */
    def overlapsFull(other: IngredientsRange): Boolean =
      other.from >= from && other.to <= to

    def widenFrom(other: IngredientsRange): IngredientsRange = copy(from = other.from)

    def widenTo(other: IngredientsRange): IngredientsRange = copy(to = other.to)

  object Ingredient:
    def apply(raw: String): Ingredient =
      Ingredient(BigInt(raw))

  case class Ingredient(identifier: BigInt)

  case class AccumulatingIngredientsRange(range: IngredientsRange, next: Option[AccumulatingIngredientsRange] = None):

    final def fitMeIn(newRange: IngredientsRange): AccumulatingIngredientsRange =
      if range.overlapsFrom(newRange) then copy(range = range.widenFrom(newRange))
      else if range.overlapsTo(newRange) then copy(range = range.widenTo(newRange))
      else if range.overlapsFull(newRange) then this
      else
        next match
          case None        => copy(next = Some(AccumulatingIngredientsRange(newRange)))
          case Some(value) => copy(next = Some(value.fitMeIn(newRange)))

    final def totalIngredients: BigInt =
      val thisTotal = range.to + 1 - range.from
      next match
        case None        => thisTotal
        case Some(value) => thisTotal + value.totalIngredients

  def day5A(): BigInt =
    val (ingredientRanges: Seq[IngredientsRange], ingredients: Seq[Ingredient]) = lines
      .flatMap(l => if l.contains("-") then Some(IngredientsRange(l)) else if l.nonEmpty then Some(Ingredient(l)) else None)
      .partition(_.isInstanceOf[IngredientsRange])

    ingredients.count(ingredient => ingredientRanges.exists(_.isFresh(ingredient)))

  def day5B(): BigInt =
    val (ingredientRanges: Seq[IngredientsRange], _) = lines
      .flatMap(l => if l.contains("-") then Some(IngredientsRange(l)) else if l.nonEmpty then Some(Ingredient(l)) else None)
      .partition(_.isInstanceOf[IngredientsRange])

    val sorted = ingredientRanges.sortBy(_.from)

    val accumulatingRange = sorted.tail
      .foldLeft(AccumulatingIngredientsRange(sorted.head))((acc, range) => acc.fitMeIn(range))

    accumulatingRange.totalIngredients

  @main
  def five(): Unit =
    println(s"5A: ${day5A()}")
    println(s"5B: ${day5B()}")
