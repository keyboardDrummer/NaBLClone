import language.Program
import language.expressions.Expression
import language.types.{IntType, LanguageType}
import modes._

object Checker {

  def failExpression(program: Expression, languageType: LanguageType = IntType, skip: Set[Checker] = Set.empty): Unit =
  {
    checkExpression(program, languageType, allCheckersSet.diff(skip))
  }

  def checkExpression(program: Expression, languageType: LanguageType = IntType, skip: Set[Checker] = Set.empty): Unit = {
    val answers = allCheckers.map(mode => (mode, mode.checkExpression(program, languageType))).toMap
    processAnswers(answers, allCheckersSet.diff(skip))
  }

  def fail(program: Program, skip: Set[Checker] = Set.empty): Unit = check(program, skip = allCheckersSet.diff(skip))

  def check(program: Program, skip: Set[Checker] = Set.empty) : Unit = {
    val answers = allCheckers.map(mode => (mode, mode.check(program))).toMap
    processAnswers(answers, allCheckersSet.diff(skip))
  }

  val allCheckers: Seq[Checker] = Seq(MachineChecker, ConstraintClosure,
    ConstraintHindleyMilner(true), ConstraintHindleyMilner(false), SimpleConstraintChecker)
  val allCheckersSet: Set[Checker] = allCheckers.toSet

  val noSubTypingModes: Set[Checker] = Set(ConstraintHindleyMilner(false), SimpleConstraintChecker)
  val threeMusketiers: Set[Checker] = ConstraintHindleyMilner.both ++ Set(SimpleConstraintChecker)

  def processAnswers(answers: Map[Checker, Boolean], successModes: Set[Checker]): Unit = {
    val badAnswers = answers.filter(answer => successModes.contains(answer._1) ^ answer._2)
    assert(badAnswers.isEmpty, s"the following answers are bad: $badAnswers")
  }
}