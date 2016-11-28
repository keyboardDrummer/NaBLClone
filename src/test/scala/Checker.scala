import language.Program
import language.expressions.Expression
import language.types.{IntLanguageType, LanguageType}
import modes._

object Checker {

  def failExpression(program: Expression, languageType: LanguageType = IntLanguageType, skip: Set[Checker] = Set.empty): Unit =
  {
    checkExpression(program, languageType, allModes.diff(skip))
  }

  def checkExpression(program: Expression, languageType: LanguageType = IntLanguageType, skip: Set[Checker] = Set.empty): Unit = {
    val answers = allModes.map(mode => (mode, mode.checkExpression(program, languageType))).toMap
    processAnswers(answers, allModes.diff(skip))
  }
  
  def fail(program: Program, skip: Set[Checker] = Set.empty): Unit = check(program, skip = allModes.diff(skip))

  def check(program: Program, skip: Set[Checker] = Set.empty) : Unit = {
    val answers = allModes.map(mode => (mode, mode.check(program))).toMap
    processAnswers(answers, allModes.diff(skip))
  }

  val allModes: Set[Checker] = Set(MachineChecker, ConstraintClosure, ConstraintHindleyMilner)

  def processAnswers(answers: Map[Checker, Boolean], successModes: Set[Checker]): Unit = {
    val badAnswers = answers.filter(answer => successModes.contains(answer._1) ^ answer._2)
    assert(badAnswers.isEmpty, s"the following answers are bad: $badAnswers")
  }
}