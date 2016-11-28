import language.Program
import language.expressions.Expression
import language.types.{IntLanguageType, LanguageType}
import modes._

object Checker {

  def failExpression(program: Expression, languageType: LanguageType = IntLanguageType, checkers: Set[Checker] = allModes): Unit =
  {
    checkExpression(program, languageType, checkers = allModes.diff(checkers))
  }

  def checkExpression(program: Expression, languageType: LanguageType = IntLanguageType, checkers: Set[Checker] = allModes): Unit = {
    val answers = allModes.map(mode => (mode, mode.checkExpression(program, languageType))).toMap
    processAnswers(answers, checkers)
  }
  
  def processAnswers(answers: Map[Checker, Boolean], successModes: Set[Checker]): Unit = {
    val badAnswers = answers.filter(answer => successModes.contains(answer._1) ^ answer._2)
    assert(badAnswers.isEmpty, s"the following answers are bad: $badAnswers")
  }
  
  def fail(program: Program, checkers: Set[Checker] = allModes): Unit = check(program, checkers = allModes.diff(checkers))

  def check(program: Program, checkers: Set[Checker] = allModes) : Unit = {
    val answers = checkers.map(mode => (mode, mode.check(program))).toMap
    processAnswers(answers, checkers)
  }

  val allModes: Set[Checker] = Set(MachineChecker, ConstraintClosure, ConstraintHindleyMilner)
}