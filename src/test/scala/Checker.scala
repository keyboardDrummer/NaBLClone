import language.Program
import language.expressions.Expression
import language.types.{IntLanguageType, LanguageType}
import modes._

object Checker {

  def failExpression(program: Expression, languageType: LanguageType = IntLanguageType): Unit =
  {
    checkExpression(program, languageType, modes = Set.empty)
  }
  
  def checkExpression(program: Expression, languageType: LanguageType = IntLanguageType, modes: Set[Checker] = allModes): Unit = {
    val answers = allModes.map(mode => (mode, mode.checkExpression(program, languageType))).toMap
    processAnswers(answers, modes)
  }
  
  def processAnswers(answers: Map[Checker, Boolean], successModes: Set[Checker]): Unit = {
    val badAnswers = answers.filter(answer => successModes.contains(answer._1) ^ answer._2)
    assert(badAnswers.isEmpty, s"the following answers are bad: $badAnswers")
  }
  
  def fail(program: Program): Unit = check(program, Set.empty)

  def check(program: Program, modes: Set[Checker] = allModes) : Unit = {
    val answers = modes.map(mode => (mode, mode.check(program))).toMap
    processAnswers(answers, modes)
  }

  val allModes: Set[Checker] = Set(MachineChecker, ConstraintClosure, ConstraintHindleyMilner)
}