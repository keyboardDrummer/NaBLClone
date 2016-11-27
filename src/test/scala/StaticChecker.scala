import language.Program
import language.expressions.Expression
import language.types.{IntLanguageType, LanguageType}
import modes._

object StaticChecker {
  def checkMachine(program: Program) = check(program, Set(MachineMode)) //TODO inline
  def checkMachine(program: Expression) = checkExpression(program, success = Set(MachineMode)) //TODO inline

  def failExpression(program: Expression, languageType: LanguageType = IntLanguageType): Unit =
  {
    checkExpression(program, languageType, success = Set.empty)
  }
  
  def checkExpression(program: Expression, languageType: LanguageType = IntLanguageType, success: Set[Mode] = allModes): Unit = {
    val answers = allModes.map(mode => (mode, mode.checkExpression(program, languageType))).toMap
    processAnswers(answers, success)
  }
  
  def processAnswers(answers: Map[Mode, Boolean], successModes: Set[Mode]): Unit = {
    val badAnswers = answers.filter(answer => successModes.contains(answer._1) ^ answer._2)
    assert(badAnswers.isEmpty, s"the following answers are bad: $badAnswers")
  }

  def both(program: Program, mode: ConstraintMode = ConstraintClosure) : Unit = { //TODO INLINE
    check(program)
  }
  
  def fail(program: Program): Unit = check(program, Set.empty)

  def check(program: Program, modes: Set[Mode] = allModes) : Unit = {
    val answers = modes.map(mode => (mode, mode.check(program))).toMap
    processAnswers(answers, modes)
  }

  val allModes: Set[Mode] = Set(MachineMode, ConstraintHindleyMilner, ConstraintClosure)
}