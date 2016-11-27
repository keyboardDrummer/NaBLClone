package constraints

import bindingTypeMachine.{Machine, MachineType}
import language.Program
import language.expressions.Expression
import language.types.{IntLanguageType, LanguageType}

object StaticChecker {
  def checkMachine(program: Program) = check(program, Set(MachineMode)) //TODO inline
  def checkMachine(program: Expression) = checkExpression(program, modes = Set(MachineMode)) //TODO inline


  def bothExpression(program: Expression, languageType: LanguageType = IntLanguageType, mode: ConstraintMode = ConstraintClosure): Boolean = {
    checkExpression(program, languageType)
  }

  def checkExpression(program: Expression, languageType: LanguageType = IntLanguageType, modes: Set[Mode] = allModes): Boolean = {
    val answers = modes.map(mode => (mode, mode.checkExpression(program, languageType))).toMap
    if (answers.values.toSet.size > 1)
    {
      throw new IllegalStateException(s"modes give conflicting answers: $answers")
    }
    answers.values.head
  }

  def both(program: Program, mode: ConstraintMode = ConstraintClosure): Boolean = {
    check(program)
  }

  def check(program: Program, modes: Set[Mode] = allModes) : Boolean = {
    val answers = modes.map(mode => (mode, mode.check(program))).toMap
    if (answers.values.toSet.size > 1)
    {
      throw new IllegalStateException(s"modes give conflicting answers: $answers")
    }
    answers.values.head
  }

  val allModes: Set[Mode] = Set(MachineMode, ConstraintHindleyMilner, ConstraintClosure)
}

trait Mode
{
  def check(program: Program): Boolean
  def checkExpression(expression: Expression, _type: LanguageType = IntLanguageType): Boolean
}

object MachineMode extends Mode
{
  override def check(program: Program): Boolean =  {
    val machine: Machine = new Machine()
    try
    {
      program.evaluate(machine)
      true
    }
    catch
    {
      case e: Throwable =>
        val f = e
        Console.out.append("")
        false
    }
  }

  override def checkExpression(expression: Expression, _type: LanguageType): Boolean = {
    val expressionMachine: Machine = Machine.expressionMachine
    try
    {
      val machineType = expression.evaluate(expressionMachine)
      val expectedMachineType: MachineType = _type.evaluate(expressionMachine)
      expectedMachineType == machineType
    }
    catch
    {
      case e: Throwable =>
        val f = e
        false
    }
  }
}

trait ConstraintMode extends Mode
{
  override def check(program: Program): Boolean = {
    val factory = new Factory()
    val builder: ConstraintBuilder = new ConstraintBuilder(factory, this)
    program.constraints(builder)
    val constraints = builder.getConstraints
    new ConstraintSolver(factory, constraints).run()
  }

  override def checkExpression(expression: Expression, languageType: LanguageType): Boolean = {
    val factory = new Factory()
    val builder: ConstraintBuilder = new ConstraintBuilder(factory, this)
    builder.add(Program.libraryConstraints)
    val scope = factory.freshScope
    val _type = languageType.constraints(builder, scope)
    expression.constraints(builder, _type, scope)
    val constraints = builder.getConstraints
    new ConstraintSolver(factory, constraints).run()
  }
}

object ConstraintHindleyMilner extends ConstraintMode
object ConstraintClosure extends ConstraintMode