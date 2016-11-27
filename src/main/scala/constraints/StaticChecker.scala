package constraints

import bindingTypeMachine.{Machine, MachineType}
import constraints.types.objects.{IntType, Type}
import language.expressions.Expression
import language.Program
import language.types.{IntLanguageType, LanguageType}

import scala.util.Try

object StaticChecker {

  def bothExpression(program: Expression, languageType: LanguageType = IntLanguageType, mode: Mode = AbstractMachine): Boolean = {
    val machineResult: Boolean = checkMachine(program, languageType)
    val constraintResult = checkExpression(program, languageType, mode)
    if (machineResult != constraintResult)
      throw new IllegalStateException(s"machine says $machineResult while constraints says $constraintResult")
    machineResult
  }

  def checkMachine(program: Expression, languageType: LanguageType = IntLanguageType): Boolean = {
    val expressionMachine: Machine = Machine.expressionMachine
    try
    {
      val machineType = program.evaluate(expressionMachine)
      val expectedMachineType: MachineType = languageType.evaluate(expressionMachine)
      expectedMachineType == machineType
    } catch
    {
      case e: Throwable =>
        val f = e
        false
    }
  }

  def checkExpression(program: Expression, languageType: LanguageType = IntLanguageType, mode: Mode = AbstractMachine): Boolean = {
    val factory = new Factory()
    val builder: ConstraintBuilder = new ConstraintBuilder(factory, mode)
    builder.add(Program.libraryConstraints)
    val scope = factory.freshScope
    val _type = languageType.constraints(builder, scope)
    program.constraints(builder, _type, scope)
    val constraints = builder.getConstraints
    new ConstraintSolver(factory, constraints).run()
  }

  def both(program: Program, mode: Mode = AbstractMachine): Boolean = {
    val machineResult: Boolean = checkMachine(program)
    val constraintResult = check(program, mode)
    if (machineResult != constraintResult)
      throw new IllegalStateException(s"machine says $machineResult while constraints says $constraintResult")
    machineResult
  }

  def checkMachine(program: Program): Boolean = {
    val machine: Machine = new Machine()
    try
    {
      program.evaluate(machine)
      true
    } catch
      {
        case e: Throwable =>
          val f = e
          Console.out.append("")
          false
      }
  }

  def check(program: Program, mode: Mode = AbstractMachine) : Boolean = {
    val factory = new Factory()
    val builder: ConstraintBuilder = new ConstraintBuilder(factory, mode)
    program.constraints(builder)
    val constraints = builder.getConstraints
    new ConstraintSolver(factory, constraints).run()
  }
}
