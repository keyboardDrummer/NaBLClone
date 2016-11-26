package constraints

import bindingTypeMachine.{Machine, MachineType}
import constraints.types.objects.{IntType, Type}
import language.expressions.Expression
import language.Program
import language.types.{IntLanguageType, LanguageType}

import scala.util.Try

object StaticChecker {

  def both(program: Expression, languageType: LanguageType = IntLanguageType): Boolean = {
    val machineResult: Boolean = checkMachine(program, languageType)
    val constraintResult = check(program, languageType)
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

  def check(program: Expression, languageType: LanguageType): Boolean = {
    val factory = new Factory()
    val builder: ConstraintBuilder = new ConstraintBuilder(factory)
    builder.add(Program.libraryConstraints)
    val scope = factory.freshScope
    val _type = languageType.constraints(builder, scope)
    program.constraints(builder, _type, scope)
    val constraints = builder.getConstraints
    new ConstraintSolver(factory, constraints).run()
  }

  def check(program: Expression, _type: Type = IntType) : Boolean = {
    val factory = new Factory()
    val builder: ConstraintBuilder = new ConstraintBuilder(factory)
    builder.add(Program.libraryConstraints)
    program.constraints(builder, _type, factory.freshScope)
    val constraints = builder.getConstraints
    new ConstraintSolver(factory, constraints).run()
  }

  def both(program: Program): Boolean = {
    val machineResult: Boolean = checkMachine(program)
    val constraintResult = check(program)
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

  def check(program: Program) : Boolean = {
    val factory = new Factory()
    val builder: ConstraintBuilder = new ConstraintBuilder(factory)
    program.constraints(builder)
    val constraints = builder.getConstraints
    new ConstraintSolver(factory, constraints).run()
  }
}
