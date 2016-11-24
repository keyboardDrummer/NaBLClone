package constraints

import bindingTypeMachine.Machine
import constraints.types.objects.Type
import language.expressions.Expression
import language.Program
import language.types.{IntLanguageType, IntType, LanguageType}

import scala.util.Try

object StaticChecker {

  def both(program: Expression, languageType: LanguageType = IntLanguageType): Boolean = {
    val machineResult = Try(program.evaluate(Machine.expressionMachine)).map(_ => true).getOrElse(false)
    val constraintResult = check(program, languageType)
    if (machineResult != constraintResult)
      throw new IllegalStateException()
    machineResult
  }

  def check(program: Expression, languageType: LanguageType): Boolean = {
    val factory = new Factory()
    val builder: ConstraintBuilder = new ConstraintBuilder(factory)
    builder.add(Program.libraryConstraints)
    val _type = builder.typeVariable()
    val scope = factory.freshScope
    languageType.constraints(builder, _type, scope)
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

  def check(program: Program) : Boolean = {
    val factory = new Factory()
    val builder: ConstraintBuilder = new ConstraintBuilder(factory)
    program.constraints(builder)
    val constraints = builder.getConstraints
    new ConstraintSolver(factory, constraints).run()
  }
}
