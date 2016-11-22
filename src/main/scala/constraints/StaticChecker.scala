package constraints

import constraints.types.objects.Type
import language.expressions.Expression
import language.Program
import language.types.IntType

object StaticChecker {

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
