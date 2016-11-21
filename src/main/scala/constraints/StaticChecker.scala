package constraints

import language.expressions.Expression
import language.Program
import language.types.IntType

object StaticChecker {

  def check(program: Expression) : Boolean = {
    val factory = new Factory()
    val constraints = program.constraints(new ConstraintBuilder(factory), IntType, factory.freshScope)
    new ConstraintSolver(factory, constraints).run()
  }

  def check(program: Program) : Boolean = {
    val factory = new Factory()
    val constraints = program.constraints(new ConstraintBuilder(factory))
    new ConstraintSolver(factory, constraints).run()
  }
}
