package constraints

import language.{Expression, IntType, Program}

object StaticChecker {

  def check(program: Expression) : Boolean = {
    val factory = new Factory()
    val constraints = program.constraints(factory, IntType, factory.freshScope)
    new ConstraintSolver(factory, constraints).run()
  }

  def check(program: Program) : Boolean = {
    val factory = new Factory()
    val constraints = program.constraints(factory)
    new ConstraintSolver(factory, constraints).run()
  }
}
