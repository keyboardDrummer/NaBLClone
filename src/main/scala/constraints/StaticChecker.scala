package constraints

import constraints.types.objects.Type
import language.expressions.Expression
import language.Program
import language.types.IntType

object StaticChecker {

  def check(program: Expression, _type: Type = IntType) : Boolean = {
    val factory = new Factory()
    val constraints = Program.libraryConstraints ++ program.constraints(new ConstraintBuilder(factory), _type, factory.freshScope)
    new ConstraintSolver(factory, constraints).run()
  }

  def check(program: Program) : Boolean = {
    val factory = new Factory()
    val constraints = program.constraints(new ConstraintBuilder(factory))
    new ConstraintSolver(factory, constraints).run()
  }
}
