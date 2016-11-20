
object StaticChecker {

  def check(program: Expression) : Boolean = {
    val factory = new Factory()
    val constraints = program.constraints(factory, IntType, factory.freshScope)
    val remainingConstraints = ConstraintResolver.resolve(factory, constraints)
    remainingConstraints.isEmpty
  }

  def check(program: Program) : Boolean = {
    val factory = new Factory()
    val constraints = program.constraints(factory)
    val remainingConstraints = ConstraintResolver.resolve(factory, constraints)
    remainingConstraints.isEmpty
  }
}
