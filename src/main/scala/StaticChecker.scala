
object StaticChecker {

  def check(program: Expression) : Boolean = {
    val factory = new Factory()
    val constraints = program.constraints(factory, IntType, factory.freshScope)
    val remainingConstraints = ConstraintResolver.resolve(constraints)
    remainingConstraints.isEmpty
  }
}
