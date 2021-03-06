package modes

import constraints.{ConstraintBuilder, ConstraintSolver, Factory}
import language.Program
import language.expressions.Expression
import language.types.LanguageType

trait ConstraintChecker extends Checker
{
  def subTyping: Boolean

  override def check(program: Program): Boolean = {
    val factory = new Factory()
    val builder: ConstraintBuilder = new ConstraintBuilder(factory, this)
    program.constraints(builder)
    val constraints = builder.getConstraints
    new ConstraintSolver(builder, constraints).run()
  }

  override def checkExpression(expression: Expression, languageType: LanguageType): Boolean = {
    val factory = new Factory()
    val builder: ConstraintBuilder = new ConstraintBuilder(factory, this)
    builder.add(Program.libraryConstraints)
    val scope = factory.newScope
    val _type = languageType.constraints(builder, scope)
    expression.constraints(builder, _type, scope)
    val constraints = builder.getConstraints
    new ConstraintSolver(builder, constraints).run()
  }
}
