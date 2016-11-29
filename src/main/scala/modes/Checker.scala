package modes

import language.Program
import language.expressions.Expression
import language.types.{IntType, LanguageType}

trait Checker
{
  def check(program: Program): Boolean
  def checkExpression(expression: Expression, _type: LanguageType = IntType): Boolean
}
