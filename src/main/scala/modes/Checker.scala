package modes

import language.Program
import language.expressions.Expression
import language.types.{IntLanguageType, LanguageType}

trait Checker
{
  def check(program: Program): Boolean
  def checkExpression(expression: Expression, _type: LanguageType = IntLanguageType): Boolean
}
