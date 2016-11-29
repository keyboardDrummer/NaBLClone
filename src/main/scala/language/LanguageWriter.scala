package language

import constraints.types.objects.{PrimitiveType, Type, TypeApplication}
import language.expressions._
import language.structs.{Access, Field, StructFieldInit}
import language.types.{LanguageType, LanguageTypeVariable}

trait LanguageWriter {

  implicit def const(value: Int) : Const = Const(value)
  implicit def bool(value: Boolean) : BoolConst = BoolConst(value)
  implicit def variable(name: String) : Variable = Variable(name)

  implicit def typeVariable(name: String) : LanguageTypeVariable = LanguageTypeVariable(name)

  class ExpressionWriter(expression: Expression) {
    def $(argument: Expression) = Application(expression, argument)
    def add(right: Expression) = Add(expression, right)
    def access(field: String): Access = Access(expression, field)
  }

  implicit class ExtendedExpression(expression: Expression) extends ExpressionWriter(expression) {
  }

  implicit class LanguageInt(value: Int) extends ExpressionWriter(Const(value))

  implicit class LanguageName(name: String) extends ExpressionWriter(Variable(name)) {
    def is(value: Expression): StructFieldInit = StructFieldInit(name, value)
    def of( _type: LanguageType): Field = Field(name, _type)
  }
}






























