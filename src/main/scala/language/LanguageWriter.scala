package language

import constraints.types.objects.{PrimitiveType, Type, TypeApplication}
import language.expressions.{BoolConst, Const, Expression, Variable}
import language.structs.{Access, Field, StructFieldInit}
import language.types.{LanguageType, LanguageTypeVariable}

trait LanguageWriter {

  implicit def const(value: Int) : Const = Const(value)
  implicit def bool(value: Boolean) : BoolConst = BoolConst(value)
  implicit def variable(name: String) : Variable = Variable(name)

  implicit def typeVariable(name: String) : LanguageTypeVariable = LanguageTypeVariable(name)

  implicit class LanguageName(name: String) {
    def is(value: Expression): StructFieldInit = StructFieldInit(name, value)
    def of( _type: LanguageType): Field = Field(name, _type)
    def access(field: String): Access = Access(Variable(name), field)
  }
}






























