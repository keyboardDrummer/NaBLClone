package language

import constraints._
import constraints.objects.{DeclarationVariable, NamedDeclaration, Reference}
import constraints.scopes._
import constraints.scopes.imports.{DeclarationOfScope, ScopeImport}
import constraints.scopes.objects.{ConcreteScope, Scope}
import constraints.types.objects.{PrimitiveType, StructType, Type, TypeApplication}
import constraints.types.{DeclarationOfType, Specialization, TypesAreEqual}
import language.expressions.Expression
import language.modules.Module

object Language {
  def getFunctionType(argument: Type, result: Type) = TypeApplication(PrimitiveType("Func"), Seq(argument, result))
}






























