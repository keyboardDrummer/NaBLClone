package language

import constraints._
import constraints.objects.{DeclarationVariable, NamedDeclaration, Reference}
import constraints.scopes._
import constraints.scopes.imports.{DeclarationOfScope, ScopeImport}
import constraints.scopes.objects.{ConcreteScope, Scope}
import constraints.types.objects.{ConcreteType, StructType, Type}
import constraints.types.{DeclarationOfType, Specialization, TypesAreEqual}

object Language {
  def getFunctionType(argument: Type, result: Type) = ConcreteType("Func", Seq(argument, result))
}

case class Program(modules: Seq[Module])
{
  def constraints(builder: ConstraintBuilder): Seq[Constraint] = {
    val scope = builder.newScope()
    modules.flatten(module => module.constraints(builder, scope))
  }
}

class Module(name: String, bindings: Seq[Binding], structs: Seq[Struct] = Seq.empty, imports: Seq[ModuleImport] = Seq.empty)
{
  def constraints(builder: ConstraintBuilder, parentScope: Scope): Seq[Constraint] = {
    val scope = builder.newScope()
    val moduleDeclaration = NamedDeclaration(name, this)
    Seq(DeclarationInsideScope(moduleDeclaration, parentScope), DeclarationOfScope(moduleDeclaration, scope), ParentScope(scope, parentScope)) ++
      structs.flatMap(struct => struct.constraints(builder, scope)) ++
      bindings.flatMap(binding => binding.constraints(builder, scope)) ++
      imports.flatMap(_import => _import.constraints(builder, scope))
  }
}

class Binding(name: String, _type: LanguageType, body: Expression)
{
  def constraints(builder: ConstraintBuilder, parentScope: Scope): Seq[Constraint] = {
    val typeVariable = builder.typeVariable()
    val declaration = NamedDeclaration(name, this)
    Seq(DeclarationOfType(declaration, typeVariable), DeclarationInsideScope(declaration, parentScope)) ++
      body.constraints(builder, typeVariable, parentScope) ++
      _type.constraints(builder, typeVariable, parentScope)
  }
}

trait Expression {
  def constraints(builder: ConstraintBuilder, _type: Type, parentSocpe: Scope): Seq[Constraint]
}

class Field(val name: String, val _type: LanguageType)
class Struct(name: String, fields: Seq[Field])
{
  def constraints(builder: ConstraintBuilder, parentScope: Scope): Seq[Constraint] =
  {
    val scope = builder.newScope()
    val _type = builder.typeVariable()
    val structDeclaration: NamedDeclaration = NamedDeclaration(name, this)
    val fieldConstraints: Seq[Constraint] = fields.flatMap(field => {
      val fieldDeclaration: NamedDeclaration = NamedDeclaration(field.name, field)
      Seq(DeclarationInsideScope(fieldDeclaration, scope), DeclarationOfType(fieldDeclaration, _type)) ++ field._type.constraints(builder, _type, scope)
    })
    Seq(ParentScope(scope, parentScope), DeclarationOfScope(structDeclaration, scope),
      DeclarationInsideScope(structDeclaration, parentScope)) ++ fieldConstraints
  }
}

class ModuleImport(name: String) {
  def constraints(builder: ConstraintBuilder, scope: Scope): Seq[Constraint] = {
    val importedDeclaration = builder.declarationVariable()
    val importedScope = builder.declaredScopeVariable(importedDeclaration)
    val reference: Reference = Reference(name, this)
    Seq(ReferenceInScope(reference, scope), ResolvesTo(reference, importedDeclaration)) ++ builder.getConstraints ++  Seq(ScopeImport(scope, importedScope))
  }
}

trait LanguageType
{
  def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint]
}

object IntLanguageType extends LanguageType {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = Seq(TypesAreEqual(_type, IntType))
}

class LanguageStructType(name: String) extends LanguageType {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = {
    val structDeclaration = builder.declarationVariable()
    val reference: Reference = Reference(name, this)
    Seq(TypesAreEqual(_type, StructType(structDeclaration)),
      ResolvesTo(reference, structDeclaration),
      ReferenceInScope(reference, scope))
  }
}

object IntType extends ConcreteType("Int", Seq.empty)

case class FunctionLanguageType(argument: LanguageType, result: LanguageType) extends LanguageType {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = {
    val inputType = builder.typeVariable()
    val outputType = builder.typeVariable()
    Seq(TypesAreEqual(_type, Language.getFunctionType(inputType, outputType))) ++
      argument.constraints(builder, inputType, scope) ++
      result.constraints(builder, outputType, scope)
  }
}

class Let(name: String, bindingValue: Expression, value: Expression) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Seq[Constraint] = {
    val scope = builder.newScope()
    val bindingType = builder.typeVariable()
    val declaration = NamedDeclaration(name, this)
    Seq(DeclarationOfType(declaration, bindingType), DeclarationInsideScope(declaration, scope), ParentScope(scope, parentScope)) ++
      bindingValue.constraints(builder, bindingType, parentScope) ++
      value.constraints(builder, _type, scope)
  }
}

class Lambda(name: String, body: Expression, argumentType: Option[LanguageType] = None) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = {
    val bodyScope: ConcreteScope = builder.newScope(Some(scope))
    val argumentConstraintType = builder.typeVariable()
    builder.declaration(name, this, argumentConstraintType, bodyScope)
    val bodyType = builder.typeVariable()
    builder.getConstraints ++ Seq(TypesAreEqual(_type, Language.getFunctionType(argumentConstraintType, bodyType))) ++
        body.constraints(builder, bodyType, bodyScope) ++
        argumentType.fold(Seq.empty[Constraint])(at => at.constraints(builder, argumentConstraintType, scope))
  }
}

class Variable(name: String) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = {
    val declaration: DeclarationVariable = builder.declarationVariable()
    val reference: Reference = Reference(name, this)
    val declarationType = builder.typeVariable()
    Seq(DeclarationOfType(declaration, declarationType), ReferenceInScope(reference, scope), ResolvesTo(reference, declaration),
      Specialization(_type, declarationType))
  }
}

case class Application(function: Expression, value: Expression) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = {
    val functionType = builder.typeVariable()
    val argumentType = builder.typeVariable()
    Seq(TypesAreEqual(functionType, Language.getFunctionType(argumentType, _type))) ++
      function.constraints(builder, functionType, scope) ++
      value.constraints(builder, argumentType, scope)
  }
}

case class Const(value: Int) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = Seq(TypesAreEqual(_type, IntType))
}

case class Add(left: Expression, right: Expression) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = Seq(TypesAreEqual(_type, IntType)) ++
    left.constraints(builder, IntType, scope) ++
    right.constraints(builder, IntType, scope)
}

class Access(target: Expression, field: String) extends Expression
{
  /* We don't need a scope import because we can directly use the struct scope to resolve the member.
   */
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = {
    val structDeclaration = builder.declarationVariable()
    val fieldDeclaration = builder.declarationVariable(_type)
    val structScope = builder.scopeVariable()
    builder.reference(field, this, structScope, fieldDeclaration)
    val targetType = builder.typeVariable()
    builder.getConstraints ++ Seq(TypesAreEqual(StructType(structDeclaration), targetType),
      DeclarationOfScope(structDeclaration, structScope)) ++
      target.constraints(builder, targetType, scope)
  }
}

class StructFieldInit(fieldName: String, value: Expression) {
  def constraints(builder: ConstraintBuilder, structScope: Scope, parentScope: Scope): Seq[Constraint] = {
    val reference = Reference(fieldName, this)
    val fieldDeclaration = builder.declarationVariable()
    val fieldType = builder.typeVariable()
    Seq(ReferenceInScope(reference, structScope), ResolvesTo(reference, fieldDeclaration),
      DeclarationOfType(fieldDeclaration, fieldType)) ++ value.constraints(builder, fieldType, parentScope)
  }
}

class New(structName: String, values: Seq[StructFieldInit]) extends Expression
{
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Seq[Constraint] = {
    val structDeclaration = builder.declarationVariable()
    val reference: Reference = Reference(structName, this)
    val structScope = builder.scopeVariable()
    Seq(ReferenceInScope(reference, scope), ResolvesTo(reference, structDeclaration),
      TypesAreEqual(_type, StructType(structDeclaration)),
      DeclarationOfScope(structDeclaration, structScope)) ++
        values.flatMap(value => value.constraints(builder, structScope, scope))
  }
}