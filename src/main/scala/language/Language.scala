package language

import constraints._
import constraints.scopeConstraints.{ConcreteScope, ParentScope, Scope, ScopeImport}

object Language {
  def getFunctionType(argument: Type, result: Type) = ConcreteType("Func", Seq(argument, result))
}

case class Program(modules: Seq[Module])
{
  def constraints(factory: Factory): Seq[Constraint] = {
    val scope = factory.freshScope
    modules.flatten(module => module.constraints(factory, scope))
  }
}

class Module(name: String, bindings: Seq[Binding], structs: Seq[Struct] = Seq.empty, imports: Seq[ModuleImport] = Seq.empty)
{
  def constraints(factory: Factory, parentScope: Scope): Seq[Constraint] = {
    val scope = factory.freshScope
    val moduleDeclaration = NamedDeclaration(name, this)
    Seq(DeclarationInsideScope(moduleDeclaration, parentScope), DeclarationOfScope(moduleDeclaration, scope), ParentScope(scope, parentScope)) ++
      structs.flatMap(struct => struct.constraints(factory, scope)) ++
      bindings.flatMap(binding => binding.constraints(factory, scope)) ++
      imports.flatMap(_import => _import.constraints(factory, scope))
  }
}

class Binding(name: String, _type: LanguageType, body: Expression)
{
  def constraints(factory: Factory, parentScope: Scope): Seq[Constraint] = {
    val typeVariable = factory.typeVariable
    val declaration = NamedDeclaration(name, this)
    Seq(DeclarationOfType(declaration, typeVariable), DeclarationInsideScope(declaration, parentScope)) ++
      body.constraints(factory, typeVariable, parentScope) ++
      _type.constraints(factory, typeVariable, parentScope)
  }
}

trait Expression {
  def constraints(factory: Factory, _type: Type, parentSocpe: Scope): Seq[Constraint]
}

class Field(val name: String, val _type: LanguageType)
class Struct(name: String, fields: Seq[Field])
{
  def constraints(factory: Factory, parentScope: Scope): Seq[Constraint] =
  {
    val scope = factory.freshScope
    val _type = factory.typeVariable
    val structDeclaration: NamedDeclaration = NamedDeclaration(name, this)
    val fieldConstraints: Seq[Constraint] = fields.flatMap(field => {
      val fieldDeclaration: NamedDeclaration = NamedDeclaration(field.name, field)
      Seq(DeclarationInsideScope(fieldDeclaration, scope), DeclarationOfType(fieldDeclaration, _type)) ++ field._type.constraints(factory, _type, scope)
    })
    Seq(ParentScope(scope, parentScope), DeclarationOfScope(structDeclaration, scope),
      DeclarationInsideScope(structDeclaration, parentScope)) ++ fieldConstraints
  }
}

class ModuleImport(name: String) {
  def constraints(factory: Factory, scope: Scope): Seq[Constraint] = {
    val importedDeclaration = factory.declarationVariable
    val importedScope = factory.scopeVariable
    val reference: Reference = Reference(name, this)
    Seq(ReferenceInScope(reference, scope), ResolvesTo(reference, importedDeclaration), DeclarationOfScope(importedDeclaration, importedScope),
      ScopeImport(scope, importedScope))
  }
}

trait LanguageType
{
  def constraints(factory: Factory, _type: Type, scope: Scope): Seq[Constraint]
}

object IntLanguageType extends LanguageType {
  override def constraints(factory: Factory, _type: Type, scope: Scope): Seq[Constraint] = Seq(TypesAreEqual(_type, IntType))
}

class LanguageStructType(name: String) extends LanguageType {
  override def constraints(factory: Factory, _type: Type, scope: Scope): Seq[Constraint] = {
    val structDeclaration = factory.declarationVariable
    val reference: Reference = Reference(name, this)
    Seq(TypesAreEqual(_type, StructType(structDeclaration)),
      ResolvesTo(reference, structDeclaration),
      ReferenceInScope(reference, scope))
  }
}

object IntType extends ConcreteType("Int", Seq.empty)

case class FunctionLanguageType(argument: LanguageType, result: LanguageType) extends LanguageType {
  override def constraints(factory: Factory, _type: Type, scope: Scope): Seq[Constraint] = {
    val inputType = factory.typeVariable
    val outputType = factory.typeVariable
    Seq(TypesAreEqual(_type, Language.getFunctionType(inputType, outputType))) ++
      argument.constraints(factory, inputType, scope) ++
      result.constraints(factory, outputType, scope)
  }
}

class Let(name: String, bindingValue: Expression, value: Expression) extends Expression {
  override def constraints(factory: Factory, _type: Type, parentScope: Scope): Seq[Constraint] = {
    val scope = factory.freshScope
    val bindingType = factory.typeVariable
    val declaration = NamedDeclaration(name, this)
    Seq(DeclarationOfType(declaration, bindingType), DeclarationInsideScope(declaration, scope), ParentScope(scope, parentScope)) ++
      bindingValue.constraints(factory, bindingType, parentScope) ++
      value.constraints(factory, _type, scope)
  }
}

class Lambda(name: String, body: Expression, argumentType: Option[LanguageType] = None) extends Expression {
  override def constraints(factory: Factory, _type: Type, scope: Scope): Seq[Constraint] = {
    val bodyScope: ConcreteScope = factory.freshScope
    val argumentDeclaration: NamedDeclaration = NamedDeclaration(name, this)
    val bodyType = factory.typeVariable
    val argumentConstraintType = factory.typeVariable
    Seq(ParentScope(bodyScope, scope),
      DeclarationInsideScope(argumentDeclaration, bodyScope),
      DeclarationOfType(argumentDeclaration, argumentConstraintType),
      TypesAreEqual(_type, Language.getFunctionType(argumentConstraintType, bodyType))) ++
        body.constraints(factory, bodyType, bodyScope) ++
        argumentType.fold(Seq.empty[Constraint])(at => at.constraints(factory, argumentConstraintType, scope))
  }
}

class Variable(name: String) extends Expression {
  override def constraints(factory: Factory, _type: Type, scope: Scope): Seq[Constraint] = {
    val declaration: DeclarationVariable = factory.declarationVariable
    val reference: Reference = Reference(name, this)
    val declarationType = factory.typeVariable
    Seq(ReferenceInScope(reference, scope), ResolvesTo(reference, declaration), DeclarationOfType(declaration, declarationType),
      Specialization(_type, declarationType))
  }
}

case class Application(function: Expression, value: Expression) extends Expression {
  override def constraints(factory: Factory, _type: Type, scope: Scope): Seq[Constraint] = {
    val functionType = factory.typeVariable
    val argumentType = factory.typeVariable
    Seq(TypesAreEqual(functionType, Language.getFunctionType(argumentType, _type))) ++
      function.constraints(factory, functionType, scope) ++
      value.constraints(factory, argumentType, scope)
  }
}

case class Const(value: Int) extends Expression {
  override def constraints(factory: Factory, _type: Type, scope: Scope): Seq[Constraint] = Seq(TypesAreEqual(_type, IntType))
}

case class Add(left: Expression, right: Expression) extends Expression {
  override def constraints(factory: Factory, _type: Type, scope: Scope): Seq[Constraint] = Seq(TypesAreEqual(_type, IntType)) ++
    left.constraints(factory, IntType, scope) ++
    right.constraints(factory, IntType, scope)
}

class Access(target: Expression, field: String) extends Expression
{
  override def constraints(factory: Factory, _type: Type, scope: Scope): Seq[Constraint] = {
    val structDeclaration = factory.declarationVariable
    val fieldDeclaration = factory.declarationVariable
    val structScope = factory.scopeVariable
    val fieldReference: Reference = Reference(field, this)
    val targetType = factory.typeVariable
    Seq(DeclarationOfType(fieldDeclaration, _type), TypesAreEqual(StructType(structDeclaration), targetType),
      DeclarationOfScope(structDeclaration, structScope),
      ResolvesTo(fieldReference, fieldDeclaration), ReferenceInScope(fieldReference, structScope)) ++
      target.constraints(factory, targetType, scope)
  }
}

class StructFieldInit(fieldName: String, value: Expression) {
  def constraints(factory: Factory, structScope: Scope, parentScope: Scope): Seq[Constraint] = {
    val reference = Reference(fieldName, this)
    val fieldDeclaration = factory.declarationVariable
    val fieldType = factory.typeVariable
    Seq(ReferenceInScope(reference, structScope), ResolvesTo(reference, fieldDeclaration),
      DeclarationOfType(fieldDeclaration, fieldType)) ++ value.constraints(factory, fieldType, parentScope)
  }
}

class New(structName: String, values: Seq[StructFieldInit]) extends Expression
{
  override def constraints(factory: Factory, _type: Type, scope: Scope): Seq[Constraint] = {
    val structDeclaration = factory.declarationVariable
    val reference: Reference = Reference(structName, this)
    val structScope = factory.scopeVariable
    Seq(ReferenceInScope(reference, scope), ResolvesTo(reference, structDeclaration),
      TypesAreEqual(_type, StructType(structDeclaration)),
      DeclarationOfScope(structDeclaration, structScope)) ++
        values.flatMap(value => value.constraints(factory, structScope, scope))
  }
}