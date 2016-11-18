
trait Language{
  def getFunctionType(argument: Type, result: Type) = ConcreteType("Func", Seq(argument, result))

}
trait Expression extends Language {
  def constraints(factory: Factory, _type: Type, scope: Scope): Seq[Constraint]
}

class Factory
{
  var scopeCounter: Int = 0
  def freshScope: ConcreteScope = {
    scopeCounter += 1
    ConcreteScope(scopeCounter)
  }

  var typeCounter = 0
  def typeVariable : TypeVariable = {
    typeCounter += 1
    TypeVariable(typeCounter.toString)
  }

  var declarationCounter = 0
  def declarationVariable: DeclarationVariable = {
    declarationCounter += 1
    DeclarationVariable(declarationCounter.toString)
  }
}

trait LanguageType extends Language
{
  def constraints(factory: Factory, _type: Type, scope: Scope): Seq[Constraint]
}

object IntLanguageType extends LanguageType {
  override def constraints(factory: Factory, _type: Type, scope: Scope): Seq[Constraint] = Seq(TypesAreEqual(_type, IntType))
}

object IntType extends ConcreteType("Int", Seq.empty)

case class FunctionLanguageType(argument: LanguageType, result: LanguageType) extends LanguageType {
  override def constraints(factory: Factory, _type: Type, scope: Scope): Seq[Constraint] = {
    val inputType = factory.typeVariable
    val outputType = factory.typeVariable
    Seq(TypesAreEqual(_type, getFunctionType(inputType, outputType))) ++
      argument.constraints(factory, inputType, scope) ++
      result.constraints(factory, outputType, scope)
  }
}


case class Lambda(name: String, argumentType: LanguageType, body: Expression) extends Expression {
  override def constraints(factory: Factory, _type: Type, scope: Scope): Seq[Constraint] = {
    val bodyScope: ConcreteScope = factory.freshScope
    val argumentDeclaration: NamedDeclaration = NamedDeclaration(name)
    val bodyType = factory.typeVariable
    val argumentConstraintType = factory.typeVariable
    Seq(ParentScope(bodyScope, scope),
      DeclarationInScope(argumentDeclaration, bodyScope),
      DeclarationOfType(argumentDeclaration, argumentConstraintType),
      TypesAreEqual(_type, getFunctionType(argumentConstraintType, bodyType))) ++
        body.constraints(factory, bodyType, bodyScope) ++
        argumentType.constraints(factory, argumentConstraintType, scope)
  }
}

case class Variable(name: String) extends Expression {
  override def constraints(factory: Factory, _type: Type, scope: Scope): Seq[Constraint] = {
    val declaration: DeclarationVariable = factory.declarationVariable
    val reference: Reference = Reference(name)
    Seq(ReferenceInScope(reference, scope), ResolvesTo(reference, declaration), DeclarationOfType(declaration, _type))
  }
}

case class Application(function: Expression, value: Expression) extends Expression {
  override def constraints(factory: Factory, _type: Type, scope: Scope): Seq[Constraint] = {
    val functionType = factory.typeVariable
    val argumentType = factory.typeVariable
    Seq(TypesAreEqual(functionType, getFunctionType(argumentType, _type))) ++
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
