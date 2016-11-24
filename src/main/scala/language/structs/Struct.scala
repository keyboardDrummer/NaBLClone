package language.structs

import bindingTypeMachine.Machine
import constraints.ConstraintBuilder
import constraints.objects.NamedDeclaration
import constraints.scopes.objects.{ConcreteScope, Scope}
import constraints.types.AssignSubType
import constraints.types.objects.{ConcreteType, StructType}

class Struct(name: String, fields: Seq[Field], parent: Option[String] = None)
{
  def evaluate(machine: Machine) = {
    val structDeclaration = machine.declare(name, this)
    val scopeOfParent: Option[ConcreteScope] = parent.map(p => {
      val parentDeclaration: NamedDeclaration = machine.resolve(p, this)
      val scopeOfParent = machine.resolveScope(parentDeclaration)
      machine.subType(StructType(structDeclaration), StructType(parentDeclaration))
      scopeOfParent
    })

    val currentScope = machine.currentScope
    val structScope: ConcreteScope = machine.newScope()
    machine.scopeGraph.declareScope(structDeclaration, structScope)
    scopeOfParent.foreach(ps => machine.scopeGraph.parent(structScope, ps))

    fields.foreach(field => {
      val _type: ConcreteType = field._type.evaluate(machine)
      machine.declare(field.name, field, _type)
    })

    machine.currentScope = currentScope
  }

  def constraints(builder: ConstraintBuilder, parentScope: Scope) =
  {
    val structDeclaration: NamedDeclaration = builder.declaration(name, this, parentScope)
    val scopeOfParent: Option[Scope] = parent.map(p => {
      val parentDeclaration = builder.declarationVariable()
      val scopeOfParent = builder.declaredScopeVariable(parentDeclaration)
      builder.reference(p, this, parentScope, parentDeclaration)
      builder.add(Seq(AssignSubType(StructType(structDeclaration), StructType(parentDeclaration))))
      scopeOfParent
    })
    val structScope = builder.declaredNewScope(structDeclaration, scopeOfParent)
    fields.foreach(field => {
      val _type = builder.typeVariable()
      builder.declaration(field.name, field, structScope, Some(_type))
      field._type.constraints(builder, _type, parentScope)
    })
  }
}
