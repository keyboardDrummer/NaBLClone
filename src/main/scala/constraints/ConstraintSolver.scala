package constraints

import constraints.objects.{Declaration, DeclarationVariable}
import constraints.scopes._
import constraints.scopes.objects.{ConcreteScope, Scope, ScopeVariable}
import constraints.types.{TypeGraph, TypeNode}
import constraints.types.objects._

class ConstraintSolver(val factory: Factory, val startingConstraints: Seq[Constraint])
{
  val scopeGraph = new ScopeGraph
  val typeGraph = new TypeGraph
  var environment = Map.empty[Declaration, Type]
  var constraints: Seq[Constraint] = startingConstraints
  var mappedTypeVariables: Map[TypeVariable, Type] = Map.empty

  def run() : Boolean = {
    var progress = true
    while(progress && constraints.nonEmpty)
    {
      progress = cycle()
    }
    constraints.isEmpty
  }

  def cycle() : Boolean = {
    val remainingConstraints = constraints.filter(c =>
      !c.apply(this)
    )
    val result = constraints.size > remainingConstraints.size
    constraints = remainingConstraints
    result
  }

  def instantiateType(v: TypeVariable, t: Type): Boolean = {
    if (t.variables.contains(v))
      return false

    mappedTypeVariables += v -> t
    startingConstraints.foreach(c => c.instantiateType(v, t)) //TODO startingConstraints mag ook gewoon constraints zijn.
    environment = environment.mapValues(existingType => existingType.instantiateType(v, t))
    true
  }

  def instantiateScope(v: ScopeVariable, s: Scope) = {
    constraints.foreach(c => c.instantiateScope(v, s))
  }

  def unifyScopes(left: Scope, right: Scope): Boolean = (left, right) match {
    case (v: ScopeVariable, _) =>
      instantiateScope(v, right); true
    case (_, v: ScopeVariable) =>
      instantiateScope(v, left); true
    case (ConcreteScope(x), ConcreteScope(y)) => if (x == y) true else false
    case _ => false
  }

  def canAssignTo(target: Type, value: Type): Boolean = (target, value) match {
    case (v: TypeVariable,_) => false
    case (_,v: TypeVariable) => false
    case _ => typeGraph.isSuperType(TypeNode(target), TypeNode(value))
  }

  def unifyTypes(left: Type, right: Type): Boolean = (left,right) match {
    case (v: TypeVariable,_) => mappedTypeVariables.get(v) match
    {
      case Some(newLeft) => unifyTypes(newLeft, right)
      case _ => instantiateType(v,right)
    }
    case (_,v: TypeVariable) =>  mappedTypeVariables.get(v) match
    {
      case Some(newRight) => unifyTypes(left, newRight)
      case _ => instantiateType(v,left)
    }
    case(StructType(leftDeclaration), StructType(rightDeclaration)) =>
      unifyDeclarations(leftDeclaration, rightDeclaration)
    case (PrimitiveType(leftName), PrimitiveType(rightName)) => leftName == rightName
    case (TypeApplication(leftFunction, leftArguments), TypeApplication(rightFunction, rightArguments)) =>
      if (leftArguments.size == rightArguments.size && unifyTypes(leftFunction, rightFunction))
        leftArguments.indices.forall(index =>
          unifyTypes(left.asInstanceOf[TypeApplication].arguments(index), right.asInstanceOf[TypeApplication].arguments(index)))
      else
        false
    case _ =>
      false
  }

  def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {
    constraints.foreach(x => x.instantiateDeclaration(variable, instance))
    environment = environment.map(kv => if (kv._1 == variable) (instance, kv._2) else kv)
  }

  def unifyDeclarations(left: Declaration, right: Declaration): Boolean = (left, right) match {
    case (v:DeclarationVariable,_) =>
      instantiateDeclaration(v, right); true
    case (_, v:DeclarationVariable) =>
      instantiateDeclaration(v, left); true
    case _ => left == right
  }

  def boundVariables : Set[TypeVariable] = {
    val constraintTypes = constraints.flatMap(c => c.boundTypes)
    constraintTypes.flatMap(t => t.variables).toSet
  }
}
