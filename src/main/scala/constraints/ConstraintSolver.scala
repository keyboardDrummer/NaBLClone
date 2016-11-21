package constraints

import constraints.objects.{Declaration, DeclarationVariable}
import constraints.scopes._
import constraints.scopes.objects.{ConcreteScope, Scope, ScopeVariable}
import constraints.types.{TypeGraph, TypeNode}
import constraints.types.objects.{ConcreteType, StructType, Type, TypeVariable}

class ConstraintSolver(val factory: Factory, val startingConstraints: Seq[Constraint])
{
  val scopeGraph = new ScopeGraph
  val typeGraph = new TypeGraph
  var environment = Map.empty[Declaration, Type]
  var constraints: Seq[Constraint] = startingConstraints

  def run() : Boolean = {
    var progress = true
    while(progress && constraints.nonEmpty)
    {
      progress = cycle()
    }
    constraints.isEmpty
  }

  def cycle() : Boolean = {
    val remainingConstraints = constraints.filter(c => !c.apply(this))
    val result = constraints.size > remainingConstraints.size
    constraints = remainingConstraints
    result
  }

  def instantiateType(v: TypeVariable, t: Type) = {
    constraints.foreach(c => c.instantiateType(v, t))
    environment = environment.mapValues(existingType => if (existingType == v) t else existingType)
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
    case(StructType(leftDeclaration), StructType(rightDeclaration)) =>
      unifyDeclarations(leftDeclaration, rightDeclaration)
    case (ConcreteType(leftName, leftArguments), ConcreteType(rightName, rightArguments)) =>
      if (typeGraph.isSuperType(TypeNode(rightName), TypeNode(leftName)) && leftArguments.size == rightArguments.size)
        leftArguments.zip(rightArguments).forall(t => unifyTypes(t._1, t._2))
      else
        false
    case _ =>
      false
  }

  def unifyTypes(left: Type, right: Type): Boolean = (left,right) match {
    case (v: TypeVariable,_) => instantiateType(v,right); true
    case (_,v: TypeVariable) => instantiateType(v,left); true
    case(StructType(leftDeclaration), StructType(rightDeclaration)) =>
      unifyDeclarations(leftDeclaration, rightDeclaration)
    case (ConcreteType(leftName, leftArguments), ConcreteType(rightName, rightArguments)) =>
      if (leftName == rightName && leftArguments.size == rightArguments.size)
        leftArguments.zip(rightArguments).forall(t => unifyTypes(t._1, t._2))
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
}
