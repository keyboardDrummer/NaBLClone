package constraints

import constraints.scopeConstraints._

class ConstraintSolver(val factory: Factory, val startingConstraints: Seq[Constraint])
{
  val graph = new Graph
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

  def unifyTypes(left: Type, right: Type): Boolean = (left,right) match {
    case (v: TypeVariable,_) => instantiateType(v,right); true
    case (_,v: TypeVariable) => instantiateType(v,left); true
    case(StructType(left), StructType(right)) =>
      unifyDeclarations(left, right)
    case (ConcreteType(leftName, leftArguments), ConcreteType(rightName, rightArguments)) =>
      if (leftName == rightName && leftArguments.size == rightArguments.size)
      {
        leftArguments.zip(rightArguments).foreach(t => unifyTypes(t._1, t._2))
        true
      }
      else
        false
    case _ =>
      false
  }

  def instantiateDeclaration(v: DeclarationVariable, right: Declaration): Unit = {
    constraints.foreach(x => x.instantiateDeclaration(v, right))
    environment = environment.map(kv => if (kv._1 == v) (right, kv._2) else kv)
  }

  def unifyDeclarations(left: Declaration, right: Declaration): Boolean = (left, right) match {
    case (v:DeclarationVariable,_) =>
      instantiateDeclaration(v, right); true
    case (_, v:DeclarationVariable) =>
      instantiateDeclaration(v, left); true
    case _ => false
  }
}
