import scala.collection.mutable

object ConstraintResolver
{
  //noinspection MatchToPartialFunction
  def resolve(input: Seq[Constraint]): Seq[Constraint] = {
    val scopeConstraints: Seq[ScopeConstraint] = input.collect({ case s:ScopeConstraint => s })
    val graph = createGraph(scopeConstraints)
    var environment = Map.empty[Declaration, Type]
    val constraintsForNextRun = new mutable.Queue[Constraint]()
    constraintsForNextRun.enqueue(input.diff(scopeConstraints):_*)

    var allConstraints: List[Constraint] = null
    def instantiateType(v: TypeVariable, t: Type) = {
      allConstraints.foreach(c => c.instantiateType(v, t))
      environment = environment.mapValues(existingType => if (existingType == v) t else existingType)
    }

    def unifyTypes(constraints: Seq[Constraint], left: Type, right: Type): Boolean = (left,right) match {
      case (v: TypeVariable,_) => instantiateType(v,right); true
      case (_,v: TypeVariable) => instantiateType(v,left); true
      case (ConcreteType(leftName, leftArguments), ConcreteType(rightName, rightArguments)) =>
        if (leftName == rightName && leftArguments.size == rightArguments.size)
        {
          leftArguments.zip(rightArguments).foreach(t => unifyTypes(constraints, t._1, t._2))
          true
        }
        else
          false
      case _ =>
        false
    }

    var progress = true
    while(progress && constraintsForNextRun.nonEmpty)
    {
      val startingSize = constraintsForNextRun.size
      allConstraints = constraintsForNextRun.toList
      constraintsForNextRun.clear()
      allConstraints.foreach(x => x match {
        case ResolvesTo(reference, declaration) =>
          val resolvedDeclaration = graph.resolve(reference)
          if (resolvedDeclaration != null)
          {
            declaration match {
              case NamedDeclaration(name) => if (name != resolvedDeclaration.name)
                throw new IllegalStateException("collision!")
              case v@DeclarationVariable(name) =>
                allConstraints.foreach(x => x.instantiateDeclaration(v, resolvedDeclaration))
                environment = environment.map(kv => if (kv._1 == v) (resolvedDeclaration, kv._2) else kv)
            }
          }
          else
          {
            constraintsForNextRun.enqueue(x)
          }
        case TypesAreEqual(left, right) => if (!unifyTypes(allConstraints, left, right))
          constraintsForNextRun.enqueue(x)
        case DeclarationOfType(declaration, _type) =>
          environment = environment.get(declaration).fold[Map[Declaration, Type]]({
            environment + (declaration -> _type)
          })((existingType: Type) => {
            if (!unifyTypes(allConstraints, existingType, _type)) {
              constraintsForNextRun.enqueue(x)
            }
            environment
          })
        case DeclarationOfScope(declaration, scope) => declaration match {
          case named:NamedDeclaration => graph.add(DeclarationNode(named), Declares(ScopeNode(scope)))
          case _ => constraintsForNextRun.enqueue(x)
        }
      })
      progress = constraintsForNextRun.size != startingSize
    }
    constraintsForNextRun
  }


  def createGraph(input: Seq[ScopeConstraint]) : Graph = {
    val result = new Graph()
    input.foreach {
      case ReferenceInScope(reference, scope) => result.add(ReferenceNode(reference), ReferenceEdge(ScopeNode(scope)))
      case DeclarationInsideScope(declaration, scope) => result.add(ScopeNode(scope), DeclaredIn(DeclarationNode(declaration)))
      case ParentScope(child, parent) => result.add(ScopeNode(child), Parent(ScopeNode(parent)))
      case ScopeImport(scope, importedScope) => result.add(ScopeNode(scope), ImportEdge(ScopeNode(importedScope)))
    }
    result
  }
}
