import scala.collection.mutable

object ConstraintResolver
{


  //noinspection MatchToPartialFunction
  def resolve(input: Seq[Constraint]): Seq[Constraint] = {
    val graph = new Graph
    var environment = Map.empty[Declaration, Type]
    val constraintsForNextRun = new mutable.Queue[Constraint]()
    constraintsForNextRun.enqueue(input:_*)

    var allConstraints: List[Constraint] = null
    def instantiateType(v: TypeVariable, t: Type) = {
      allConstraints.foreach(c => c.instantiateType(v, t))
      environment = environment.mapValues(existingType => if (existingType == v) t else existingType)
    }

    def instantiateScope(v: ScopeVariable, s: Scope) = {
      allConstraints.foreach(c => c.instantiateScope(v, s))
    }

    def unifyScopes(left: Scope, right: Scope): Boolean = (left, right) match {
      case (v: ScopeVariable, _) => instantiateScope(v, right); true
      case (_, v: ScopeVariable) => instantiateScope(v, left); true
      case (ConcreteScope(x), ConcreteScope(y)) => if (x == y) true else false
      case _ => false
    }

    def unifyTypes(left: Type, right: Type): Boolean = (left,right) match {
      case (v: TypeVariable,_) => instantiateType(v,right); true
      case (_,v: TypeVariable) => instantiateType(v,left); true
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

    var progress = true
    while(progress && constraintsForNextRun.nonEmpty)
    {
      val startingSize = constraintsForNextRun.size
      allConstraints = constraintsForNextRun.toList
      constraintsForNextRun.clear()
      allConstraints.foreach(x => x match {

        case ReferenceInScope(reference, s:ConcreteScope) => graph.add(ReferenceNode(reference), ReferenceEdge(ScopeNode(s)))
        case DeclarationInsideScope(declaration, scope: ConcreteScope) => graph.add(ScopeNode(scope), DeclaredIn(DeclarationNode(declaration)))
        case ParentScope(child: ConcreteScope, parent: ConcreteScope) => graph.add(ScopeNode(child), Parent(ScopeNode(parent)))
        case ScopeImport(scope: ConcreteScope, importedScope: ConcreteScope) => graph.add(ScopeNode(scope), ImportEdge(ScopeNode(importedScope)))
        case ResolvesTo(reference, declaration) =>
          val resolvedDeclaration = graph.resolve(reference)
          if (resolvedDeclaration != null)
          {
            declaration match {
              case NamedDeclaration(name, _) => if (name != resolvedDeclaration.name)
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
        case TypesAreEqual(left, right) => if (!unifyTypes(left, right))
          constraintsForNextRun.enqueue(x)
        case DeclarationOfType(declaration, _type) =>
          environment = environment.get(declaration).fold[Map[Declaration, Type]]({
            environment + (declaration -> _type)
          })((existingType: Type) => {
            if (!unifyTypes(existingType, _type)) {
              constraintsForNextRun.enqueue(x)
            }
            environment
          })
        case DeclarationOfScope(declaration: NamedDeclaration, scope) =>
            val edges = graph.getOrElseUpdate(DeclarationNode(declaration), mutable.Set.empty[GraphEdge])
            val declaredEdge = edges.find(e => e.isInstanceOf[Declares])
            if (declaredEdge.nonEmpty) {
              if (!unifyScopes(declaredEdge.head.asInstanceOf[Declares].target.scope, scope)) {
                constraintsForNextRun.enqueue(x)
              }
            }
            else {
              scope match {
                case c: ConcreteScope => edges.add(Declares(ScopeNode(c)))
                case _ => constraintsForNextRun.enqueue(x)
              }
            }
        case _ => constraintsForNextRun.enqueue(x)
      })
      progress = constraintsForNextRun.size != startingSize
    }
    constraintsForNextRun
  }
}
