import scala.collection.mutable

object ConstraintResolver
{
  def resolve(input: Seq[Constraint]): Seq[Constraint] = {
    val scopeConstraints: Seq[ScopeConstraint] = input.collect({ case s:ScopeConstraint => s })
    val graph = createGraph(scopeConstraints)
    var environment = Map.empty[Declaration, Type]
    var progress = true
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
          environment.get(declaration).fold[Unit](() => {
            environment = environment + (declaration -> _type)
          })(existingType => {
            if (!unifyTypes(allConstraints, existingType, _type))
              constraintsForNextRun.enqueue(x)
          })
      })
      progress = constraintsForNextRun.size != startingSize
    }
    constraintsForNextRun
  }


  def createGraph(input: Seq[ScopeConstraint]) : Graph = {
    val result = new Graph()
    input.foreach {
      case ReferenceInScope(reference, scope) => result.add(ReferenceNode(reference), ReferenceEdge(ScopeNode(scope)))
      case DeclarationInScope(declaration, scope) => result.add(ScopeNode(scope), DeclaredIn(DeclarationNode(declaration)))
      case ParentScope(child, parent) => result.add(ScopeNode(child), Parent(ScopeNode(parent)))
      case DeclarationOfScope(declaration, scope) => result.add(DeclarationNode(declaration), Declares(ScopeNode(scope)))
      case ScopeImport(scope, reference) => result.add(ScopeNode(scope), ImportEdge(ReferenceNode(reference)))
    }
    result
  }
}

trait GraphNode
case class ScopeNode(scope: Scope) extends GraphNode
case class ReferenceNode(reference: Reference) extends GraphNode
case class DeclarationNode(declaration: NamedDeclaration) extends GraphNode


trait GraphEdge {
  def target: GraphNode
}
case class ReferenceEdge(target: ScopeNode) extends GraphEdge
case class ImportEdge(target: ReferenceNode) extends GraphEdge
case class DeclaredIn(target: DeclarationNode) extends GraphEdge
case class Parent(target: ScopeNode) extends GraphEdge
case class Declares(target: ScopeNode) extends GraphEdge

class Graph extends scala.collection.mutable.HashMap[GraphNode, mutable.Set[GraphEdge]]
{
  def resolve(reference: Reference): NamedDeclaration = {
    val reachableNodes = depthFirst(new ReferenceNode(reference)).collect({case d:DeclarationNode => d}).
      filter(d => d.declaration.name == reference.name)
    if (reachableNodes.size == 1)
    {
      return reachableNodes.head.declaration
    }
    null
  }

  def depthFirst(root: GraphNode): Seq[GraphNode] = {
    var result = List.empty[GraphNode]
    val visited = mutable.Set.empty[GraphNode]
    val queue = new mutable.Queue[GraphNode]
    queue.enqueue(root)
    while(queue.nonEmpty)
    {
      val element = queue.dequeue()
      if (visited.add(element))
      {
        result ::= element
        this.get(element).foreach(x => x.foreach(c => queue.enqueue(c.target)))
      }
    }
    result.reverse
  }

  def add(node: GraphNode, edge: GraphEdge): Unit =
  {
    val edges = this.getOrElseUpdate(node, mutable.Set.empty)
    edges.add(edge)
  }
}
