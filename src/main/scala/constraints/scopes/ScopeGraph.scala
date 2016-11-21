package constraints.scopes

import constraints._
import constraints.objects.{NamedDeclaration, Reference}
import constraints.scopes.objects.ConcreteScope

import scala.collection.mutable

trait GraphNode
case class ScopeNode(scope: ConcreteScope) extends GraphNode
{
  override def toString = scope.toString
}
case class ReferenceNode(reference: Reference) extends GraphNode
{
  override def toString = reference.toString
}
case class DeclarationNode(declaration: NamedDeclaration) extends GraphNode
{
  override def toString = declaration.toString
}


trait GraphEdge {
  def target: GraphNode
  def traverse: Boolean
}
case class ReferenceEdge(target: ScopeNode) extends GraphEdge
{
  override def traverse: Boolean = true
}
case class ImportEdge(target: ScopeNode) extends GraphEdge {
  override def traverse: Boolean = true
}
case class DeclaredIn(target: DeclarationNode) extends GraphEdge {
  override def traverse: Boolean = true
}
case class Parent(target: ScopeNode) extends GraphEdge {
  override def traverse: Boolean = true
}
case class Declares(target: ScopeNode) extends GraphEdge {
  override def traverse: Boolean = false
}

class ScopeGraph extends scala.collection.mutable.HashMap[GraphNode, mutable.Set[GraphEdge]]
{
  def resolve(reference: Reference): NamedDeclaration = {
    val reachableNodes = depthFirst(ReferenceNode(reference)).collect({case d:DeclarationNode => d}).
      filter(d => d.declaration.name == reference.name)
    if (reachableNodes.nonEmpty)
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
        this.get(element).foreach(x => x.filter(c => c.traverse).foreach(c => queue.enqueue(c.target)))
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
