package language.types

import constraints.types.objects.AppliedType

class PrimitiveType(name: String) extends AppliedType(name, Seq.empty)
{
  override def toString: String = name
}

object LongType extends PrimitiveType("Long")
object IntType extends PrimitiveType("Int")
object BoolType extends PrimitiveType("Bool")
