package language.modules

import constraints.objects.NamedDeclaration
import constraints.scopes.imports.DeclarationOfScope
import constraints.scopes.objects.Scope
import constraints.scopes.{DeclarationInsideScope, ParentScope}
import constraints.{Constraint, ConstraintBuilder}
import language.structs.Struct

class Module(name: String, bindings: Seq[Binding], structs: Seq[Struct] = Seq.empty, imports: Seq[ModuleImport] = Seq.empty)
{
  def constraints(builder: ConstraintBuilder, parentScope: Scope): Seq[Constraint] = {
    val scope = builder.newScope()
    val moduleDeclaration = NamedDeclaration(name, this)
    Seq(DeclarationInsideScope(moduleDeclaration, parentScope), DeclarationOfScope(moduleDeclaration, scope), ParentScope(scope, parentScope)) ++
      structs.flatMap(struct => struct.constraints(builder, scope)) ++
      bindings.flatMap(binding => binding.constraints(builder, scope)) ++
      imports.flatMap(_import => _import.constraints(builder, scope)) //TODO moet bovenaan
  }
}
