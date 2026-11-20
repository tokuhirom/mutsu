//! `my Mu $x := EXPR`: a declared `Mu` the bind makes unobservable.
//!
//! A `:=` declaration checks the binding against the variable's constraint,
//! and every value, container and type object binds to `Mu`. After the bind,
//! `$x.VAR` is the bound value or container, not a container of the
//! variable's own, and a later `$x := ...` rebind checks against `Mu` --
//! which is the constraint an untyped `$x` binds against too
//! (`my $x := 1; $x := Mu` works in rakudo). So the declared `Mu` changes
//! nothing a program can observe, and the declaration compiles exactly as
//! `my $x := EXPR`.
//!
//! What dropping it saves is the type ops' work on every execution of the
//! declaration (`SetVarTypeHoisted`, the typed `SetVarDynamic`,
//! `SetVarTypeScoped`, the deferred `TypeCheckBind`): ~7k instructions per
//! declaration, which AttrX::Mooish runs a dozen times per attribute access
//! (ADR-18244 stage B).

use super::*;

impl Compiler {
    /// Whether the declared constraint `tc` of the declaration of `name` is a
    /// `Mu` that its `:=` bind makes unobservable (see the module doc).
    ///
    /// Only a plain lexical `$` scalar qualifies: a `state`, `our`, dynamic
    /// or `constant` declaration (`other_scope`) keeps its own storage rules,
    /// a sigilless term or pointy parameter is bound by other machinery, and
    /// a lexically declared `Mu` type (`my class Mu { }`) is another type.
    // Cost: O(1).
    pub(super) fn mu_constraint_is_unobservable_bind(
        &self,
        tc: &str,
        name: &str,
        bind: Option<crate::ast::bind_decl::BindKind>,
        marks: &crate::ast::var_marks::VarMarks,
        other_scope: bool,
    ) -> bool {
        tc == "Mu"
            && matches!(bind, Some(crate::ast::bind_decl::BindKind::Scalar))
            && !marks.sigilless_decl
            && !marks.pointy_param
            && marks.term == crate::ast::var_marks::TermMark::No
            && !other_scope
            && crate::ast::bind_decl::is_scalar_bind_name(name)
            && !name.starts_with('*')
            && !self.lexical_types_in_scope.contains("Mu")
            && !self.class_names_current_scope.contains("Mu")
    }
}
