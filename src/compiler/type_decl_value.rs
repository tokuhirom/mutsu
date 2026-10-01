//! A type declaration in value position.

use super::*;

impl Compiler {
    /// Compile a class or role declaration (`my` or `our`)
    /// in value position: register it, then leave its type object on the
    /// stack — a declaration is an expression whose value is the type
    /// (`sub f { my class B { } }` returns `B`). Returns `false`, compiling
    /// nothing, for any other statement. Shared by every value-tail path (a
    /// `do` statement, a routine/block/branch tail) so they cannot drift.
    pub(super) fn compile_type_decl_value(&mut self, stmt: &Stmt) -> bool {
        match stmt {
            Stmt::ClassDecl { name_expr, .. } => {
                // Register the class and return the type object.
                //
                // `PushLastRegisteredClass` pushes the type object the
                // `RegisterClass` op immediately above JUST created, read
                // off `Interpreter::last_registered_class_key` — the actual
                // registry key it was stored under (package-qualified
                // and/or lexically mangled per `exec_register_class_op`),
                // not a fresh bareword lookup of the source-level name. A
                // bareword lookup here can resolve to an unrelated,
                // same-named class from a completely different scope: e.g.
                // `class A { has $.x }` written as an expression inside
                // `EVAL`'d code that executes in a different package than
                // the caller resolves the bare name `A` to whichever `A`
                // the CALLER already declared, not the class this
                // declaration just created (see
                // `news/2026-08/class-decl-expr-is-not-a-name-lookup.md`).
                self.compile_stmt(stmt);
                if let Some(expr) = name_expr {
                    self.compile_expr(expr);
                    self.code.emit(OpCode::IndirectTypeLookup);
                } else {
                    self.code.emit(OpCode::PushLastRegisteredClass);
                }
            }
            Stmt::RoleDecl { .. } => {
                // Register the role and return the role type object. Rakudo
                // hands back the INDIVIDUAL parametric role just declared (a
                // `ParametricRoleHOW`), not the same-named role *group* the
                // installed name resolves to (a `ParametricRoleGroupHOW`) —
                // `PushLastRegisteredRole` starts from the actual qualified
                // group this declaration installed; a bareword lookup can
                // select an unrelated same-named role from another scope.
                // `RoleGroupToCandidate` then narrows that group to the new
                // individual candidate.
                self.compile_stmt(stmt);
                self.code.emit(OpCode::PushLastRegisteredRole);
                self.code.emit(OpCode::RoleGroupToCandidate);
            }
            _ => return false,
        }
        true
    }
}
