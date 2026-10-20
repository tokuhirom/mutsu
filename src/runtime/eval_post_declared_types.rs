use super::*;
use crate::ast_visit::{Visit, walk_expr};

/// The capitalised bareword terms of a subtree (`Foo`, `Foo.bar`): the type
/// names it refers to.
#[derive(Default)]
struct TypeRefs {
    names: Vec<String>,
}

impl<'ast> Visit<'ast> for TypeRefs {
    fn visit_expr(&mut self, expr: &'ast Expr) {
        if let Expr::BareWord(name) = expr
            && name.starts_with(|c: char| c.is_ascii_uppercase())
        {
            self.names.push(name.clone());
        }
        walk_expr(self, expr);
    }
}

impl Interpreter {
    /// Detect an *illegally post-declared* type: a type name used (as a term or
    /// method invocant) before its textual declaration in the same EVAL'd unit.
    /// Raku resolves type names lexically; using `Foo.bar` and only declaring
    /// `class Foo {}` (or grammar/role/enum/subset) *afterwards* is
    /// X::Undeclared::Symbols with a `post_types` entry, distinct from a
    /// never-declared name (`unk_types`). Mirrors rakudo's CHECK-time check,
    /// which also sees a use inside a routine or method body.
    // Cost: O(n), n = size of the unit's AST.
    pub(crate) fn check_eval_post_declared_types(
        &self,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        // Map each top-level type declaration name -> the index of the statement
        // that declares it (first declaration wins).
        let mut decl_index: HashMap<String, usize> = HashMap::new();
        for (i, stmt) in stmts.iter().enumerate() {
            match stmt {
                Stmt::ClassDecl { name, .. }
                | Stmt::RoleDecl { name, .. }
                | Stmt::SubsetDecl { name, .. }
                | Stmt::EnumDecl { name, .. }
                | Stmt::Package { name, .. } => {
                    decl_index.entry(name.resolve()).or_insert(i);
                }
                _ => {}
            }
        }
        if decl_index.is_empty() {
            return Ok(());
        }
        // For each statement, collect the type names it references and flag any
        // whose declaration only appears at a *later* top-level statement.
        for (i, stmt) in stmts.iter().enumerate() {
            let mut refs = TypeRefs::default();
            refs.visit_stmt(stmt);
            for name in refs.names {
                if let Some(&j) = decl_index.get(&name)
                    && j > i
                {
                    let msg = format!("Illegally post-declared type:\n    {} used at line 1", name);
                    return Err(RuntimeError::post_declared_type_symbols(&name, msg));
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::TypeRefs;
    use crate::ast_visit::walk_stmts;

    #[test]
    fn type_refs_reach_routine_bodies_but_not_literals() {
        let stmts = crate::parser::parse_program("sub f { Foo.new; say 'Bar' }").expect("parse").0;
        let mut refs = TypeRefs::default();
        walk_stmts(&mut refs, &stmts);
        assert!(refs.names.iter().any(|name| name == "Foo"));
        assert!(!refs.names.iter().any(|name| name == "Bar"));
    }
}
