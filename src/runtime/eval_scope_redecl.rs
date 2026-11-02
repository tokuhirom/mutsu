//! The duplicate-routine check of an `EVAL` unit, per scope (#15909).
//!
//! A routine may be declared again in a nested scope, where it shadows the
//! outer one; two declarations among the siblings of ONE statement list are
//! `X::Redeclaration`. The run-time registration cannot tell the two apart
//! (it sees a name that already exists), so the unit is judged here, once,
//! by its shape: every statement list the tree has (the unit, a block, a
//! routine or closure body) is a scope of its own. The run-time check then
//! lets a nested scope shadow (`eval_blocks_lexical_shadow`).

use super::*;
use crate::ast_visit::{Visit, walk_stmts};

/// The first routine redeclared among the siblings of one scope anywhere in
/// `stmts`, as `X::Redeclaration`.
// Cost: O(n), n = size of the unit's AST.
pub(super) fn find_scope_routine_redeclaration(stmts: &[Stmt]) -> Option<RuntimeError> {
    let mut check = ScopeRoutines { found: None };
    check.visit_stmts(stmts);
    check.found
}

struct ScopeRoutines {
    found: Option<RuntimeError>,
}

impl ScopeRoutines {
    /// Judge one statement list: a name declared by two `sub`s, unless both
    /// are `multi` or the first is a `{...}` stub the second completes.
    fn judge(&mut self, body: &[Stmt]) {
        // Whether every declaration of the name so far is a `multi`.
        let mut seen: HashMap<String, bool> = HashMap::new();
        let mut stubbed: HashSet<String> = HashSet::new();
        let mut prev_was_anon = false;
        for stmt in body {
            if stmt.is_marker() {
                continue;
            }
            let was_anon = prev_was_anon;
            prev_was_anon = matches!(stmt, Stmt::Expr(Expr::BareWord(w)) if w == "anon");
            let Stmt::SubDecl {
                name,
                multi,
                body,
                supersede,
                ..
            } = stmt
            else {
                continue;
            };
            // `anon sub` declares nothing, `supersede sub` replaces on purpose.
            if was_anon || *supersede {
                continue;
            }
            let name = name.resolve().to_string();
            if name.is_empty() {
                continue;
            }
            let is_stub = is_stub_body(body);
            if stubbed.remove(&name) && !is_stub {
                continue;
            }
            match seen.get(&name) {
                None => {
                    seen.insert(name.clone(), *multi);
                    if is_stub {
                        stubbed.insert(name);
                    }
                }
                Some(all_multi) if *all_multi && *multi => {}
                Some(_) => {
                    self.found = Some(RuntimeError::redeclaration_routine(&name));
                    return;
                }
            }
        }
    }
}

/// A forward stub body is a single `... { !!! }` placeholder, compiled to a
/// lone `__mutsu_stub_die` / `__mutsu_stub_warn` call.
fn is_stub_body(body: &[Stmt]) -> bool {
    let real: Vec<_> = body.iter().filter(|s| !s.is_marker()).collect();
    real.len() == 1
        && matches!(real[0], Stmt::Expr(Expr::Call { name, .. })
            if crate::ast::stub::is_marker(name.as_str()))
}

impl<'ast> Visit<'ast> for ScopeRoutines {
    fn visit_stmts(&mut self, body: &'ast [Stmt]) {
        if self.found.is_some() {
            return;
        }
        self.judge(body);
        if self.found.is_none() {
            walk_stmts(self, body);
        }
    }
}
