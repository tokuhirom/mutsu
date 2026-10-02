//! Declaration scans over an `EVAL`'d snippet: the `my` lexicals it declares
//! (so the caller's same-named lexicals can be restored afterwards) and
//! duplicate `our sub` declarations. Both are typed AST visitors (ADR-0137).

use super::*;
use crate::ast_visit::{Visit, walk_expr, walk_stmt, walk_stmts};

/// The env keys of the `my` lexicals an `EVAL`'d snippet declares in its own
/// frame, in the form the environment uses (scalars sigil-less, `@`/`%`
/// keeping their sigil).
///
/// Only plain `my` declarations count: `our` is package-scoped and `state`
/// keeps its own cell, and neither shadows a caller lexical the way a `my`
/// does. Nested blocks, loop bodies and expressions (`say my $x = 1`) are
/// walked — a `my` there is EVAL-scoped too — but routine, package and
/// closure bodies are not: their lexicals live in their own frame and never
/// reach the caller's pad by this route. The names of the lexically scoped
/// types the snippet declares (`my class`, `my package`, `my role`) are
/// included, spelled as the environment binds them (type names are not
/// sigiled or lowercased).
// Cost: O(n), n = size of the snippet's AST.
pub(super) fn eval_declared_lexical_keys(stmts: &[Stmt]) -> HashSet<String> {
    let mut scan = EvalLexicalKeys::default();
    walk_stmts(&mut scan, stmts);
    scan.keys
}

#[derive(Default)]
struct EvalLexicalKeys {
    keys: HashSet<String>,
}

fn env_key(name: &str) -> Option<String> {
    let name = name.strip_prefix('\\').unwrap_or(name);
    // Scalars are stored sigil-less (`$a` -> `"a"`).
    let key = name.strip_prefix('$').unwrap_or(name);
    crate::env::is_plain_user_lexical(key).then(|| key.to_string())
}

impl EvalLexicalKeys {
    /// Record the env keys a lexical type declared as `name` binds: the name
    /// itself and, for a compound name, its last segment.
    fn insert_type_keys(&mut self, name: crate::symbol::Symbol) {
        if crate::qualified::is_qualified(name) {
            self.keys.insert(
                crate::qualified::unqualified_part(name)
                    .as_str()
                    .to_string(),
            );
        }
        self.keys.insert(name.as_str().to_string());
    }
}

impl<'ast> Visit<'ast> for EvalLexicalKeys {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        match stmt {
            Stmt::VarDecl {
                name,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                ..
            } => {
                if let Some(key) = env_key(name) {
                    self.keys.insert(key);
                }
                walk_stmt(self, stmt);
            }
            // A lexically scoped type is bound in the snippet's pad like a `my`
            // variable: its bare name shadows (and, once the snippet returns,
            // must give back) a caller binding of the same name. Both the
            // written name and its last segment are bound (`my class A::B`
            // also binds `B`). The body runs in a frame of its own.
            Stmt::ClassDecl {
                name,
                is_lexical: true,
                ..
            }
            | Stmt::Package {
                name, is_my: true, ..
            } => self.insert_type_keys(*name),
            Stmt::RoleDecl {
                name,
                custom_traits,
                ..
            } if custom_traits.iter().any(|(t, _)| t == "__my_scoped") => {
                self.insert_type_keys(*name)
            }
            // Routine, package and phaser bodies run in a frame of their own
            // (a phaser at another time), so their `my`s never shadow the
            // caller's pad.
            Stmt::SubDecl { .. }
            | Stmt::MethodDecl { .. }
            | Stmt::TokenDecl { .. }
            | Stmt::RuleDecl { .. }
            | Stmt::ProtoDecl { .. }
            | Stmt::ClassDecl { .. }
            | Stmt::RoleDecl { .. }
            | Stmt::AugmentClass { .. }
            | Stmt::Package { .. }
            | Stmt::PackageRuntimeBody { .. }
            | Stmt::Phaser { .. }
            | Stmt::Whenever { .. } => {}
            _ => walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        match expr {
            // Closures: their body runs in its own frame when called.
            Expr::AnonSub { .. }
            | Expr::AnonSubParams { .. }
            | Expr::Lambda { .. }
            | Expr::Block(_)
            | Expr::Gather(_)
            | Expr::PhaserExpr { .. } => {}
            _ => walk_expr(self, expr),
        }
    }
}

/// The first duplicate `our sub` in a snippet. Two `our sub foo` declarations
/// install the same package symbol, so a duplicate is X::Redeclaration
/// wherever the second one sits — a sibling block, a routine body or a
/// closure in the same package (rakudo agrees). `my sub` is lexical and does
/// not conflict across scopes, so it is ignored here.
// Cost: O(n), n = size of the snippet's AST.
pub(super) fn find_our_routine_redeclaration(stmts: &[Stmt]) -> Option<RuntimeError> {
    let mut scan = OurRoutines::default();
    walk_stmts(&mut scan, stmts);
    scan.found
}

#[derive(Default)]
struct OurRoutines {
    seen: HashSet<String>,
    found: Option<RuntimeError>,
}

impl<'ast> Visit<'ast> for OurRoutines {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.found.is_some() {
            return;
        }
        match stmt {
            Stmt::SubDecl {
                name,
                multi: false,
                custom_traits,
                ..
            } if custom_traits.iter().any(|(t, _)| t == "__our_scoped") => {
                let n = name.resolve().to_string();
                if !n.is_empty() && !self.seen.insert(n.clone()) {
                    let mut attrs = ValueMap::default();
                    attrs.insert("symbol".to_string(), Value::str(n.clone()));
                    attrs.insert("what".to_string(), Value::str("routine".to_string()));
                    attrs.insert(
                        "message".to_string(),
                        Value::str(format!("Redeclaration of routine '{}'", n)),
                    );
                    self.found = Some(RuntimeError::typed("X::Redeclaration", attrs));
                    return;
                }
                walk_stmt(self, stmt);
            }
            // A package body installs its `our sub`s into that package, a
            // different symbol from the snippet's.
            Stmt::ClassDecl { .. }
            | Stmt::RoleDecl { .. }
            | Stmt::AugmentClass { .. }
            | Stmt::Package { .. }
            | Stmt::PackageRuntimeBody { .. } => {}
            _ => walk_stmt(self, stmt),
        }
    }
}
