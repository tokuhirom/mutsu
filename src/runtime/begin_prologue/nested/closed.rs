//! Whether a unit-level `BEGIN` can run when the parser reaches it, before the
//! unit's frame exists (ADR-17558 slice 5).
//!
//! The prologue runs a `BEGIN` in the unit's own frame, where it sees the
//! unit's lexicals in their static state, the routines and types the unit
//! declared ahead of it, and the imports of the `use`s before it. At parse time
//! none of that exists yet: the unit is not compiled, and a parse-time `use`
//! has loaded its module without importing it. So a `BEGIN` runs at parse time
//! only when its body reaches none of it. Then running it there is
//! indistinguishable from running it in the prologue, except for its order
//! against the `use`s around it, which is rakudo's.
//!
//! The body is *closed* when, compiled on its own ([`Scan`]):
//!
//! - it has no free lexical: every variable it reads or writes it declares
//!   itself, or is a dynamic variable (`%*ENV`), which is looked up at run
//!   time along the callers and in PROCESS, the same at parse time as in the
//!   prologue as long as the unit declares none of that name ahead of the
//!   `BEGIN` (that declaration is a prologue effect, which keeps the `BEGIN`
//!   in the prologue) and the compiler does not bind it (`$*R`, `$*CU`);
//! - every routine it calls is a core one the unit does not redeclare or
//!   import (a parse-time `use` has not imported yet), and it calls none by a
//!   qualified name;
//! - it cannot reach a name dynamically (`EVAL`, `::($name)`, `CALLER::`);
//! - every type or bare term it names is a core one the unit has not
//!   redeclared or imported;
//! - it declares nothing the unit could see: no routine, package, type, `our`
//!   variable, no `use`, `need`, `import` or `require`, and no `BEGIN` of its
//!   own.

use super::mentions::Mentions;
use super::scan::{Scan, is_core_routine};
use crate::ast::{Expr, Stmt};
use crate::ast_visit::{NameKind, Visit, walk_expr, walk_stmt};

/// The names `body`, the body of a unit-level statement-form `BEGIN`, uses,
/// when it reaches nothing of its unit but core names (see the module docs);
/// `None` when it reaches more. The unit must provide none of the names
/// returned (declare or import one of that name ahead of the `BEGIN`) for the
/// body to be closed: they then name the core routines, types and terms.
// Cost: O(n), n = size of `body` and of the code it compiles to.
pub(crate) fn begin_body_core_names(body: &[Stmt], bare: bool) -> Option<Vec<String>> {
    // A blockless `BEGIN my $x = ...` declares into the unit's scope.
    let mut escapes = Escapes {
        found: false,
        bare,
    };
    for stmt in body {
        escapes.visit_stmt(stmt);
    }
    if escapes.found {
        return None;
    }
    let mentions = Mentions::of(body);
    if mentions.symbolic || mentions.has_begin || mentions.changes_type {
        return None;
    }
    let mut names = Vec::new();
    for (name, kind) in &mentions.names {
        let core = match kind {
            NameKind::SubDecl | NameKind::Decl | NameKind::Attribute => false,
            NameKind::Type => crate::known_types::is_known_type_constraint(name),
            NameKind::Term => {
                crate::known_types::is_known_type_constraint(name)
                    || crate::runtime::Interpreter::is_builtin_function(name)
                    || matches!(name.as_str(), "True" | "False" | "Nil" | "Empty" | "Inf" | "NaN")
            }
            _ => continue,
        };
        if !core {
            return None;
        }
        names.push(name.clone());
    }
    let scan = Scan::of(body);
    if scan.reflective {
        return None;
    }
    for free in scan.free() {
        // A dynamic variable is looked up at run time, along the callers and
        // then in PROCESS, which are the same at parse time as in the
        // prologue, unless the unit declares it (a prologue effect, which
        // keeps this `BEGIN` in the prologue) or the compiler binds it.
        match crate::parser::dynamic_var_key(free.as_str()) {
            Some(key) if !COMPILE_TIME_DYNAMICS.contains(&key.as_str()) => {}
            _ => return None,
        }
    }
    for callee in &scan.callees {
        if !is_core_routine(*callee) || matches!(callee.as_str(), "require" | "EVAL" | "EVALFILE") {
            return None;
        }
        names.push(callee.resolve());
    }
    Some(names)
}

/// The dynamic variables the compile of the unit binds for its BEGIN-time
/// code (the resolver, the compunit, the grammar): a `BEGIN` that reads one
/// needs the compile it belongs to.
const COMPILE_TIME_DYNAMICS: &[&str] = &["$*R", "$*CU", "$*W", "$*LANG", "%*LANG"];

/// Finds what a `BEGIN` body could declare into, or load for, its unit.
struct Escapes {
    found: bool,
    /// The phaser has no block: any variable it declares is the unit's.
    bare: bool,
}

impl<'ast> Visit<'ast> for Escapes {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        self.found |= matches!(
            stmt,
            Stmt::Use { .. }
                | Stmt::No { .. }
                | Stmt::Need { .. }
                | Stmt::Import { .. }
                | Stmt::Package { .. }
                | Stmt::ClassDecl { .. }
                | Stmt::VarDecl { is_our: true, .. }
        ) || (self.bare && matches!(stmt, Stmt::VarDecl { .. }));
        if !self.found {
            walk_stmt(self, stmt);
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if !self.found {
            walk_expr(self, expr);
        }
    }
}
