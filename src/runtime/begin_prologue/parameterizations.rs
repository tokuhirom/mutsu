//! Compile-time parameterization of a class with its own `^parameterize`
//! (#17085).
//!
//! Rakudo resolves a type written `C[T]` -- a variable's or parameter's type
//! constraint, a return type, the term `C[T]` -- while it compiles the code,
//! by calling `C.HOW.parameterize(C, T)`, which a class can supply as `method
//! ^parameterize`. Its side effects therefore happen at compile time, in
//! source order, interleaved with the unit's `BEGIN` blocks, and never again
//! when the statement runs (a loop body's `my C[T] $x` calls it once).
//!
//! mutsu resolves such a type when the code first needs it and keeps the
//! result per type arguments (`Interpreter::parameterize_by_user_metamethod`).
//! Here every statement of a unit that spells `C[T]`, for a `C` the unit
//! declared earlier with its own `^parameterize` and `T` given as type names,
//! gets a `BEGIN C[T];` ahead of it; the prologue split then runs it at BEGIN
//! time, and the run-time uses find the type already built.
//!
//! Only classes this unit declares are known here; a class imported from a
//! module is still parameterized when the code first needs it.

use crate::ast::{Expr, PhaserKind, Stmt};
use crate::ast_visit::{NameKind, Visit, walk_expr, walk_stmt};
use std::collections::HashSet;

/// Insert a `BEGIN C[T];` ahead of each top-level statement that spells a
/// parameterization of a unit class with its own `^parameterize` (see the
/// module docs). One BEGIN per statement, holding its distinct spellings in
/// source order.
// Cost: O(n), n = size of the unit's AST.
pub(super) fn lift_parameterizations(stmts: &mut Vec<Stmt>) {
    let mut parametric: HashSet<String> = HashSet::new();
    let mut out = Vec::with_capacity(stmts.len());
    for stmt in std::mem::take(stmts) {
        if !parametric.is_empty() {
            let mut params = ParamNames::default();
            params.visit_stmt(&stmt);
            let mut scan = Spellings {
                parametric: &parametric,
                bound: &params.names,
                found: Vec::new(),
            };
            scan.visit_stmt(&stmt);
            if !scan.found.is_empty() {
                let body = scan
                    .found
                    .iter()
                    .filter_map(|spelling| parameterization_expr(spelling))
                    .map(Stmt::Expr)
                    .collect::<Vec<_>>();
                if !body.is_empty() {
                    out.push(Stmt::Phaser {
                        kind: PhaserKind::Begin,
                        body,
                        condition: None,
                        end_index: None,
                        spans: None,
                        bare: false,
                        scope: 0,
                    });
                }
            }
        }
        declare_parametric_classes(&stmt, &mut parametric);
        out.push(stmt);
    }
    *stmts = out;
}

/// Record the classes `stmt` declares with their own `method ^parameterize`
/// (an exported one arrives in the parser's synthetic group, which
/// `scope_members` looks through).
// Cost: O(m), m = members of the declared class bodies.
fn declare_parametric_classes(stmt: &Stmt, parametric: &mut HashSet<String>) {
    for member in crate::ast::scope_members(std::slice::from_ref(stmt)) {
        if let Stmt::ClassDecl { name, body, .. } = member
            && crate::ast::scope_members(body).any(
                |s| matches!(s, Stmt::MethodDecl { name, .. } if *name == "^parameterize"),
            )
        {
            parametric.insert(name.resolve().to_string());
        }
    }
}

/// Collects the parameterizations of the known parametric classes a
/// statement spells, by their spelling (`C[A, B]`).
struct Spellings<'p> {
    parametric: &'p HashSet<String>,
    /// Names the statement binds at run time (see [`ParamNames`]).
    bound: &'p HashSet<String>,
    found: Vec<String>,
}

impl Spellings<'_> {
    // Cost: O(n), n = chars of the spelling, plus the found list.
    fn note(&mut self, spelling: String) {
        let Some((base, args)) = crate::runtime::types::split_parameterization(&spelling) else {
            return;
        };
        // An argument the code binds when it runs (`method make(\t) { C[t] }`,
        // `sub f(::T $x) { my C[T] $y }`) is not known at compile time.
        if !self.parametric.contains(base)
            || args.iter().any(|arg| mentions_bound(arg, self.bound))
            || self.found.contains(&spelling)
        {
            return;
        }
        self.found.push(spelling);
    }
}

/// Whether a type argument spelling names one of `bound` (as a whole word).
// Cost: O(n * b), n = chars of the argument, b = bound names.
fn mentions_bound(arg: &str, bound: &HashSet<String>) -> bool {
    arg.split(|c: char| !(c.is_alphanumeric() || matches!(c, '_' | '-' | ':')))
        .any(|word| bound.contains(word))
}

/// The names a statement's parameters bind (sigilless `\t`, `$t`'s `t`, and
/// type captures `::T`): an argument spelled with one is a run-time value.
#[derive(Default)]
struct ParamNames {
    names: HashSet<String>,
}

impl<'ast> Visit<'ast> for ParamNames {
    fn visit_param(&mut self, param: &'ast crate::ast::ParamDef) {
        let name = param.name.trim_start_matches(['$', '@', '%', '&', '\\']);
        if !name.is_empty() {
            self.names.insert(name.to_string());
        }
        if let Some(capture) = &param.type_capture {
            self.names.insert(capture.trim_start_matches(':').to_string());
        }
        crate::ast_visit::walk_param(self, param);
    }

    // A loop's or pointy block's parameter, and a sigilless `my \t`.
    fn visit_name(&mut self, name: &str, kind: NameKind) {
        if matches!(kind, NameKind::BlockParam | NameKind::VarDecl) {
            let name = name.trim_start_matches(['$', '@', '%', '&', '\\']);
            if !name.is_empty() {
                self.names.insert(name.to_string());
            }
        }
    }
}

impl<'ast> Visit<'ast> for Spellings<'_> {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        // A class's own declaration cannot parameterize itself before it is
        // composed; Rakudo would not either.
        if let Stmt::ClassDecl { name, .. } = stmt
            && self.parametric.contains(name.resolve().as_str())
        {
            return;
        }
        walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if let Expr::Index {
            target,
            index,
            is_positional: true,
            ..
        } = expr
            && let Expr::BareWord(base) = target.as_ref()
            && let Some(args) = type_name_args(index)
        {
            self.note(format!("{base}[{}]", args.join(", ")));
        }
        walk_expr(self, expr);
    }

    fn visit_name(&mut self, name: &str, kind: NameKind) {
        // A type constraint keeps its spelling, smileys and all: only a plain
        // `C[...]` is a parameterization.
        if kind == NameKind::Type && name.ends_with(']') && name.contains('[') {
            self.note(name.to_string());
        }
    }
}

/// The type names of a term's subscript, when it holds only type names
/// (`C[Int]`, `C[Int, Str]`).
// Cost: O(a), a = arguments.
fn type_name_args(index: &Expr) -> Option<Vec<&str>> {
    let is_type_name = |s: &str| s.starts_with(|c: char| c.is_alphabetic() || c == '_');
    match index {
        Expr::BareWord(name) if is_type_name(name) => Some(vec![name.as_str()]),
        Expr::ArrayLiteral(items) if !items.is_empty() => items
            .iter()
            .map(|item| match item {
                Expr::BareWord(name) if is_type_name(name) => Some(name.as_str()),
                _ => None,
            })
            .collect(),
        _ => None,
    }
}

/// The term `C[A, B]` for a spelling, its arguments type names or nested
/// parameterizations. `None` for a spelling that is not one.
// Cost: O(n), n = chars of the spelling.
fn parameterization_expr(spelling: &str) -> Option<Expr> {
    let (base, args) = crate::runtime::types::split_parameterization(spelling)?;
    let mut items = args
        .into_iter()
        .map(|arg| {
            if arg.contains('[') {
                parameterization_expr(arg)
            } else if arg.starts_with(|c: char| c.is_alphabetic() || c == '_')
                && arg.chars().all(|c| c.is_alphanumeric() || matches!(c, '_' | '-' | ':'))
            {
                Some(Expr::BareWord(arg.to_string()))
            } else {
                None
            }
        })
        .collect::<Option<Vec<_>>>()?;
    let index = if items.len() == 1 {
        items.pop()?
    } else {
        Expr::ArrayLiteral(items)
    };
    Some(Expr::Index {
        target: Box::new(Expr::BareWord(base.to_string())),
        index: Box::new(index),
        is_positional: true,
        spelling: crate::ast::IndexSpelling::Subscript,
    })
}
