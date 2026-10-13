//! Every name a piece of code mentions, for the types a lifted BEGIN needs
//! (ADR-0134, slice 2; [`super::decls`]).

use crate::ast::{Expr, PhaserKind, Stmt};
use crate::ast_visit::{NameKind, Visit, contains_word, walk_expr, walk_stmt};

/// Every name some code mentions, and whether it can reach a name the scan
/// cannot list.
#[derive(Default)]
pub(super) struct Mentions {
    pub(super) names: Vec<(String, NameKind)>,
    /// A symbolic lookup or a pseudo-package (`::($name)`, `MY::{...}`).
    pub(super) symbolic: bool,
    /// A `BEGIN` of its own.
    pub(super) has_begin: bool,
    /// It may change a type through its metaobject (`K.^add_method(...)`,
    /// `K.HOW`, `augment`).
    pub(super) changes_type: bool,
}

/// The metamethods that only read a type. Any other may change it.
const READ_ONLY_METAMETHODS: &[&str] = &[
    "name",
    "shortname",
    "mro",
    "can",
    "isa",
    "does",
    "lookup",
    "find_method",
    "methods",
    "attributes",
    "roles",
    "parents",
    "ver",
    "auth",
    "api",
    "archetypes",
    "is_composed",
    "enum_values",
    "enum_value_list",
];

impl<'ast> Visit<'ast> for Mentions {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        self.has_begin |= matches!(
            stmt,
            Stmt::Phaser {
                kind: PhaserKind::Begin,
                ..
            }
        );
        self.changes_type |= matches!(stmt, Stmt::AugmentClass { .. });
        if let Stmt::Call { name, .. } = stmt {
            self.note_call(name.resolve().as_str());
        }
        walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        self.has_begin |= matches!(
            expr,
            Expr::PhaserExpr {
                kind: PhaserKind::Begin,
                ..
            }
        );
        if let Expr::MethodCall { name, modifier, .. } = expr {
            let name = name.resolve();
            self.changes_type |= name == "HOW"
                || (*modifier == Some('^') && !READ_ONLY_METAMETHODS.contains(&name.as_str()));
            if modifier.is_none() {
                self.note_call(name.as_str());
            }
        }
        if let Expr::Call { name, .. } = expr {
            self.note_call(name.resolve().as_str());
        }
        walk_expr(self, expr);
    }

    fn visit_name(&mut self, name: &str, kind: NameKind) {
        self.symbolic |= kind == NameKind::Symbolic;
        self.names.push((name.to_string(), kind));
    }
}

impl Mentions {
    /// An `EVAL` (`EVALFILE`) compiles source text the scan cannot read: it
    /// may name any type, and do anything to one.
    fn note_call(&mut self, name: &str) {
        if matches!(name, "EVAL" | "EVALFILE") {
            self.symbolic = true;
            self.changes_type = true;
        }
    }

    pub(super) fn of<'a>(stmts: impl IntoIterator<Item = &'a Stmt>) -> Mentions {
        let mut mentions = Mentions::default();
        for stmt in stmts {
            mentions.visit_stmt(stmt);
        }
        mentions
    }

    /// The code variables the code assigns to (`&g = ...`) without declaring
    /// them itself.
    pub(super) fn code_var_writes(&self) -> Vec<String> {
        let declared = |name: &str| {
            self.names
                .iter()
                .any(|(n, kind)| *kind == NameKind::VarDecl && n == name)
        };
        self.names
            .iter()
            .filter(|(name, kind)| {
                *kind == NameKind::AssignTarget && name.starts_with('&') && !declared(name)
            })
            .map(|(name, _)| name.clone())
            .collect()
    }

    /// Whether some mentioned name is `declared`, or a name qualified by it
    /// (`K::x`, `$P::v`) or qualifying it. A name counts wherever it occurs as
    /// a whole word, which errs toward naming it: that only costs a repeat.
    // Cost: O(n * m), n = total length of the mentioned names, m = `declared.len()`.
    pub(super) fn names(&self, declared: &str) -> bool {
        self.symbolic
            || self
                .names
                .iter()
                .any(|(name, _)| contains_word(name, declared))
    }
}
