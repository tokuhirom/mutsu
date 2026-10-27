//! Whether a `for` body can tell an aliased `$_` from a copy of the element (#14495).
//!
//! `for @a { ... }` binds `$_` to the element's own container (ADR-0045), which
//! mutsu implements by promoting every element to a `ContainerRef` cell: an
//! allocation, a GC object and a later drop per element. Rakudo pays none of it
//! because array slots are `Scalar`s to begin with. The alias is observable
//! only through the topic itself or through another path to the element, so a
//! body that provably does neither can bind the plain value and skip the
//! promotion.
//!
//! The proof is an allow-list: every statement and expression of the body must
//! be one of the forms below, otherwise the loop keeps its alias. The forms are
//! the ones that cannot write `$_`, bind or capture it, call user code, or
//! reach the source array by another name:
//!
//! - scalar reads (including `$_`), literals, string interpolation;
//! - the arithmetic, comparison, string and boolean operators;
//! - plain `=` / compound assignment to, and `++`/`--` of, a scalar other than
//!   `$_`, and `my $x = ...` of an untyped, non-`state` scalar;
//! - `say` / `put` / `print` / `note`, `if` / `unless` / ternary, `next` / `last`.
//!
//! Accepted limits, all of which need the loop to mutate the very array it
//! iterates and to read `$_` afterwards in the same iteration: an operator
//! overloaded by user code, a tied (`Proxy`) scalar, a `.gist`/`.Str` that
//! mutates the source, and a scalar bound to one of the source's elements.

use crate::ast::{AssignOp, Expr, Stmt};
use crate::ast_visit::{Visit, walk_expr, walk_stmt};
use crate::token_kind::TokenKind;

/// Whether `body` (the statements of a `for` block) cannot observe whether its
/// implicit topic is the element's container or a copy of its value.
// Cost: O(n), n = AST nodes of the body.
pub(crate) fn topic_alias_unobservable(body: &[Stmt]) -> bool {
    let mut scan = AliasScan { ok: true };
    for stmt in body {
        scan.visit_stmt(stmt);
        if !scan.ok {
            return false;
        }
    }
    true
}

struct AliasScan {
    ok: bool,
}

/// A plain scalar variable name that is not the topic: the only assignment
/// target the proof admits.
fn is_plain_scalar(name: &str) -> bool {
    name != "_"
        && name
            .chars()
            .next()
            .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
        && !name.contains("::")
}

fn is_plain_scalar_var(expr: &Expr) -> bool {
    matches!(expr, Expr::Var(name) if is_plain_scalar(name))
}

fn safe_binary(op: &TokenKind) -> bool {
    matches!(
        op,
        TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::StarStar
            | TokenKind::Percent
            | TokenKind::Slash
            | TokenKind::Tilde
            | TokenKind::EqEq
            | TokenKind::BangEq
            | TokenKind::Lt
            | TokenKind::Lte
            | TokenKind::Gt
            | TokenKind::Gte
            | TokenKind::AndAnd
            | TokenKind::OrOr
            | TokenKind::SlashSlash
            | TokenKind::BitAnd
            | TokenKind::BitOr
            | TokenKind::BitXor
    )
}

fn safe_unary(op: &TokenKind) -> bool {
    matches!(
        op,
        TokenKind::Minus
            | TokenKind::Plus
            | TokenKind::Bang
            | TokenKind::Question
            | TokenKind::Tilde
            | TokenKind::MetaAssignIdentity(_)
    )
}

impl<'ast> Visit<'ast> for AliasScan {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if !self.ok {
            return;
        }
        let allowed = match stmt {
            Stmt::SetLine(..)
            | Stmt::Expr(_)
            | Stmt::Say(_)
            | Stmt::Put(_)
            | Stmt::Print(_)
            | Stmt::Note(_)
            | Stmt::Next(_)
            | Stmt::Last(_)
            | Stmt::Block(_)
            | Stmt::SyntheticBlock(_) => true,
            Stmt::Assign {
                name,
                op: AssignOp::Assign,
                target_is_sigilless: false,
                ..
            } => is_plain_scalar(name),
            Stmt::If {
                binding_var: None, ..
            } => true,
            Stmt::VarDecl {
                name,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                type_constraint: None,
                where_constraint: None,
                custom_traits,
                constant: None,
                bind: None,
                marks,
                ..
            } => is_plain_scalar(name) && custom_traits.is_empty() && marks.is_plain(),
            _ => false,
        };
        if allowed {
            walk_stmt(self, stmt);
        } else {
            self.ok = false;
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if !self.ok {
            return;
        }
        let allowed = match expr {
            Expr::Literal(_) | Expr::Var(_) | Expr::Grouped(_) | Expr::StringInterpolation(_) => {
                true
            }
            Expr::Ternary { .. } => true,
            Expr::Binary { op, .. } => safe_binary(op),
            Expr::Unary { op, expr, .. } => {
                safe_unary(op)
                    || (matches!(op, TokenKind::PlusPlus | TokenKind::MinusMinus)
                        && is_plain_scalar_var(expr))
            }
            Expr::PostfixOp { op, expr } => {
                matches!(op, TokenKind::PlusPlus | TokenKind::MinusMinus)
                    && is_plain_scalar_var(expr)
            }
            Expr::AssignExpr { name, is_bind, .. } => !*is_bind && is_plain_scalar(name),
            Expr::CompoundAssign { target, op, .. } => {
                is_plain_scalar_var(target)
                    && matches!(
                        op.as_str(),
                        "+=" | "-=" | "*=" | "/=" | "%=" | "**=" | "~=" | "//=" | "||=" | "&&="
                    )
            }
            _ => false,
        };
        if allowed {
            walk_expr(self, expr);
        } else {
            self.ok = false;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn body_of(src: &str) -> Vec<Stmt> {
        let stmts = crate::parser::parse_program(src).expect("parse").0;
        for stmt in stmts {
            if let Stmt::For { body, .. } = stmt {
                return body;
            }
        }
        panic!("no for loop in {src}");
    }

    fn unobservable(src: &str) -> bool {
        topic_alias_unobservable(&body_of(src))
    }

    #[test]
    fn pure_scalar_bodies_are_unobservable() {
        assert!(unobservable("my $n = 0; for @a { $n += $_ }"));
        assert!(unobservable(
            "my $n = 0; for @a { $n = $n + $_ * 2 if $_ > 1 }"
        ));
        assert!(unobservable("for @a { my $y = $_ + 1; say $y }"));
        assert!(unobservable("my $c = 0; for @a { $c++ }"));
    }

    #[test]
    fn topic_writes_and_escapes_keep_the_alias() {
        assert!(!unobservable("for @a { $_ = 5 }"));
        assert!(!unobservable("for @a { $_ += 1 }"));
        assert!(!unobservable("for @a { $_++ }"));
        assert!(!unobservable("my $x; for @a { $x := $_ }"));
        assert!(!unobservable("for @a { foo($_) }"));
        assert!(!unobservable("for @a { .foo }"));
        assert!(!unobservable("for @a { @a[0] = $_ }"));
        assert!(!unobservable("my @o; for @a { @o.push($_) }"));
        assert!(!unobservable("for @a { my $f = { $_ } }"));
        assert!(!unobservable("for @a { s/a/b/ }"));
    }
}
