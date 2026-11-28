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
//! - `say` / `put` / `print` / `note`, `if` / `unless` / ternary, `next` / `last`;
//! - an associative store into a `%` variable, `%h{$k} = $v`: a hash element
//!   is never an array element, so the store cannot reach the source array,
//!   and the stored value is a copy of what the operands evaluate to.
//! - a read of a `%` variable's element, `%h{$k}`, and `++`/`--` of one
//!   (`%h{$w}++`), for the same reason.
//!
//! Accepted limits, all of which need the loop to mutate the very array it
//! iterates and to read `$_` afterwards in the same iteration: an operator
//! overloaded by user code, a tied (`Proxy`) scalar, a `.gist`/`.Str` that
//! mutates the source, a scalar bound to one of the source's elements, and a
//! `%` variable bound to a user class whose `ASSIGN-KEY` mutates the source.

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

/// Whether `body`, the statements of a `for ... -> $param` block, cannot
/// observe how its parameter `$param` (`param` without its sigil) is bound:
/// the topic's allow-list, and no write to the parameter itself (which is
/// readonly, so the write would have to raise). The VM may then bind only the
/// parameter's slot per item (`ForLoopSpec::param_slot_only`).
// Cost: O(n), n = AST nodes of the body.
pub(crate) fn param_alias_unobservable(body: &[Stmt], param: &str) -> bool {
    let mut scan = AliasScan { ok: true };
    let mut writes = ParamWrites { param, found: false };
    for stmt in body {
        scan.visit_stmt(stmt);
        writes.visit_stmt(stmt);
        if !scan.ok || writes.found {
            return false;
        }
    }
    true
}

/// Finds a write to one scalar name among the forms the allow-list admits:
/// `=`, a compound assignment, `++`/`--`, or a `my` of the same name (which
/// would shadow it with a slot of its own).
struct ParamWrites<'p> {
    param: &'p str,
    found: bool,
}

impl<'ast> Visit<'ast> for ParamWrites<'_> {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        match stmt {
            Stmt::Assign { name, .. } | Stmt::VarDecl { name, .. } if name == self.param => {
                self.found = true;
            }
            _ => walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        let target_is_param = |e: &Expr| matches!(e, Expr::Var(n) if n == self.param);
        match expr {
            Expr::AssignExpr { name, .. } if name == self.param => self.found = true,
            Expr::CompoundAssign { target, .. } if target_is_param(target) => self.found = true,
            Expr::Unary { op, expr: e, .. } | Expr::PostfixOp { op, expr: e }
                if matches!(op, TokenKind::PlusPlus | TokenKind::MinusMinus)
                    && target_is_param(e) =>
            {
                self.found = true
            }
            _ => walk_expr(self, expr),
        }
    }
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

/// `%h{$k}`: an element of a `%` variable. Incrementing one (`%h{$w}++`)
/// writes a hash element, which is never an element of the source array --
/// the same reasoning that admits `%h{$k} = $v`.
fn is_hash_element(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::Index {
            target,
            is_positional: false,
            ..
        } if matches!(**target, Expr::HashVar(_))
    )
}

fn safe_binary(op: &TokenKind) -> bool {
    matches!(
        op,
        TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::StarStar
            | TokenKind::Percent
            | TokenKind::PercentPercent
            | TokenKind::BangPercentPercent
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
            Expr::Literal(_)
            | Expr::Var(_)
            | Expr::HashVar(_)
            | Expr::Grouped(_)
            | Expr::StringInterpolation(_) => true,
            Expr::IndexAssign {
                target,
                is_positional: false,
                ..
            } => matches!(**target, Expr::HashVar(_)),
            Expr::Ternary { .. } => true,
            // Reading `%h{$k}` (and the element a `%h{$k}++` writes): a hash
            // element is never an element of the source array.
            expr if is_hash_element(expr) => true,
            Expr::Binary { op, .. } => safe_binary(op),
            Expr::Unary { op, expr, .. } => {
                safe_unary(op)
                    || (matches!(op, TokenKind::PlusPlus | TokenKind::MinusMinus)
                        && (is_plain_scalar_var(expr) || is_hash_element(expr)))
            }
            Expr::PostfixOp { op, expr } => {
                matches!(op, TokenKind::PlusPlus | TokenKind::MinusMinus)
                    && (is_plain_scalar_var(expr) || is_hash_element(expr))
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
        assert!(unobservable("my %h; for @a { %h{$_} = 1 }"));
        assert!(unobservable("my %h; for @a { %h{$_ ~ 'x'} = $_ + 1 }"));
        assert!(unobservable("my $n = 0; for @a { $n++ if $_ %% 3 }"));
        assert!(unobservable("my $n = 0; for @a { $n++ if $_ !%% 3 }"));
    }

    #[test]
    fn hash_element_increments_are_unobservable() {
        assert!(unobservable("my %h; for @a { %h{$_}++ }"));
        assert!(unobservable("my %h; for @a { --%h{$_} }"));
        assert!(unobservable("my %h; my $n = 0; for @a { $n += %h{$_} }"));
        assert!(!unobservable("my @o; for @a { @o[$_]++ }"));
    }

    fn param_unobservable(src: &str, param: &str) -> bool {
        param_alias_unobservable(&body_of(src), param)
    }

    #[test]
    fn named_param_bodies() {
        assert!(param_unobservable("my %c; for @a -> $w { %c{$w}++ }", "w"));
        assert!(param_unobservable("my $n = 0; for @a -> $w { $n += $w }", "w"));
        assert!(!param_unobservable("for @a -> $w { $w = 1 }", "w"));
        assert!(!param_unobservable("for @a -> $w { $w++ }", "w"));
        assert!(!param_unobservable("for @a -> $w { my $w = 1 }", "w"));
        assert!(!param_unobservable("for @a -> $w { foo($w) }", "w"));
        assert!(!param_unobservable("for @a -> $w { my $f = { $w } }", "w"));
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
        assert!(!unobservable("my %h; for @a { %h{$_} = 1; $_ = 5 }"));
        assert!(!unobservable("my %h; for @a { %h{$_} = foo($_) }"));
        assert!(!unobservable("my %h; for @a { %h{$_}[0] = 1 }"));
        assert!(!unobservable("my %h; for @a { %h{$_} := $_ }"));
        assert!(!unobservable("my @o; for @a { @o[0] = $_ }"));
    }
}
