//! Whether a `with` / `without` can tell an aliased `$_` from a copy of its
//! topic's value.
//!
//! `with $x { ... }` / `X without $h<k>` bind `$_` to the topic's own
//! container, so the body can write the source through it. The compiler tags
//! the source (`TagContainerRef` / `TagElementSource`) for that. The tag also
//! moves the source variable off the env-free declaration lane: its
//! `my $x = ...` keeps its name-keyed bookkeeping and takes the general store.
//! MCP::Server::Tool::Web's memo helper does this on every call:
//!
//! ```raku
//! my $known = %memo{$needle};
//! if $known.defined && $known[0] <= $from {
//!     return Int without $known[1];
//! ```
//!
//! The alias is observable only through `$_`. The parser desugars `with` /
//! `without` to a `Given` whose body is one `if` testing `$_.defined`. When
//! the branches of that `if` never name the topic, nothing in the body can
//! observe whether `$_` is the source container. The topic test reads the
//! value before any branch runs.
//!
//! The proof is an allow-list, as `for_topic_alias.rs`'s is: the branches may
//! only return, die or break out of a loop with literals, type names and plain
//! variable and element reads, combined by plain operators. Anything else (a
//! call, which could reach `$_` through `CALLER::`, a regex that matches
//! against `$_`, a block, a `when`) keeps the tag.

use crate::ast::{Expr, Stmt};
use crate::ast_visit::{Visit, walk_expr, walk_stmt};
use crate::token_kind::TokenKind;

/// Whether `body`, the body of a `with` / `without` (`Stmt::Given` with a
/// `with_kind`), cannot observe that its `$_` aliases the topic's container.
// Cost: O(n), n = AST nodes of the body.
pub(crate) fn with_topic_alias_unobservable(body: &[Stmt]) -> bool {
    let mut stmts = body.iter().filter(|s| !matches!(s, Stmt::SetLine(..)));
    let (
        Some(Stmt::If {
            cond,
            then_branch,
            else_branch,
            binding_var: None,
            ..
        }),
        None,
    ) = (stmts.next(), stmts.next())
    else {
        return false;
    };
    if !is_topic_defined_test(cond) {
        return false;
    }
    let mut scan = BranchScan { ok: true };
    for stmt in then_branch.iter().chain(else_branch.iter()) {
        scan.visit_stmt(stmt);
        if !scan.ok {
            return false;
        }
    }
    true
}

/// `$_.defined` or `!$_.defined`: the test the parser builds for `with` /
/// `without`.
fn is_topic_defined_test(cond: &Expr) -> bool {
    let cond = match cond {
        Expr::Unary {
            op: TokenKind::Bang,
            expr,
            ..
        } => expr.as_ref(),
        other => other,
    };
    matches!(cond, Expr::MethodCall { target, name, args, modifier: None, .. }
        if matches!(target.as_ref(), Expr::Var(t) if t == "_")
            && args.is_empty()
            && name.with_str(|n| n == "defined"))
}

struct BranchScan {
    ok: bool,
}

/// A variable read that is not the topic (nor an attribute, whose read is a
/// method call on `self`).
fn plain_var(name: &str) -> bool {
    name != "_" && crate::value::attr_twigil_base(name).is_none()
}

impl<'ast> Visit<'ast> for BranchScan {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if !self.ok {
            return;
        }
        let allowed = matches!(
            stmt,
            Stmt::SetLine(..)
                | Stmt::Return(_)
                | Stmt::Die(_)
                | Stmt::Next(_)
                | Stmt::Last(_)
                | Stmt::Expr(_)
        );
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
            Expr::Literal(_) | Expr::Grouped(_) => true,
            // A type name (`return Int without ...`); a lower-case bare word
            // may be a call of user code.
            Expr::BareWord(name) => name.starts_with(|c: char| c.is_ascii_uppercase()),
            Expr::Var(name) => plain_var(name),
            Expr::ArrayVar(_) | Expr::HashVar(_) => true,
            Expr::Index { .. } => true,
            Expr::Binary { op, .. } => matches!(
                op,
                TokenKind::Plus
                    | TokenKind::Minus
                    | TokenKind::Star
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
            ),
            Expr::Unary { op, .. } => matches!(op, TokenKind::Minus | TokenKind::Bang),
            _ => false,
        };
        if allowed {
            walk_expr(self, expr);
        } else {
            self.ok = false;
        }
    }
}
