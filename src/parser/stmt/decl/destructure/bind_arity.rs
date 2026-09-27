//! The signature side of a positional destructuring bind
//! (`my ($a, $b?, *@r) := RHS`): the arity guard and the defaults of
//! optional elements the RHS does not reach.

use super::{DestructureVar, native_type_default};
use crate::ast::{Expr, Stmt};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

/// `@<staged>.elems` -- the positional count of a destructuring bind's RHS.
pub(super) fn staged_elems(array_bare: &str) -> Expr {
    Expr::MethodCall {
        target: Box::new(Expr::ArrayVar(array_bare.to_string())),
        name: Symbol::intern("elems"),
        args: Vec::new(),
        modifier: None,
        quoted: false,
    }
}

/// `@<staged>.EXISTS-POS(i)` -- whether the RHS reaches position `i`, without
/// forcing a lazy RHS any further than that (`my ($a, *@r) := 1..*`).
pub(super) fn staged_exists(array_bare: &str, i: usize) -> Expr {
    Expr::MethodCall {
        target: Box::new(Expr::ArrayVar(array_bare.to_string())),
        name: Symbol::intern("EXISTS-POS"),
        args: vec![Expr::Literal(Value::int(i as i64))],
        modifier: None,
        quoted: false,
    }
}

/// The value an unfilled optional parameter without a default gets: a native
/// type's zero value, else the constraint's type object (`Mu` when untyped),
/// as rakudo gives `my ($x, Int $y?) := (1,)` an `Int` and `$y?` a `Mu`.
pub(super) fn optional_param_default(tc: &Option<String>) -> Expr {
    let Some(t) = tc.as_deref() else {
        return Expr::BareWord("Mu".to_string());
    };
    if crate::runtime::native_types::is_native_int_type(t)
        || matches!(t, "num" | "num32" | "num64" | "str")
    {
        return native_type_default(tc);
    }
    // A definiteness smiley does not change the type object (`Int:D $y?`
    // still defaults to `Int`); anything that is not a plain (qualified)
    // type name, such as a parameterization, falls back to `Mu`.
    let base = t
        .strip_suffix(":D")
        .or_else(|| t.strip_suffix(":U"))
        .or_else(|| t.strip_suffix(":_"))
        .unwrap_or(t);
    if !base.is_empty()
        && base
            .chars()
            .all(|c| c.is_alphanumeric() || c == '_' || c == '-' || c == ':')
    {
        Expr::BareWord(base.to_string())
    } else {
        Expr::BareWord("Mu".to_string())
    }
}

/// Emit the arity guard of a positional destructuring bind
/// (`my ($a, $b?, *@r) := RHS`): die with rakudo's "Too few/Too many
/// positionals passed" wording when the staged RHS has fewer elements than the
/// required ones or, without a slurpy, more than every positional can take.
pub(super) fn push_bind_arity_check(
    stmts: &mut Vec<Stmt>,
    vars: &[DestructureVar],
    array_bare: &str,
) {
    let has_slurpy = vars.iter().any(|v| v.is_slurpy);
    let positional = vars.iter().filter(|v| !v.is_slurpy);
    let total = positional.clone().count();
    let required = positional
        .filter(|v| !v.is_optional && v.default.is_none())
        .count();
    let plural = |n: usize| if n == 1 { "" } else { "s" };
    let expected = if required == total {
        format!("expected {total} argument{} but got ", plural(total))
    } else if total == required + 1 {
        format!("expected {required} or {total} arguments but got ")
    } else {
        format!("expected {required} to {total} arguments but got ")
    };
    // Probe one position rather than counting: a lazy RHS feeding a slurpy
    // must not be reified just to check its lower bound. The count is only
    // taken for the message, once the bind is already failing.
    let mut guard = |too_few: bool, probe: usize, msg: String| {
        let exists = staged_exists(array_bare, probe);
        stmts.push(Stmt::If {
            // Too few: position `required - 1` is missing; too many:
            // position `total` is present.
            cond: if too_few {
                Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(exists),
                }
            } else {
                exists
            },
            then_branch: vec![Stmt::Die(Expr::Binary {
                left: Box::new(Expr::Literal(Value::str(msg))),
                op: TokenKind::Tilde,
                right: Box::new(staged_elems(array_bare)),
            })],
            else_branch: Vec::new(),
            binding_var: None,
            is_statement_modifier: false,
            is_unless: false,
            with_kind: None,
        });
    };
    if required > 0 {
        let msg = if has_slurpy {
            format!(
                "Too few positionals passed; expected at least {required} argument{} but got only ",
                plural(required)
            )
        } else {
            format!("Too few positionals passed; {expected}")
        };
        guard(true, required - 1, msg);
    }
    if !has_slurpy {
        guard(
            false,
            total,
            format!("Too many positionals passed; {expected}"),
        );
    }
}
