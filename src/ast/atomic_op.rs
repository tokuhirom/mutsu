//! The atomic operators (`⚛$x`, `$x ⚛= 5`, `$x⚛++`, `++⚛$x`, `$x ⚛+= 2`).
//!
//! Rakudo has no node of their own: `⚛$x` is `ApplyPrefix(Prefix "⚛")`,
//! `$x ⚛= 5` an `ApplyInfix(Infix "⚛=")`, `$x⚛++` an `ApplyPostfix(Postfix
//! "⚛++")`. The parser spells the fetch and the plain store as the reserved
//! calls below, which name the variable by a string, and the others as calls
//! named for the operator (`postfix:<⚛++>`, `prefix:<++⚛>`, `infix:<⚛+=>`)
//! over the variable itself. The builders are the parser's and the RakuAST
//! lowering's; [`recognize`] is the converter's.

use super::Expr;
use crate::symbol::Symbol;
use crate::value::Value;

const FETCH_VAR: &str = "__mutsu_atomic_fetch_var";
const STORE_VAR: &str = "__mutsu_atomic_store_var";

/// `⚛$x`: the atomic fetch of the variable named `name`.
// Cost: O(|name|).
pub(crate) fn fetch_var(name: String) -> Expr {
    Expr::Call {
        name: Symbol::intern(FETCH_VAR),
        args: vec![Expr::Literal(Value::str(name))],
    }
}

/// `$x ⚛= VALUE`: the atomic store into the variable named `name`.
// Cost: O(|name|).
pub(crate) fn store_var(name: String, value: Expr) -> Expr {
    Expr::Call {
        name: Symbol::intern(STORE_VAR),
        args: vec![Expr::Literal(Value::str(name)), value],
    }
}

/// An operator-named call over the target: `postfix:<⚛++>`, `prefix:<++⚛>`,
/// `infix:<⚛+=>`.
// Cost: O(|operator|).
pub(crate) fn operator_call(category: Category, operator: &str, args: Vec<Expr>) -> Expr {
    Expr::Call {
        name: Symbol::intern(&format!("{}:<{operator}>", category.word())),
        args,
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum Category {
    Prefix,
    Postfix,
    Infix,
}

impl Category {
    // Cost: O(1).
    pub(crate) fn word(self) -> &'static str {
        match self {
            Category::Prefix => "prefix",
            Category::Postfix => "postfix",
            Category::Infix => "infix",
        }
    }
}

/// An atomic operator application, read back from the parser's call.
pub(crate) enum Atomic<'a> {
    /// `⚛$x`.
    Fetch(&'a str),
    /// `$x ⚛= VALUE`.
    Store(&'a str, &'a Expr),
    /// `⚛OP` forms over one operand: `$x⚛++`, `++⚛$x`, `⚛@a[0]`.
    Unary {
        category: Category,
        operator: &'a str,
        operand: &'a Expr,
    },
    /// `$x ⚛+= 2`.
    Binary {
        operator: &'a str,
        left: &'a Expr,
        right: &'a Expr,
    },
}

/// The atomic operator the call `name(args)` is, if it is one.
// Cost: O(|name|).
pub(crate) fn recognize<'a>(name: &'a str, args: &'a [Expr]) -> Option<Atomic<'a>> {
    match (name, args) {
        (FETCH_VAR, [Expr::Literal(v)]) => Some(Atomic::Fetch(v.as_str()?)),
        (STORE_VAR, [Expr::Literal(v), value]) => Some(Atomic::Store(v.as_str()?, value)),
        _ => {
            let (category, rest) = [Category::Prefix, Category::Postfix, Category::Infix]
                .into_iter()
                .find_map(|c| {
                    name.strip_prefix(c.word())
                        .and_then(|r| r.strip_prefix(":<"))
                        .and_then(|r| r.strip_suffix('>'))
                        .map(|r| (c, r))
                })?;
            if !rest.contains('⚛') {
                return None;
            }
            match (category, args) {
                (Category::Infix, [left, right]) => Some(Atomic::Binary {
                    operator: rest,
                    left,
                    right,
                }),
                (Category::Infix, _) => None,
                (category, [operand]) => Some(Atomic::Unary {
                    category,
                    operator: rest,
                    operand,
                }),
                _ => None,
            }
        }
    }
}
