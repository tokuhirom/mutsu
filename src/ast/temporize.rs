//! `temp` / `let` as a prefix operator over an lvalue.
//!
//! Rakudo has `temp LVALUE` as an `ApplyPrefix` and the assignment as the
//! `ApplyInfix` around it. The parser keeps that shape
//! (`ParsedStmt::Temporize`), and [`expand`] is its one lowering to what the
//! compiler runs: a [`Stmt::Let`] that saves the variable (or one element of
//! it) and, when a plain assignment follows, assigns inside the same node. A
//! compound assignment (`temp $x ~= "a"`) cannot be spelled in one
//! `Stmt::Let`, so it is a [`Stmt::SyntheticBlock`] of the save and the
//! ordinary statement. A declaration (`temp my $x`) is kept as written by the
//! parser (`ParsedStmt::TempDecl`) and lowered to the declaration plus its save.

use super::{Expr, Stmt};

/// What a `temp` / `let` saves.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum Target {
    /// A variable, by its environment key (`x`, `@a`, `%h`, `*CWD`, `!attr`).
    Variable(String),
    /// `temp undefine($x) = VALUE`: the variable is undefined first, then
    /// saved in that state.
    Undefine(String),
    /// An element of a variable or a deeper subscript, as the lvalue
    /// expression (`@a[1]`, `%h<k>`, `$s[1]<k>`).
    Element(Expr),
}

/// The assignment that follows the prefix, if any.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum Assigned {
    No,
    /// `= VALUE`.
    Plain(Expr),
    /// `OP= VALUE`, as the parser's marked `CompoundAssign` expression.
    Compound(Expr),
}

/// A `temp` / `let` statement: the prefix over its lvalue and what follows.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct Temporized {
    pub(crate) is_temp: bool,
    pub(crate) target: Target,
    pub(crate) assigned: Assigned,
}

/// The expression for the container the environment key `key` names.
// Cost: O(|key|).
pub(crate) fn variable_expr(key: &str) -> Expr {
    if let Some(rest) = key.strip_prefix('@') {
        Expr::ArrayVar(rest.to_string())
    } else if let Some(rest) = key.strip_prefix('%') {
        Expr::HashVar(rest.to_string())
    } else {
        Expr::Var(key.to_string())
    }
}

/// The element lvalue `name[index]` / `name<index>` a single-level save names.
/// A positional (array) subscript is told by the container's sigil, or by an
/// index that is not a string literal; the parser keeps the index only.
// Cost: O(n), n = size of the index (it is cloned).
pub(crate) fn element_expr(name: &str, index: &Expr) -> Expr {
    let is_positional =
        name.starts_with('@') || !matches!(index, Expr::Literal(v) if v.as_str().is_some());
    Expr::Index {
        target: Box::new(variable_expr(name)),
        index: Box::new(index.clone()),
        is_positional,
        spelling: Default::default(),
    }
}

/// The `Stmt::Let` a `temp` / `let` over the variable (or element) `name`
/// parses to, with an optional assigned value.
pub(crate) fn save(name: &str, index: Option<&Expr>, value: Option<Expr>, is_temp: bool) -> Stmt {
    Stmt::Let {
        name: name.to_string(),
        index: index.map(|i| Box::new(i.clone())),
        value: value.map(Box::new),
        is_temp,
        undefine_first: false,
        nested_lvalue: false,
    }
}

/// The `Stmt::Let` a `temp` over a compound element (`temp $s[1]<k> = v`,
/// `temp (@a)[0]`) parses to: `value` is the whole element assignment, or the
/// bare element, and `name` is the base variable.
pub(crate) fn save_nested(name: &str, value: Expr, is_temp: bool) -> Stmt {
    Stmt::Let {
        name: name.to_string(),
        index: None,
        value: Some(Box::new(value)),
        is_temp,
        undefine_first: false,
        nested_lvalue: true,
    }
}

/// The base variable's environment key under any depth of subscripts and
/// parentheses (`temp (@a)[0]` and `temp $s[1]<k>` name elements of `@a` and
/// `$s`), or `None` when the base is not a plain variable.
// Cost: O(d + |name|), d = subscript and paren depth.
pub(crate) fn base_name(expr: &Expr) -> Option<String> {
    let mut expr = expr;
    loop {
        match expr {
            Expr::Index { target, .. } | Expr::Grouped(target) => expr = target,
            other => return other.container_var_key(),
        }
    }
}

/// Whether the element is a subscript of a plain variable (`@a[1]`, `%h<k>`).
// Cost: O(1).
fn single_level(element: &Expr) -> bool {
    matches!(
        element,
        Expr::Index { target, .. }
            if matches!(target.as_ref(), Expr::Var(_) | Expr::ArrayVar(_) | Expr::HashVar(_))
    )
}

/// The statement the compiler runs for a `temp` / `let`: the one lowering of
/// `ParsedStmt::Temporize`. `None` when the lvalue names no variable.
// Cost: O(n), n = size of the lvalue and the assigned expression.
pub(crate) fn expand(t: Temporized) -> Option<Stmt> {
    let Temporized {
        is_temp,
        target,
        assigned,
    } = t;
    let value = match &assigned {
        Assigned::Plain(value) => Some(value.clone()),
        _ => None,
    };
    let (stmt, name, index) = match target {
        Target::Undefine(name) => {
            return Some(Stmt::Let {
                name,
                index: None,
                value: value.map(Box::new),
                is_temp: true,
                undefine_first: true,
                nested_lvalue: false,
            });
        }
        Target::Variable(name) => (None, name, None),
        Target::Element(element) => {
            let Expr::Index {
                target,
                index,
                is_positional,
                ..
            } = &element
            else {
                return None;
            };
            let name = base_name(target)?;
            if single_level(&element) {
                (None, name, Some(index.as_ref().clone()))
            } else {
                // A deeper subscript or a parenthesised container: the
                // element assignment (or the bare element) rides in the save,
                // so a compound assignment cannot.
                if matches!(assigned, Assigned::Compound(_)) {
                    return None;
                }
                let carried = match value {
                    Some(value) => Expr::IndexAssign {
                        target: target.clone(),
                        index: index.clone(),
                        value: Box::new(value),
                        is_positional: *is_positional,
                        spelling: Default::default(),
                    },
                    None => element.clone(),
                };
                (Some(save_nested(&name, carried, is_temp)), name, None)
            }
        }
    };
    if let Some(nested) = stmt {
        return Some(nested);
    }
    Some(match assigned {
        // `temp $x ~= "a"`: the save, then the assignment parsed from the
        // variable on.
        Assigned::Compound(assignment) => Stmt::SyntheticBlock(vec![
            save(&name, index.as_ref(), None, is_temp),
            Stmt::Expr(assignment),
        ]),
        Assigned::Plain(value) => save(&name, index.as_ref(), Some(value), is_temp),
        Assigned::No => save(&name, index.as_ref(), None, is_temp),
    })
}
