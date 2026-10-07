//! `temp` / `let` as a prefix operator over an lvalue.
//!
//! The parser (`parser::stmt::simple_expr_stmt::let_temp`) turns the prefix
//! into a [`Stmt::Let`] that saves the variable (or one element of it) and,
//! when an assignment follows, assigns inside the same node. A compound
//! assignment (`temp $x ~= "a"`) and a declaration (`temp my $x = 1`) cannot be
//! spelled in one `Stmt::Let`, so they are a [`Stmt::SyntheticBlock`] of the
//! save and the ordinary statement.
//!
//! Rakudo has none of that: `temp LVALUE` is an `ApplyPrefix` and the
//! assignment is the `ApplyInfix` around it. [`recognize`] reads the parser's
//! forms back as that one shape, for the RakuAST converter, and
//! [`variable_expr`] / [`element_expr`] rebuild the lvalue.

use super::{Expr, Stmt};

/// What a `temp` / `let` saves.
pub(crate) enum Target {
    /// A variable, by its environment key (`x`, `@a`, `%h`, `*CWD`, `!attr`).
    Variable(String),
    /// A declaration: `temp my $x = 1`.
    Declaration(Box<Stmt>),
    /// An element of a variable or a deeper subscript, as the lvalue
    /// expression (`@a[1]`, `%h<k>`, `$s[1]<k>`).
    Element(Expr),
}

/// The assignment that follows the prefix, if any.
pub(crate) enum Assigned {
    No,
    /// `= VALUE`.
    Plain(Expr),
    /// `OP= VALUE`, as the parser's marked `CompoundAssign` expression.
    Compound(Expr),
}

/// A `temp` / `let` statement read back as the prefix over its lvalue.
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

/// The save of one variable or element with no assignment, as the first half
/// of a compound-assignment block.
fn bare_save(stmt: &Stmt) -> Option<(&str, Option<&Expr>, bool)> {
    match stmt {
        Stmt::Let {
            name,
            index,
            value: None,
            is_temp,
            undefine_first: false,
            nested_lvalue: false,
        } => Some((name, index.as_deref(), *is_temp)),
        _ => None,
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

/// The `temp` / `let` that `stmt` is the parser's expansion of, if it is one.
// Cost: O(n), n = size of the statement (its parts are cloned).
pub(crate) fn recognize(stmt: &Stmt) -> Option<Temporized> {
    match stmt {
        Stmt::Let {
            name,
            index,
            value,
            is_temp,
            undefine_first: false,
            nested_lvalue,
        } => {
            let target;
            let assigned;
            if *nested_lvalue {
                // The value is the whole element assignment, or the element.
                match value.as_deref()? {
                    Expr::IndexAssign {
                        target: container,
                        index: key,
                        value: assigned_value,
                        is_positional,
                    } => {
                        target = Target::Element(Expr::Index {
                            target: container.clone(),
                            index: key.clone(),
                            is_positional: *is_positional,
                            spelling: Default::default(),
                        });
                        assigned = Assigned::Plain((**assigned_value).clone());
                    }
                    element @ Expr::Index { .. } => {
                        target = Target::Element(element.clone());
                        assigned = Assigned::No;
                    }
                    _ => return None,
                }
            } else {
                target = match index {
                    Some(index) => Target::Element(element_expr(name, index)),
                    None => Target::Variable(name.clone()),
                };
                assigned = match value {
                    Some(value) => Assigned::Plain((**value).clone()),
                    None => Assigned::No,
                };
            }
            Some(Temporized {
                is_temp: *is_temp,
                target,
                assigned,
            })
        }
        Stmt::SyntheticBlock(parts) => match parts.as_slice() {
            // `temp $x ~= "a"`: the save, then the assignment parsed from the
            // variable on.
            [save, Stmt::Expr(assignment @ Expr::CompoundAssign { .. })] => {
                let (name, index, is_temp) = bare_save(save)?;
                Some(Temporized {
                    is_temp,
                    target: match index {
                        Some(index) => Target::Element(element_expr(name, index)),
                        None => Target::Variable(name.to_string()),
                    },
                    assigned: Assigned::Compound(assignment.clone()),
                })
            }
            // `temp my $x = 1`: the declaration, then a bare `temp` of it.
            [
                decl @ Stmt::VarDecl { name, .. },
                Stmt::Let {
                    name: saved,
                    index: None,
                    value: None,
                    is_temp: true,
                    undefine_first: false,
                    nested_lvalue: false,
                },
            ] if name == saved => Some(Temporized {
                is_temp: true,
                target: Target::Declaration(Box::new(decl.clone())),
                assigned: Assigned::No,
            }),
            _ => None,
        },
        _ => None,
    }
}
