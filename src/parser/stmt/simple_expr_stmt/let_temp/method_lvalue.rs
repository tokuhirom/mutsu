//! `temp` / `let` over a METHOD-CALL lvalue: `temp $obj.m = v`,
//! `let Type.^meta = v`, `temp @a[0].n ~= v`, `temp $.attr = v` and the bare
//! `temp $obj.m;`. Every form becomes one `Stmt::TempMethodAssign`; `let` and
//! `temp` differ only in when the save is restored.

use crate::ast::{Expr, Stmt};
use crate::parser::expr::expression;
use crate::parser::helpers::ws;
use crate::parser::parse_result::PResult;
use crate::parser::stmt::modifier::parse_statement_modifier;

/// What a method-lvalue's invocant is, for the save.
enum Invocant {
    /// A variable, or `self` (`var_name`), with whether it was the `$.attr`
    /// spelling.
    Var { var_name: String, attribute: bool },
    /// An element (`@a[0].n`): the object it holds is what is saved on.
    Element(Expr),
    /// A type object (`Type.^meta`), named by its type.
    Type(String),
}

impl Invocant {
    // Cost: O(1), plus a clone of an element invocant.
    fn of(invocant: &Expr) -> Option<Self> {
        if let Some(var_name) = temp_method_invocant_name(invocant) {
            let attribute = matches!(invocant, Expr::Var(name) if name == "__ANON_STATE#");
            return Some(Invocant::Var {
                var_name,
                attribute,
            });
        }
        if matches!(invocant, Expr::Index { .. }) {
            return Some(Invocant::Element(invocant.clone()));
        }
        type_object_invocant_name(invocant)
            .filter(|name| *name != "self")
            .map(|name| Invocant::Type(name.to_string()))
    }

    // Cost: O(n), n = nodes in the method arguments and the value.
    fn temporize(
        self,
        method_name: String,
        method_args: Vec<Expr>,
        value: Option<Expr>,
        is_temp: bool,
    ) -> Stmt {
        let (var_name, is_attribute, type_invocant, invocant_expr) = match self {
            Invocant::Var {
                var_name,
                attribute,
            } => (var_name, attribute, false, None),
            Invocant::Element(e) => (String::new(), false, false, Some(Box::new(e))),
            Invocant::Type(name) => (name, false, true, None),
        };
        Stmt::TempMethodAssign {
            var_name,
            method_name,
            method_args,
            value,
            is_let: !is_temp,
            is_attribute,
            type_invocant,
            invocant_expr,
        }
    }
}

/// The type a `temp Type.method …` names: a bare name, or one an import spelled.
/// A unit that imported through a `sub EXPORT` hook parses every bareword as
/// an imported term (`ParsedExpr::ImportedTerm`), so `temp Bla.^table = …`
/// after `use Red` names its type that way (#18377).
fn type_object_invocant_name(invocant: &Expr) -> Option<&str> {
    let invocant = match invocant {
        Expr::Spelled(spelled) => &spelled.expr,
        other => other,
    };
    match invocant {
        Expr::BareWord(name) => Some(name),
        Expr::Ast(node) => match node.as_ref() {
            crate::ast::parsed::ParsedExpr::ImportedTerm(name) => Some(name),
            _ => None,
        },
        _ => None,
    }
}

/// The variable a `temp <invocant>.method …` saves: a plain scalar `$obj`, or
/// `self` — spelled `self` or as the `$.attr` invocant carrier, which the
/// parser leaves as the collapsed anonymous-state name `__ANON_STATE#`.
fn temp_method_invocant_name(invocant: &Expr) -> Option<String> {
    match invocant {
        Expr::Var(name) if name == "__ANON_STATE#" => Some("self".to_string()),
        Expr::Var(name) => Some(name.clone()),
        Expr::BareWord(name) if name == "self" => Some("self".to_string()),
        _ => None,
    }
}

/// The method-lvalue `temp`/`let` `expr` spells, `expr_rest` being the input
/// after it. `None` when `expr` is no method-call lvalue.
// Cost: O(n), n = size of the lvalue and the assigned expression.
pub(super) fn method_lvalue_temporize<'a>(
    expr: &Expr,
    expr_rest: &'a str,
    is_temp: bool,
) -> Option<PResult<'a, Stmt>> {
    // `temp $obj.method = value`: the expression parser has already lowered
    // `$obj.method = value` to the `__mutsu_assign_method_lvalue` writeback
    // call (an rw-accessor assignment is a full expression). Recover the
    // pieces to save/restore the target.
    if let Some(parts) = crate::rakuast::method_lvalue_call_parts(expr)
        && let Expr::Call { args, .. } = expr
        && args.len() == 5
        && let Some(invocant) = Invocant::of(parts.invocant)
    {
        return Some(parse_statement_modifier(
            expr_rest,
            invocant.temporize(
                parts.method_name.to_string(),
                parts.method_args.to_vec(),
                Some(parts.value.clone()),
                is_temp,
            ),
        ));
    }
    // A compound assignment through a method lvalue: `temp $obj.indent ~= '
    // '`, `temp $.indent ~= '  '` (CSS::Writer). The expanded writeback call
    // already carries the combined value (`$obj.indent ~ '  '`).
    if let Expr::CompoundAssign {
        target, expanded, ..
    } = expr
        && let Expr::MethodCall {
            target: invocant,
            name: method_name,
            args: method_args,
            ..
        } = target.as_ref()
        && let Some(invocant) = Invocant::of(invocant)
        && let Some(parts) = crate::rakuast::method_lvalue_call_parts(expanded)
    {
        return Some(parse_statement_modifier(
            expr_rest,
            invocant.temporize(
                method_name.resolve(),
                method_args.clone(),
                Some(parts.value.clone()),
                is_temp,
            ),
        ));
    }
    // `temp $.attr = value` / `temp $.attr ~= value`: the `$.attr` lvalue is
    // `self.attr`, so it is the method-lvalue form on `self`. The compound
    // form's expansion is the plain assignment of the combined value.
    let plain_assign = match expr {
        Expr::CompoundAssign { expanded, .. } => expanded.as_ref(),
        other => other,
    };
    if let Expr::AssignExpr {
        name,
        expr: value,
        is_bind: false,
    } = plain_assign
        && let Some(method_name) = name.strip_prefix('.')
        && method_name.starts_with(crate::parser::helpers::is_raku_identifier_start)
    {
        let invocant = Invocant::Var {
            var_name: "self".to_string(),
            attribute: true,
        };
        return Some(parse_statement_modifier(
            expr_rest,
            invocant.temporize(
                method_name.to_string(),
                Vec::new(),
                Some((**value).clone()),
                is_temp,
            ),
        ));
    }
    // The method call alone: `temp $obj.method = value` the expression parser
    // left unassigned, or a bare `temp Type.method;`, which saves without
    // assigning (#18392).
    let Expr::MethodCall {
        target,
        name,
        args,
        modifier,
        ..
    } = expr
    else {
        return None;
    };
    let invocant = Invocant::of(target)?;
    let method_name = match modifier {
        Some('^') if matches!(invocant, Invocant::Type(_)) => format!("^{}", name.resolve()),
        None => name.resolve(),
        Some(_) => return None,
    };
    Some((|| {
        let (rest_ws, _) = ws(expr_rest)?;
        if rest_ws.starts_with('=') && !rest_ws.starts_with("==") {
            let (rhs_rest, _) = ws(&rest_ws[1..])?;
            let (rhs_rest, rhs_expr) = expression(rhs_rest)?;
            return parse_statement_modifier(
                rhs_rest,
                invocant.temporize(method_name, args.clone(), Some(rhs_expr), is_temp),
            );
        }
        parse_statement_modifier(
            expr_rest,
            invocant.temporize(method_name, args.clone(), None, is_temp),
        )
    })())
}
