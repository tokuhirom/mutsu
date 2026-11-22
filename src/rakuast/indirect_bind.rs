//! A bind through a run-time name, [`Expr::IndirectSymbolBind`], across the
//! RakuAST boundary. Measured against rakudo 2026.11:
//!
//! ```text
//! $::($n)::A := 5   ApplyInfix(Var::Package(Name(EmptyEdge, Expression($n), Simple("A")), "$"),
//!                              Infix(":="), 5)
//! ```
//!
//! (rakudo's compiler then refuses the type form `::($n)::A := 5` with
//! "Cannot bind to pseudo-package", and the variable form at run time with
//! "This case of binding is not yet implemented"; roast's `S02-names/pseudo`
//! files specify both as binding into the named package's stash.)

use super::lower::{lower_expr, named_child};
use super::name_parts::{self, NameShape};
use super::view::{node_field, plain_infix, view_expr};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::Expr;
use crate::scope::NameScope;
use crate::value::{RuntimeError, Value};

/// `TARGET := value` as an [`Expr::IndirectSymbolBind`] when `TARGET` is a
/// name computed at run time (`$::($p)::A::x`, `::($p)::A`, `::($p)`); any
/// other target is handed back with the value.
// Cost: O(1).
pub(crate) fn indirect_symbol_bind(target: Expr, value: Expr) -> Result<Expr, Box<(Expr, Expr)>> {
    let (sigil, expr) = match target {
        Expr::SymbolicDeref { sigil, expr } => (sigil, expr),
        Expr::IndirectTypeLookup(expr) => (String::new(), expr),
        // `::($p)::A` names the same symbol as `::($p ~ '::A')`.
        Expr::IndirectTypeLookupTail {
            head,
            tail,
            trailing: false,
        } => (
            String::new(),
            Box::new(Expr::Binary {
                left: head,
                op: crate::token_kind::TokenKind::Tilde,
                right: Box::new(Expr::Literal(Value::str(tail.to_string()))),
                form: Default::default(),
            }),
        ),
        // `&::($p)::f` names the routine `&f` of package `$p`.
        Expr::IndirectCodeLookup {
            package,
            name,
            caller: false,
        } => (
            "&".to_string(),
            Box::new(Expr::Binary {
                left: package,
                op: crate::token_kind::TokenKind::Tilde,
                right: Box::new(Expr::Literal(Value::str(format!("::{name}")))),
                form: Default::default(),
            }),
        ),
        other => return Err(Box::new((other, value))),
    };
    Ok(Expr::IndirectSymbolBind {
        sigil,
        expr,
        value: Box::new(value),
    })
}

/// The bind `bind` (an [`Expr::IndirectSymbolBind`]) as
/// `ApplyInfix(left, Infix(":="), right)`.
// Cost: O(n), n = nodes of the name expression and the value.
pub(super) fn view(names: &NameScope, bind: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let Expr::IndirectSymbolBind { sigil, expr, value } = bind else {
        return Err(RuntimeError::new("RakuAST: not a run-time name bind"));
    };
    let target = if sigil.is_empty() {
        Expr::IndirectTypeLookup(expr.clone())
    } else {
        Expr::SymbolicDeref {
            sigil: sigil.to_string(),
            expr: expr.clone(),
        }
    };
    let left = view_expr(names, &target)?;
    Ok(bind_node(left, view_expr(names, value)?))
}

/// `ApplyInfix(left, Infix(":="), right)`: a bind is a plain `Infix`, not the
/// `Assignment` node `=` uses.
// Cost: O(1).
pub(super) fn bind_node(left: RakuAstNode, right: RakuAstNode) -> RakuAstNode {
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ApplyInfix,
        fields: vec![
            node_field(Some("left"), left),
            node_field(Some("infix"), plain_infix(":=")),
            node_field(Some("right"), right),
        ],
    }
}

/// Whether the `ApplyInfix` bind `node` (the caller checked the `:=`) binds a
/// name computed at run time: its left side is a `Var::Package` or a
/// `Term::Name` over a `::(EXPR)` name.
// Cost: O(p), p = parts of the left side's name.
pub(super) fn is_target(node: &RakuAstNode) -> bool {
    let Ok(left) = named_child(node, "left") else {
        return false;
    };
    let name = match left.class {
        RakuAstClass::VarPackage => named_child(left, "name").ok(),
        RakuAstClass::TermName => super::lower::named_child_or_positional(left).ok(),
        _ => None,
    };
    name.and_then(name_parts::name_shape).is_some_and(|shape| {
        matches!(
            shape,
            NameShape::Indirect {
                trailing: false,
                ..
            }
        )
    })
}

/// The bind `node` ([`is_target`] holds) as the parser's
/// [`Expr::IndirectSymbolBind`].
// Cost: O(n), n = nodes of the name expression and the value.
pub(super) fn lower(names: &NameScope, node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    let target = lower_expr(names, named_child(node, "left")?)?;
    let value = lower_expr(names, named_child(node, "right")?)?;
    indirect_symbol_bind(target, value).map_err(|pair| {
        let target = &pair.0;
        RuntimeError::new(format!(
            "RakuAST: a run-time name bind lowered to an unexpected target: {target:?}"
        ))
    })
}
