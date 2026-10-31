//! Symbolic dereference `$::($name)` / `@::($name)` and `::($name) = v` across
//! the RakuAST boundary.
//!
//! Measured against rakudo 2026.09:
//!
//! ```text
//! $::($n)         Var::Package(name => Name(Part::Empty, Part::Expression($n)), sigil => "$")
//! $::($n) = 5     ApplyInfix(Var::Package(…), Assignment(:item), 5)
//! ::($n) = 5      ApplyInfix(Term::Name(Name(Part::Empty, Part::Expression($n))), Assignment, 5)
//! &::($n)::f     Var::Package(Name(Part::Empty, Part::Expression($n), Part::Simple("f")), "&")
//! &CALLER::($n)::f has Part::Simple("CALLER") in place of Part::Empty.
//! ```
//!
//! The parser keeps the variable forms as [`Expr::SymbolicDeref`] /
//! [`Expr::SymbolicDerefAssign`] (the sigil and the name expression) and the
//! type form as [`Expr::IndirectTypeLookupAssign`] (the lookup itself, without
//! assignment, is `Expr::IndirectTypeLookup`, handled in `convert.rs`).

use crate::scope::NameScope;
use super::convert::{convert_expr, leaf_field, node_field};
use super::lower::{lower_expr, named_child};
use super::name_parts::{self, NameShape};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::value::{RuntimeError, Value, ValueView};

/// The `Name(Part::Empty, Part::Expression(EXPR))` of a dynamic lookup.
pub(super) fn indirect_name(names: &NameScope, expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let part = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::NamePartExpression,
        fields: vec![node_field(None, convert_expr(names, expr)?)],
    };
    Ok(name_parts::name_from_parts(vec![
        name_parts::leading_empty(),
        Value::rakuast(Box::new(part)),
    ]))
}

/// `$::($n)` as `Var::Package`.
// Cost: O(n), n = nodes of the name expression.
pub(super) fn convert(names: &NameScope, sigil: &str, expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::VarPackage,
        fields: vec![
            node_field(Some("name"), indirect_name(names, expr)?),
            leaf_field(Some("sigil"), Value::str(sigil.to_string())),
        ],
    })
}

/// `&::(EXPR)::name` and `&CALLER::(EXPR)::name` as a package code variable.
// Cost: O(n), n = nodes of the package expression plus the static name.
pub(super) fn convert_indirect_code(names: &NameScope, 
    package: &Expr,
    name: &str,
    caller: bool,
) -> Result<RakuAstNode, RuntimeError> {
    let mut parts = Vec::with_capacity(3);
    parts.push(if caller {
        name_parts::simple_part("CALLER")
    } else {
        name_parts::leading_empty()
    });
    parts.push(Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::NamePartExpression,
        fields: vec![node_field(None, convert_expr(names, package)?)],
    })));
    parts.push(name_parts::simple_part(name));
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::VarPackage,
        fields: vec![
            node_field(Some("name"), name_parts::name_from_parts(parts)),
            leaf_field(Some("sigil"), Value::str("&".to_string())),
        ],
    })
}

/// Recognize the two measured dynamic code-variable name shapes.
// Cost: O(n), n = name parts.
pub(super) fn lower_indirect_code(names: &NameScope, node: &RakuAstNode) -> Option<Result<Expr, RuntimeError>> {
    if node.class != RakuAstClass::VarPackage
        || super::lower::leaf_str(node, "sigil").ok()?.as_str() != "&"
    {
        return None;
    }
    let name = named_child(node, "name").ok()?;
    let RakuAstFieldValue::List(parts) = &name.fields.first()?.value else {
        return None;
    };
    let [first, middle, last] = parts.as_slice() else {
        return None;
    };
    let caller = match first.view() {
        ValueView::RakuAst(part) if part.class == RakuAstClass::NamePartEmpty => false,
        ValueView::RakuAst(part)
            if part.class == RakuAstClass::NamePartSimple
                && name_parts::part_identifier(first).as_deref() == Some("CALLER") =>
        {
            true
        }
        _ => return None,
    };
    let ValueView::RakuAst(middle) = middle.view() else {
        return None;
    };
    if middle.class != RakuAstClass::NamePartExpression {
        return None;
    }
    let RakuAstFieldValue::Node(package) = &middle.fields.first()?.value else {
        return None;
    };
    let ValueView::RakuAst(package) = package.view() else {
        return None;
    };
    let name = name_parts::part_identifier(last)?;
    Some(lower_expr(names, package).map(|package| Expr::IndirectCodeLookup {
        package: Box::new(package),
        name,
        caller,
    }))
}

/// The name expression of a plain `::(EXPR)` name (no static tail).
fn plain_indirect(name: &RakuAstNode) -> Option<&RakuAstNode> {
    match name_parts::name_shape(name)? {
        NameShape::Indirect {
            expr,
            tail,
            trailing: false,
        } if tail.is_empty() => Some(expr),
        _ => None,
    }
}

/// A `Var::Package` over a dynamic name as the parser's [`Expr::SymbolicDeref`],
/// or `None` when `node` is not one.
// Cost: O(n), n = nodes of the name expression.
pub(super) fn lower(names: &NameScope, node: &RakuAstNode) -> Option<Result<Expr, RuntimeError>> {
    if node.class != RakuAstClass::VarPackage {
        return None;
    }
    let expr = plain_indirect(named_child(node, "name").ok()?)?;
    let sigil = super::lower::leaf_str(node, "sigil").ok()?;
    if !matches!(sigil.as_str(), "$" | "@" | "%" | "&") {
        return None;
    }
    Some(lower_expr(names, expr).map(|expr| Expr::SymbolicDeref {
        sigil,
        expr: Box::new(expr),
    }))
}

/// `ApplyInfix(Var::Package(dynamic) | Term::Name(dynamic), Assignment, v)` as
/// the parser's assignment node, or `None` for any other assignment.
// Cost: O(n), n = nodes of the name expression and the value.
pub(super) fn lower_assign(names: &NameScope, node: &RakuAstNode) -> Result<Option<Expr>, RuntimeError> {
    let left = named_child(node, "left")?;
    let value = || lower_expr(named_child(node, "right")?).map(Box::new);
    match left.class {
        RakuAstClass::VarPackage => {
            let Some(Ok(Expr::SymbolicDeref { sigil, expr })) = lower(names, left) else {
                return Ok(None);
            };
            Ok(Some(Expr::SymbolicDerefAssign {
                sigil,
                expr,
                value: value()?,
            }))
        }
        RakuAstClass::TermName => {
            let Some(name) = left.fields.first().and_then(|f| match &f.value {
                super::RakuAstFieldValue::Node(v) => super::lower::rakuast_node_of(v),
                _ => None,
            }) else {
                return Ok(None);
            };
            let Some(expr) = plain_indirect(name) else {
                return Ok(None);
            };
            Ok(Some(Expr::IndirectTypeLookupAssign {
                expr: Box::new(lower_expr(names, expr)?),
                value: value()?,
            }))
        }
        _ => Ok(None),
    }
}

/// The pseudo-packages rakudo 2026.09 resolves lexically: a lookup qualified
/// by one (`MY::(EXPR)`) renders as a `Term::Name`. `GLOBAL`, `PROCESS`,
/// `EXPORT` and a user package render as a `Call::Name` over the same name.
fn is_lexical_pseudo_package(segment: &str) -> bool {
    matches!(
        segment,
        "MY" | "OUR"
            | "CORE"
            | "SETTING"
            | "UNIT"
            | "OUTER"
            | "OUTERS"
            | "CALLER"
            | "CALLERS"
            | "DYNAMIC"
            | "LEXICAL"
            | "CLIENT"
    )
}

/// `MY::(EXPR)` / `GLOBAL::(EXPR)`: the package's parts, then the key as a
/// `Name::Part::Expression`.
// Cost: O(n), n = nodes of the key, plus the package's segments.
pub(super) fn convert_package_indirect(names: &NameScope, 
    package: &str,
    key: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
    let mut parts: Vec<Value> = name_parts::identifier_segments(package)
        .map(name_parts::simple_part)
        .collect();
    parts.push(Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::NamePartExpression,
        fields: vec![node_field(None, convert_expr(names, key)?)],
    })));
    let name = name_parts::name_from_parts(parts);
    let lexical = name_parts::identifier_segments(package)
        .next()
        .is_some_and(is_lexical_pseudo_package);
    Ok(if lexical {
        RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::TermName,
            fields: vec![node_field(None, name)],
        }
    } else {
        RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::CallName,
            fields: vec![node_field(Some("name"), name)],
        }
    })
}

/// The package and key of a `Term::Name` / `Call::Name` (without arguments)
/// over `Name(Part::Simple…, Part::Expression(EXPR))`, or `None`.
// Cost: O(p), p = parts of the name.
pub(super) fn package_indirect(node: &RakuAstNode) -> Option<(String, &RakuAstNode)> {
    let name = match node.class {
        RakuAstClass::TermName => super::lower::named_child_or_positional(node).ok()?,
        RakuAstClass::CallName if !node.fields.iter().any(|f| f.name == Some("args")) => {
            named_child(node, "name").ok()?
        }
        _ => return None,
    };
    name_parts::package_and_expression(name)
}

/// [`package_indirect`] as the parser's lookup: the symbolic dereference of
/// `"PKG::" ~ EXPR`.
// Cost: O(n), n = nodes of the key.
pub(super) fn lower_package_indirect(names: &NameScope, node: &RakuAstNode) -> Option<Result<Expr, RuntimeError>> {
    let (package, key) = package_indirect(node)?;
    Some(lower_expr(names, key).map(|key| Expr::SymbolicDeref {
        sigil: String::new(),
        expr: Box::new(Expr::Binary {
            left: Box::new(Expr::Literal(Value::str(format!("{package}::")))),
            op: crate::token_kind::TokenKind::Tilde,
            right: Box::new(key),
            form: Default::default(),
        }),
    }))
}

/// A declarator's `::(EXPR)` name (`sub ::("x") { }`, `method ::($n) { }`): the
/// parser's name text and the name expression, read back from
/// `Name(Part::Empty, Part::Expression(EXPR))`. A literal or bare word keeps
/// its text as the name, anything else the parser's placeholder.
// Cost: O(n), n = nodes of the name expression.
pub(super) fn lower_declarator_name(names: &NameScope, 
    name: &RakuAstNode,
) -> Result<Option<(String, Expr)>, RuntimeError> {
    let Some(NameShape::Indirect {
        expr,
        tail,
        trailing: false,
    }) = name_parts::name_shape(name)
    else {
        return Ok(None);
    };
    if !tail.is_empty() {
        return Ok(None);
    }
    let lowered = lower_expr(names, expr)?;
    let text = match &lowered {
        Expr::Literal(v) => match v.view() {
            crate::value::ValueView::Str(s) => Some(s.to_string()),
            _ => None,
        },
        Expr::BareWord(s) => Some(s.clone()),
        _ => None,
    };
    Ok(Some((
        text.unwrap_or_else(|| crate::parser::INDIRECT_DECL_NAME.to_string()),
        lowered,
    )))
}

/// Swap a routine node's `name` for the `::(EXPR)` form when the declaration
/// had a computed name.
// Cost: O(n), n = nodes of the name expression.
pub(super) fn replace_declarator_name(names: &NameScope, 
    node: &mut RakuAstNode,
    name_expr: Option<&Expr>,
) -> Result<(), RuntimeError> {
    if let Some(expr) = name_expr {
        let field = node_field(Some("name"), indirect_name(names, expr)?);
        match node.fields.iter_mut().find(|f| f.name == Some("name")) {
            Some(slot) => *slot = field,
            None => node.fields.insert(0, field),
        }
    }
    Ok(())
}
