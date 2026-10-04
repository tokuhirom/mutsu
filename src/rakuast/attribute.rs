//! Attribute traits (`has $.x is rw = 5`) across the RakuAST boundary.
//!
//! Measured against rakudo 2026.09, an attribute is a
//! `VarDeclaration::Simple(scope => "has", …)` whose `traits` list holds one
//! `Trait::Is(name => Name.from-identifier("rw"))` per written trait, followed
//! by the implicit `Trait::WillBuild(EXPR)` an `= EXPR` default adds. The
//! default is also the `initializer`, and the gist omits the implicit
//! `WillBuild` (only `.traits` shows it).
//!
//! `is default(EXPR)` and `is built(False)` carry their argument as
//! `argument => Circumfix::Parentheses(SemiList(Statement::Expression(EXPR)))`;
//! a bare `is built` has none. A scalar attribute with `is default(EXPR)` and
//! no initializer starts with that value, which the parser also stores as its
//! `default` (flagged `default_is_trait`), and rakudo shows no initializer.
//!
//! The parser records these traits as fields on `Stmt::HasDecl`, not in
//! source order, so an attribute with more than one of them is refused rather
//! than rendered in an invented order. A `handles TERM` clause is a
//! `Trait::Handles(TERM)`; the parser keeps each clause's written term beside
//! the delegation specs it reads from it (a name, a word list, `*`), and a
//! clause spelled any other way (a rename pair, a regex, a variable) stays
//! refused. The parser keeps `is built`'s argument
//! as a `Bool`, so `is built(True)` comes back as the bare `is built` it means.

use super::convert::{
    convert_expr, name_from_identifier, node_field, statement_expression, unsupported,
};
use super::lower::{lower_expr, named_child, named_child_or_positional, positional_leaf};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::value::{RuntimeError, Value, ValueView};

/// The `is` traits of an attribute, as `HasDecl` records them.
#[derive(Debug, Default, Clone)]
pub(super) struct AttributeTraits {
    pub(super) is_rw: bool,
    pub(super) is_readonly: bool,
    pub(super) is_required: bool,
    /// `is default(EXPR)`.
    pub(super) is_default: Option<Expr>,
    /// `is built` (`true`) or `is built(False)`.
    pub(super) is_built: Option<bool>,
    /// The written term of each `handles` clause.
    pub(super) handles_terms: Vec<Expr>,
    /// The specs those terms name (`parser::handle_specs_from_term`).
    pub(super) handles: Vec<crate::ast::HandleSpec>,
}

impl AttributeTraits {
    /// Each written trait's name and, when it has one, its argument.
    fn written(&self) -> Vec<(&'static str, Option<Expr>)> {
        let mut written: Vec<(&'static str, Option<Expr>)> = [
            (self.is_rw, "rw"),
            (self.is_readonly, "readonly"),
            (self.is_required, "required"),
        ]
        .into_iter()
        .filter_map(|(on, name)| on.then_some((name, None)))
        .collect();
        if let Some(value) = &self.is_default {
            written.push(("default", Some(value.clone())));
        }
        match self.is_built {
            Some(true) => written.push(("built", None)),
            Some(false) => written.push(("built", Some(Expr::Literal(Value::truth(false))))),
            None => {}
        }
        written
    }
}

/// `(EXPR)` as a trait argument.
// Cost: O(e), e = size of the expression.
pub(super) fn paren_argument(expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let semilist = RakuAstNode {
        class: RakuAstClass::SemiList,
        fields: vec![node_field(None, statement_expression(convert_expr(expr)?))],
    };
    Ok(RakuAstNode {
        class: RakuAstClass::CircumfixParentheses,
        fields: vec![node_field(None, semilist)],
    })
}

/// The expression inside a `(EXPR)` trait argument.
// Cost: O(e), e = size of the expression.
pub(super) fn lower_paren_argument(
    owner: &RakuAstNode,
    argument: &RakuAstNode,
) -> Result<Expr, RuntimeError> {
    let refuse = || super::lower::unsupported(owner);
    if argument.class != RakuAstClass::CircumfixParentheses {
        return Err(refuse());
    }
    let semilist = named_child_or_positional(argument)?;
    let [statement] = semilist.fields.as_slice() else {
        return Err(refuse());
    };
    let RakuAstFieldValue::Node(statement) = &statement.value else {
        return Err(refuse());
    };
    let ValueView::RakuAst(statement) = statement.view() else {
        return Err(refuse());
    };
    if statement.class != RakuAstClass::StatementExpression || statement.fields.len() != 1 {
        return Err(refuse());
    }
    lower_expr(named_child(statement, "expression")?)
}

/// Put the attribute's written traits at the front of `decl`'s `traits` list,
/// ahead of an implicit `WillBuild`, creating the field (after `desigilname`)
/// when the declaration has no default.
// Cost: O(f), f = fields of `decl`.
pub(super) fn add_traits(
    decl: &mut RakuAstNode,
    traits: AttributeTraits,
) -> Result<(), RuntimeError> {
    let written = traits.written();
    if written.len() + traits.handles_terms.len() > 1 {
        return Err(unsupported(
            "attribute with several traits (their source order is not kept)",
        ));
    }
    let mut items = Vec::with_capacity(written.len() + traits.handles_terms.len());
    for term in &traits.handles_terms {
        items.push(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::TraitHandles,
            fields: vec![node_field(None, convert_expr(term)?)],
        })));
    }
    for (name, argument) in written {
        let mut fields = vec![node_field(Some("name"), name_from_identifier(name))];
        if let Some(argument) = argument {
            fields.push(node_field(Some("argument"), paren_argument(&argument)?));
        }
        items.push(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::TraitIs,
            fields,
        })));
    }
    if items.is_empty() {
        return Ok(());
    }
    if let Some(field) = decl.fields.iter_mut().find(|f| f.name == Some("traits")) {
        let RakuAstFieldValue::List(existing) = &mut field.value else {
            return Err(unsupported("attribute traits field"));
        };
        items.append(existing);
        *existing = items;
        return Ok(());
    }
    let at = decl
        .fields
        .iter()
        .position(|f| f.name == Some("desigilname"))
        .map_or(decl.fields.len(), |i| i + 1);
    decl.fields.insert(
        at,
        RakuAstField {
            name: Some("traits"),
            value: RakuAstFieldValue::List(items),
        },
    );
    Ok(())
}

/// The traits of an attribute `VarDeclaration::Simple`, back as `HasDecl`
/// flags. An implicit `WillBuild` is accepted only alongside an
/// `initializer`, which carries the same default.
// Cost: O(t), t = traits of the declaration.
pub(super) fn lower_traits(node: &RakuAstNode) -> Result<AttributeTraits, RuntimeError> {
    let refuse = || super::lower::unsupported(node);
    let mut traits = AttributeTraits::default();
    let Some(field) = node.fields.iter().find(|f| f.name == Some("traits")) else {
        return Ok(traits);
    };
    let RakuAstFieldValue::List(items) = &field.value else {
        return Err(refuse());
    };
    let has_initializer = node.fields.iter().any(|f| f.name == Some("initializer"));
    for item in items {
        let ValueView::RakuAst(t) = item.view() else {
            return Err(refuse());
        };
        match t.class {
            RakuAstClass::TraitWillBuild if has_initializer => {}
            // `handles TERM`: the specs come from the term, as the parser
            // builds them.
            RakuAstClass::TraitHandles => {
                let term = lower_expr(named_child_or_positional(t)?)?;
                let specs = crate::parser::handle_specs_from_term(&term).ok_or_else(refuse)?;
                traits.handles.extend(specs);
                traits.handles_terms.push(term);
            }
            RakuAstClass::TraitIs => {
                let name = positional_leaf(named_child(t, "name")?)?;
                let ValueView::Str(name) = name.view() else {
                    return Err(refuse());
                };
                let argument = match named_child(t, "argument") {
                    Ok(argument) => Some(lower_paren_argument(node, argument)?),
                    Err(_) if t.fields.len() == 1 => None,
                    Err(_) => return Err(refuse()),
                };
                match (name.as_str(), argument) {
                    ("rw", None) => traits.is_rw = true,
                    ("readonly", None) => traits.is_readonly = true,
                    ("required", None) => traits.is_required = true,
                    ("default", Some(value)) => traits.is_default = Some(value),
                    ("built", None) => traits.is_built = Some(true),
                    ("built", Some(Expr::Literal(value)))
                        if matches!(value.view(), ValueView::Bool(_)) =>
                    {
                        traits.is_built = Some(value.truthy());
                    }
                    _ => return Err(refuse()),
                }
            }
            _ => return Err(refuse()),
        }
    }
    Ok(traits)
}

/// The gist of an attribute omits the implicit `WillBuild` its default adds
/// (`.traits` still answers it): `decl` without it, or `None` when there is
/// none to hide.
// Cost: O(f + t), f = fields of `decl`, t = its traits.
pub(super) fn without_implicit_traits(decl: &RakuAstNode) -> Option<RakuAstNode> {
    let is_will_build = |v: &Value| matches!(v.view(), ValueView::RakuAst(t) if t.class == RakuAstClass::TraitWillBuild);
    let traits = decl.fields.iter().find(|f| f.name == Some("traits"))?;
    let RakuAstFieldValue::List(items) = &traits.value else {
        return None;
    };
    if !items.iter().any(is_will_build) {
        return None;
    }
    let mut shown = decl.clone();
    shown.fields.retain_mut(|f| {
        if f.name != Some("traits") {
            return true;
        }
        if let RakuAstFieldValue::List(items) = &mut f.value {
            items.retain(|v| !is_will_build(v));
            !items.is_empty()
        } else {
            true
        }
    });
    Some(shown)
}
