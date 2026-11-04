//! The nodes Rakudo's `visit-children` shows that the source never wrote.
//!
//! A `Method`'s `Signature` carries two parameters of its own: a leading
//! invocant, typed by the enclosing package (`Parameter(Type::Simple(Name))`,
//! a bare `Parameter` outside a package), unless the written signature opens
//! with an invocant of its own (`Int:D: $x`, `$self: $x`); and a trailing
//! `%_` (`Parameter(ParameterTarget::Var(VarDeclaration::Simple))`), unless
//! the written signature already collects named arguments (`*%h`, `|c`). A
//! method without a written signature gets a whole one. None of these has an
//! origin, and none is part of the node's fields: its constructor form,
//! equality and `EVAL` round trip stay what the source wrote.
//!
//! A `Var::Attribute::Public` (`$.n`) visits its expansion,
//! `self.n.item` (`.list` for `@`, `.hash` for `%`), also without origins.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

fn node(class: RakuAstClass, fields: Vec<RakuAstField>) -> Value {
    Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class,
        fields,
    }))
}

fn field(name: Option<&'static str>, value: Value) -> RakuAstField {
    RakuAstField {
        name,
        value: RakuAstFieldValue::Node(value),
    }
}

/// Whether the written parameter `param` is the signature's own invocant.
// Cost: O(f), f = fields of `param`.
fn is_invocant(param: &Value) -> bool {
    matches!(param.view(), ValueView::RakuAst(p)
        if p.fields.iter().any(|f| f.name == Some("invocant")
            && matches!(&f.value, RakuAstFieldValue::Node(v) if v.truthy())))
}

/// Whether the written parameter `param` takes the named arguments `%_` would
/// (a slurpy hash, a capture).
// Cost: O(f), f = fields of `param`.
fn collects_named(param: &Value) -> bool {
    let ValueView::RakuAst(p) = param.view() else {
        return false;
    };
    p.fields.iter().any(|f| {
        f.name == Some("slurpy")
            && matches!(&f.value, RakuAstFieldValue::Node(marker)
                if super::slurpy_marker_class(marker) == Some(RakuAstClass::ParameterSlurpyCapture))
    }) || p.fields.iter().any(|f| {
        f.name == Some("slurpy")
            && p.fields.iter().any(|t| {
                t.name == Some("target")
                    && matches!(&t.value, RakuAstFieldValue::Node(target)
                        if matches!(target.view(), ValueView::RakuAst(t)
                            if t.fields.iter().any(|n| n.name == Some("name")
                                && matches!(&n.value, RakuAstFieldValue::Node(v)
                                    if v.as_str().is_some_and(|s| s.starts_with('%'))))))
            })
    })
}

/// The `Signature` a `Method` / `Submethod` visits: the written one (or an
/// empty one) with the implicit parameters added.
// Cost: O(p), p = written parameters.
fn method_signature(method: &RakuAstNode) -> Value {
    let written = method.fields.iter().find_map(|f| match &f.value {
        RakuAstFieldValue::Node(v) if f.name == Some("signature") => match v.view() {
            ValueView::RakuAst(sig) => Some(sig.clone()),
            _ => None,
        },
        _ => None,
    });
    let mut signature = written.unwrap_or_else(|| RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::Signature,
        fields: Vec::new(),
    });
    let mut parameters = signature
        .fields
        .iter()
        .find_map(|f| match &f.value {
            RakuAstFieldValue::List(items) if f.name == Some("parameters") => Some(items.clone()),
            _ => None,
        })
        .unwrap_or_default();
    if !parameters.first().is_some_and(is_invocant) {
        let fields = match method.origin.3.as_deref() {
            Some(package) => vec![field(
                Some("type"),
                node(
                    RakuAstClass::TypeSimple,
                    vec![field(
                        None,
                        Value::rakuast(Box::new(super::view::name_from_identifier(package))),
                    )],
                ),
            )],
            None => Vec::new(),
        };
        parameters.insert(0, node(RakuAstClass::Parameter, fields));
    }
    if !parameters.iter().any(collects_named) {
        let target = node(
            RakuAstClass::ParameterTargetVar,
            vec![field(Some("name"), Value::str_from("%_"))],
        );
        parameters.push(node(
            RakuAstClass::Parameter,
            vec![field(Some("target"), target)],
        ));
    }
    signature.fields.retain(|f| f.name != Some("parameters"));
    signature.fields.insert(
        0,
        RakuAstField {
            name: Some("parameters"),
            value: RakuAstFieldValue::List(parameters),
        },
    );
    Value::rakuast(Box::new(signature))
}

/// `method` with its `Signature` field filled in as `visit-children` shows it,
/// in field order (after the name, ahead of the traits and the body).
// Cost: O(f + p), f = fields of `method`, p = written parameters.
pub(crate) fn method_with_signature(method: &RakuAstNode) -> Option<RakuAstNode> {
    if !matches!(method.class, RakuAstClass::Method | RakuAstClass::Submethod) {
        return None;
    }
    let signature = method_signature(method);
    let mut out = method.clone();
    match out.fields.iter().position(|f| f.name == Some("signature")) {
        Some(at) => out.fields[at].value = RakuAstFieldValue::Node(signature),
        None => {
            let at = out
                .fields
                .iter()
                .position(|f| f.name == Some("name"))
                .map_or(0, |i| i + 1);
            out.fields.insert(at, field(Some("signature"), signature));
        }
    }
    Some(out)
}

/// The expansion a `Var::Attribute::Public` visits, `None` for one written
/// with arguments (`$.n(1)`).
// Cost: O(f), f = fields of `var`.
pub(crate) fn public_attribute_expansion(var: &RakuAstNode) -> Option<Value> {
    if var.class != RakuAstClass::VarAttributePublic
        || var.fields.iter().any(|f| f.name == Some("args"))
    {
        return None;
    }
    let spelling = var.fields.iter().find_map(|f| match &f.value {
        RakuAstFieldValue::Node(v) if f.name == Some("name") => v.as_str().map(str::to_string),
        _ => None,
    })?;
    let sigil = spelling.chars().next()?;
    let attribute = spelling.split_once('.')?.1;
    let coercion = match sigil {
        '@' => "list",
        '%' => "hash",
        _ => "item",
    };
    let call = |name: &str| {
        node(
            RakuAstClass::CallMethod,
            vec![field(
                Some("name"),
                Value::rakuast(Box::new(super::view::name_from_identifier(name))),
            )],
        )
    };
    let accessor = node(
        RakuAstClass::ApplyPostfix,
        vec![
            field(Some("operand"), node(RakuAstClass::TermSelf, Vec::new())),
            field(Some("postfix"), call(attribute)),
        ],
    );
    Some(node(
        RakuAstClass::ApplyPostfix,
        vec![
            field(Some("operand"), accessor),
            field(Some("postfix"), call(coercion)),
        ],
    ))
}

/// `body` converted as the body of the package `name` (`None`: a grammar,
/// whose methods have a bare invocant parameter), so a method inside it
/// records the package its implicit invocant is typed by.
// Cost: O(n), n = size of the body.
pub(super) fn pkg(
    names: &crate::scope::NameScope,
    name: Option<crate::symbol::Symbol>,
    body: &[crate::ast::Stmt],
) -> Result<RakuAstNode, crate::value::RuntimeError> {
    let _package = crate::scope::PackageGuard::enter(names, name.map(|n| n.resolve().to_string()));
    super::view::block_node(names, body)
}
