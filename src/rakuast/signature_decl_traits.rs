//! The traits of a signature declaration (`my ($a, $b) is default(42)`;
//! #18597), and its constructor.
//!
//! Measured against rakudo 2026.11, the group's `is default(...)` is a
//! `Trait::Is` in the declaration's `traits`. Its `.raku` leaves the traits
//! out, but its `visit-children` walks them after the initializer. Each
//! parameter's `VarDeclaration::Simple` (its target's `declaration`) carries
//! the same trait. The parser keeps the trait's argument as the group default
//! the expansion assigns (`SignatureDecl::group_default`).

use super::lower::{lower_expr, named_child, named_child_or_positional};
use super::view::{leaf_field, node_field, statement_expression, view_expr};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::ast::SrcSpan;
use crate::scope::NameScope;
use crate::value::{RuntimeError, Value, ValueView};

/// Construct the same declaring signature shape produced by the parser.
// Cost: O(a), a = number of constructor arguments (a fixed number of lookups).
pub(super) fn construct(args: &[Value]) -> Result<Value, RuntimeError> {
    let ctor = "RakuAST::VarDeclaration::Signature.new";
    let signature = super::named_arg(args, "signature")
        .ok_or_else(|| RuntimeError::new(format!("{ctor} requires `signature`")))?;
    super::require_rakuast_class(&signature, RakuAstClass::Signature, ctor)?;
    let mut fields = vec![RakuAstField {
        name: Some("signature"),
        value: RakuAstFieldValue::Node(signature),
    }];
    if let Some(scope) = super::named_arg(args, "scope") {
        if !matches!(scope.view(), ValueView::Str(_)) {
            return Err(RuntimeError::new(format!(
                "{ctor} expects `scope` to be Str"
            )));
        }
        fields.push(leaf_field(Some("scope"), scope));
    }
    if let Some(ty) = super::named_arg(args, "type") {
        super::require_rakuast_type(&ty, ctor)?;
        fields.push(RakuAstField {
            name: Some("type"),
            value: RakuAstFieldValue::Node(ty),
        });
    }
    if let Some(init) = super::named_arg(args, "initializer") {
        if !matches!(init.view(), ValueView::RakuAst(n) if matches!(n.class,
            RakuAstClass::InitializerAssign | RakuAstClass::InitializerBind))
        {
            return Err(RuntimeError::new(format!(
                "{ctor} expects an assignment or binding initializer"
            )));
        }
        fields.push(RakuAstField {
            name: Some("initializer"),
            value: RakuAstFieldValue::Node(init),
        });
    }
    Ok(Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::VarDeclarationSignature,
        fields,
    })))
}

/// The `Trait::Is` of a group `is default(EXPR)`: the name, and the
/// argument in parentheses.
// Cost: O(n), n = size of the default expression's view.
pub(super) fn default_trait(
    names: &NameScope,
    default: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
    let semilist = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::SemiList,
        fields: vec![node_field(
            None,
            statement_expression(view_expr(names, default)?),
        )],
    };
    let argument = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::CircumfixParentheses,
        fields: vec![node_field(None, semilist)],
    };
    Ok(super::routine_traits::trait_is("default", Some(argument)))
}

/// The `traits` field of a declaration holding `traits`.
// Cost: O(t), t = traits (each is shared, not copied).
pub(super) fn traits_field(traits: &[RakuAstNode]) -> RakuAstField {
    RakuAstField {
        name: Some("traits"),
        value: RakuAstFieldValue::List(
            traits
                .iter()
                .map(|t| Value::rakuast(Box::new(t.clone())))
                .collect(),
        ),
    }
}

/// Give a parameter's `ParameterTarget::Var` the `VarDeclaration::Simple`
/// that carries the group's traits (its hidden `declaration`).
// Cost: O(t), t = traits.
pub(super) fn with_declaration(target: &mut RakuAstNode, traits: &[RakuAstNode]) {
    if target.class != RakuAstClass::ParameterTargetVar || traits.is_empty() {
        return;
    }
    let declaration = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::VarDeclarationSimple,
        fields: vec![traits_field(traits)],
    };
    target
        .fields
        .push(node_field(Some("declaration"), declaration));
}

/// The group default a `VarDeclaration::Signature` carries: the argument of
/// its `is default(...)` trait.
// Cost: O(t + n), t = traits, n = size of the argument.
pub(super) fn group_default(
    names: &NameScope,
    node: &RakuAstNode,
) -> Result<Option<Expr>, RuntimeError> {
    let Some(RakuAstFieldValue::List(traits)) = node
        .fields
        .iter()
        .find(|f| f.name == Some("traits"))
        .map(|f| &f.value)
    else {
        return Ok(None);
    };
    for value in traits {
        let ValueView::RakuAst(trait_node) = value.view() else {
            continue;
        };
        let is_default = trait_node.class == RakuAstClass::TraitIs
            && named_child(trait_node, "name")
                .ok()
                .and_then(super::infix_origins::leaf_text)
                .is_some_and(|name| name == "default");
        if !is_default {
            continue;
        }
        let argument = named_child(trait_node, "argument")?;
        let statement = named_child_or_positional(named_child_or_positional(argument)?)?;
        let expression = named_child(statement, "expression")?;
        return lower_expr(names, expression).map(Some);
    }
    Ok(None)
}

/// Whether `field` of `node` is left out of `.raku`: a signature
/// declaration's traits, as in Rakudo.
// Cost: O(1).
pub(super) fn is_hidden(node: &RakuAstNode, field: &RakuAstField) -> bool {
    node.class == RakuAstClass::VarDeclarationSignature && field.name == Some("traits")
}

/// Place the traits of `node`, a `VarDeclaration::Signature` whose
/// signature's `)` is at `close` of `text` (written from byte `base`), from
/// the clauses after it, and give each parameter's declaration its
/// target's range and the placed traits.
// Cost: O(n + p * t), n = length of `text`, p = parameters, t = traits.
pub(super) fn place(
    names: &NameScope,
    node: &mut RakuAstNode,
    text: &str,
    close: usize,
    base: usize,
) {
    let Some(clauses) = super::decl_origins::read_traits(text, close + 1, base) else {
        return;
    };
    super::decl_origins::traits(names, node, &clauses);
    let placed: Vec<Value> = match node.fields.iter().find(|f| f.name == Some("traits")) {
        Some(RakuAstField {
            value: RakuAstFieldValue::List(traits),
            ..
        }) => traits.clone(),
        _ => return,
    };
    super::infix_origins::with_child(node, "signature", |signature| {
        let Some(field) = signature
            .fields
            .iter_mut()
            .find(|f| f.name == Some("parameters"))
        else {
            return;
        };
        let RakuAstFieldValue::List(parameters) = &mut field.value else {
            return;
        };
        for value in parameters.iter_mut() {
            let ValueView::RakuAst(param) = value.view() else {
                continue;
            };
            let mut param = RakuAstNode::clone(param);
            super::infix_origins::with_child(&mut param, "target", |target| {
                let span = super::infix_origins::byte_range(target)
                    .map_or(SrcSpan::NONE, |(from, to)| SrcSpan::new(from, to));
                super::infix_origins::with_child(target, "declaration", |declaration| {
                    super::origin::set_span(declaration, span);
                    if let Some(field) = declaration
                        .fields
                        .iter_mut()
                        .find(|f| f.name == Some("traits"))
                    {
                        field.value = RakuAstFieldValue::List(placed.clone());
                    }
                });
            });
            *value = Value::rakuast(Box::new(param));
        }
    });
}
