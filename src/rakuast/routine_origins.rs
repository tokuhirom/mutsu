//! Origins below a routine or package declaration (ADR-10723 Stage 2): its
//! `Name`, a routine's `Signature` and `Parameter`s, and its `Block` /
//! `Blockoid` / `StatementList`, from the positions the parser recorded on
//! `Stmt::SubDecl` / `Stmt::MethodDecl` / `Stmt::ClassDecl` / `Stmt::RoleDecl`.

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::parsed::{BlockSpans, RoutineSpans, TraitSpans, TypeDeclSpans};
use crate::ast::{ParamDef, Stmt};
use crate::value::{Value, ValueView};

/// Give the parts of `node` (a `Sub`, `Method` or `Submethod` built from
/// `params`) the origins in `spans`.
// Cost: O(f + p), f = fields of the routine node, p = parameters.
pub(super) fn apply(node: &mut RakuAstNode, spans: Option<&RoutineSpans>, params: &[ParamDef]) {
    let Some(spans) = spans else {
        return;
    };
    with_child(node, "name", |name| {
        super::origin::set_span(name, spans.name)
    });
    with_child(node, "signature", |signature| {
        super::origin::set_span(signature, spans.signature);
        parameters(signature, params);
    });
    with_child(node, "body", |blockoid| {
        super::origin::set_span(blockoid, spans.block.blockoid);
        if let Some(field) = blockoid.fields.first_mut() {
            with_value(&mut field.value, |list| {
                super::origin::set_span(list, spans.block.list)
            });
        }
    });
}

/// Give the parameters of the signature of `node` (an anonymous `sub` /
/// `method` built from `params`, which has no [`RoutineSpans`]) the origins the
/// parser recorded for them.
// Cost: O(f + p), f = fields of the routine node, p = parameters.
pub(super) fn anon_parameters(node: &mut RakuAstNode, params: &[ParamDef]) {
    with_child(node, "signature", |signature| parameters(signature, params));
}

/// Give the traits of `node` (a routine whose `traits` list was rebuilt from
/// the `.AST` parse's trait records, one per record) the origins in `spans`:
/// the trait, and the name or type it names.
// Cost: O(t), t = traits.
pub(super) fn traits(node: &mut RakuAstNode, spans: Option<&RoutineSpans>) {
    let Some(spans) = spans else {
        return;
    };
    list_origins(node, "traits", &spans.traits);
}

/// Give the trait nodes in `node`'s list field `field` the origins in `spans`,
/// when there is one per trait.
// Cost: O(t), t = traits.
fn list_origins(node: &mut RakuAstNode, field: &str, spans: &[TraitSpans]) {
    let Some(field) = node.fields.iter_mut().find(|f| f.name == Some(field)) else {
        return;
    };
    let RakuAstFieldValue::List(nodes) = &mut field.value else {
        return;
    };
    if nodes.len() != spans.len() {
        return;
    }
    for (value, at) in nodes.iter_mut().zip(spans) {
        let ValueView::RakuAst(trait_node) = value.view() else {
            continue;
        };
        let mut trait_node = RakuAstNode::clone(trait_node);
        super::origin::set_span(&mut trait_node, at.whole);
        // The trait's name (`Trait::Is`) or type (`Trait::Returns`, `::Of`).
        if let Some(first) = trait_node.fields.first_mut() {
            with_value(&mut first.value, |named| type_origin(named, at.name));
        }
        *value = Value::rakuast(Box::new(trait_node));
    }
}

/// Give the package `statement` declares (a `Class`, `Grammar` or `Role`
/// under the `Statement::Expression` converted from `stmt`) its name's and
/// block's origins.
// Cost: O(f), f = fields of the nodes on the path.
pub(super) fn package_origins(statement: &mut RakuAstNode, stmt: &Stmt) {
    if super::finalize::executing() {
        return;
    }
    let stmt = match stmt {
        Stmt::Ast(node) => node.inner_stmt().unwrap_or(stmt),
        _ => stmt,
    };
    if let Stmt::EnumDecl { spans, .. } | Stmt::SubsetDecl { spans, .. } = stmt {
        if let Some(spans) = spans.as_deref() {
            type_decl_origins(statement, spans);
        }
        return;
    }
    let (Stmt::ClassDecl { spans, .. }
    | Stmt::RoleDecl { spans, .. }
    | Stmt::Package { spans, .. }) = stmt
    else {
        return;
    };
    let Some(spans) = spans.as_deref() else {
        return;
    };
    // `unit class C;` holds the rest of the unit as its body.
    let unit = super::unit_span::unit_body(stmt)
        .and_then(|body| super::unit_span::package_spans(Some(spans), body));
    let spans = unit.as_deref().unwrap_or(spans);
    let mut block = spans.block;
    // A role's body is the block its type parameters belong to, and Rakudo
    // starts it at the role's name (`R[::T] { }`).
    if matches!(stmt, Stmt::RoleDecl { .. }) && spans.name.is_some() && block.span.is_some() {
        block.span = crate::ast::SrcSpan::new(spans.name.from as usize, block.span.to as usize);
    }
    with_child(statement, "expression", |decl| {
        with_child(decl, "name", |name| {
            super::origin::set_span(name, spans.name)
        });
        with_child(decl, "body", |body| block_origins(body, block));
        // The traits are written between the name and the block.
        if spans.name.is_some() && spans.block.blockoid.is_some() {
            super::package_trait_origins::apply(
                decl,
                crate::ast::SrcSpan::new(
                    spans.name.to as usize,
                    spans.block.blockoid.from as usize,
                ),
            );
        }
    });
}

/// Give the `Type::Enum` / `Type::Subset` `statement` declares its name's
/// origin, and a subset's `Trait::Of` / `Trait::Is` (`is export`, the only
/// `is` trait a subset keeps) the origins of its `of TYPE` / `is export`.
// Cost: O(f + t), f = fields of the declaration, t = its traits.
fn type_decl_origins(statement: &mut RakuAstNode, spans: &TypeDeclSpans) {
    with_child(statement, "expression", |decl| {
        with_child(decl, "name", |name| {
            super::origin::set_span(name, spans.name)
        });
        let Some(field) = decl.fields.iter_mut().find(|f| f.name == Some("traits")) else {
            return;
        };
        let RakuAstFieldValue::List(traits) = &mut field.value else {
            return;
        };
        for value in traits.iter_mut() {
            let ValueView::RakuAst(node) = value.view() else {
                continue;
            };
            let at = match node.class {
                RakuAstClass::TraitOf => spans.of,
                RakuAstClass::TraitIs => spans.export,
                _ => continue,
            };
            let mut node = RakuAstNode::clone(node);
            super::origin::set_span(&mut node, at.whole);
            if let Some(first) = node.fields.first_mut() {
                with_value(&mut first.value, |named| type_origin(named, at.name));
            }
            *value = Value::rakuast(Box::new(node));
        }
    });
}

/// Give a `Block` over a `Blockoid` over a `StatementList` the three origins
/// in `spans`.
// Cost: O(1).
pub(super) fn block_origins(block: &mut RakuAstNode, spans: BlockSpans) {
    super::origin::set_span(block, spans.span);
    with_child(block, "body", |blockoid| {
        super::origin::set_span(blockoid, spans.blockoid);
        if let Some(field) = blockoid.fields.first_mut() {
            with_value(&mut field.value, |list| {
                super::origin::set_span(list, spans.list)
            });
        }
    });
}

/// Give each `Parameter` of `signature` the origin of the parameter it was
/// built from, when there is one node per parameter.
// Cost: O(p), p = parameters.
pub(super) fn parameters(signature: &mut RakuAstNode, params: &[ParamDef]) {
    let Some(field) = signature
        .fields
        .iter_mut()
        .find(|f| f.name == Some("parameters"))
    else {
        return;
    };
    let RakuAstFieldValue::List(nodes) = &mut field.value else {
        return;
    };
    if nodes.len() != params.len() {
        return;
    }
    for (value, param) in nodes.iter_mut().zip(params) {
        let ValueView::RakuAst(node) = value.view() else {
            continue;
        };
        let mut node = RakuAstNode::clone(node);
        parameter(
            &mut node,
            param.spans.param,
            param.spans.target,
            param.spans.type_,
        );
        list_origins(&mut node, "traits", &param.spans.traits);
        *value = Value::rakuast(Box::new(node));
    }
}

/// Give the `signature` of the pointy block `block` of a `for` loop
/// (`for @a -> $x, $y { }`) the ranges the parser recorded for its
/// parameters, and the signature the span from the first to the last.
// Cost: O(p + size of the body), p = parameters.
pub(super) fn for_pointy_block(
    names: &crate::scope::NameScope,
    params: &[ParamDef],
    body: &[crate::ast::Stmt],
) -> Result<RakuAstNode, crate::value::RuntimeError> {
    let mut block = super::view::pointy_block(names, params, body, None)?;
    place_signature(&mut block, params);
    Ok(block)
}

// Cost: O(p), p = parameters.
fn place_signature(block: &mut RakuAstNode, params: &[ParamDef]) {
    let (Some(first), Some(last)) = (params.first(), params.last()) else {
        return;
    };
    if !first.spans.param.is_some() || !last.spans.param.is_some() {
        return;
    }
    let whole = crate::ast::SrcSpan::new(
        first.spans.param.from as usize,
        last.spans.param.to as usize,
    );
    with_child(block, "signature", |signature| {
        parameters(signature, params);
        super::origin::set_span(signature, whole);
    });
}

/// Give a `Parameter`, its target and its type the origins of the text they
/// were written at.
// Cost: O(d), d = nesting of the type nodes.
pub(super) fn parameter(
    node: &mut RakuAstNode,
    param: crate::ast::SrcSpan,
    target_span: crate::ast::SrcSpan,
    type_span: crate::ast::SrcSpan,
) {
    super::origin::set_span(node, param);
    with_child(node, "target", |target| {
        super::origin::set_span(target, target_span);
        // `\x`: a `ParameterTarget::Term` over the term's `Name`.
        if target.class == RakuAstClass::ParameterTargetTerm
            && let Some(field) = target.fields.first_mut()
        {
            with_value(&mut field.value, |name| {
                super::origin::set_span(name, target_span)
            });
        }
    });
    with_child(node, "type", |ty| type_origin(ty, type_span));
}

/// Give a parameter's type, and the type and name nodes it wraps
/// (`Type::Definedness` over `Type::Simple` over `Name`), the range the type
/// was written at.
// Cost: O(d), d = nesting of the type nodes.
pub(super) fn type_origin(ty: &mut RakuAstNode, span: crate::ast::SrcSpan) {
    if !span.is_some() || ty.origin.0.is_some() {
        return;
    }
    super::origin::set_span(ty, span);
    // `Int:D`: the inner `Type::Simple` spans all of it, its `Name` only the
    // name before the smiley.
    let smiled = matches!(
        ty.class,
        RakuAstClass::TypeDefinedness | RakuAstClass::TypeAnyDefinedness
    );
    let name_end = smiled.then(|| smiley_start(span)).flatten();
    if (smiled || ty.class == RakuAstClass::TypeSimple)
        && let Some(field) = ty.fields.first_mut()
    {
        with_value(&mut field.value, |inner| type_origin(inner, span));
        if let Some(end) = name_end {
            with_value(&mut field.value, |simple| {
                if let Some(name) = simple.fields.first_mut() {
                    with_value(&mut name.value, |name| {
                        name.origin = Default::default();
                        super::origin::set_span(
                            name,
                            crate::ast::SrcSpan::new(span.from as usize, end),
                        )
                    });
                }
            });
        }
    }
}

/// Where the smiley (`:D`, `:U`, `:_`) of the type written at `span` starts.
// Cost: O(n), n = length of the written type.
fn smiley_start(span: crate::ast::SrcSpan) -> Option<usize> {
    super::unit_source::with_text(span, |written| {
        let at = written.len().checked_sub(2)?;
        let smiley = written.get(at..)?;
        matches!(smiley, ":D" | ":U" | ":_").then_some(span.from as usize + at)
    })
}

/// Apply `f` to the node held in `node`'s field `name`, if it holds one.
// Cost: O(f), f = fields of `node`.
fn with_child(node: &mut RakuAstNode, name: &str, f: impl FnOnce(&mut RakuAstNode)) {
    if let Some(field) = node.fields.iter_mut().find(|f| f.name == Some(name)) {
        with_value(&mut field.value, f);
    }
}

/// Apply `f` to the node a field value holds, if it holds one.
// Cost: O(1) plus `f`.
fn with_value(value: &mut RakuAstFieldValue, f: impl FnOnce(&mut RakuAstNode)) {
    let RakuAstFieldValue::Node(held) = value else {
        return;
    };
    let ValueView::RakuAst(node) = held.view() else {
        return;
    };
    let mut node = RakuAstNode::clone(node);
    f(&mut node);
    *held = Value::rakuast(Box::new(node));
}
