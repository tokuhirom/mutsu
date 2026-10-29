//! `RakuAST::Node.visit-children` (ADR-11276 §9, slice 3D).
//!
//! A RakuAST node is the opaque shape `RakuAstNode`, and `RakuAST::Origin` and
//! `RakuAST::Origin::Source` instances are opaque shapes of their own
//! (`value/dispatch_shape/opaque.rs`); the rows are `OWNER_ONLY` under the
//! owners those shapes name.
//!
//! The same file holds the rows of `RakuAST::Origin` (a node's `.origin`) and
//! `RakuAST::Origin::Source` (its `.source`). A statement node records only
//! the range it occupies: `from` and `to` are character offsets into the
//! unit's source, which `.source` answers, and `Source.original-line` finds the
//! line of an offset by binary search (ADR-10723 §4; `rakuast::unit_source`).

use super::super::{Handler, MethodRow, Named, RowFlags};
use crate::rakuast::{RakuAstFieldValue, RakuAstNode};
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! origin_row {
    ($owner:literal, $name:literal, $arity:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: Handler::Narrow($handler),
            flags: RowFlags::OWNER_ONLY,
            named: &[],
        }
    };
}

/// A one-argument row of the node class `$owner` that takes any argument (a
/// node).
macro_rules! node_row {
    ($owner:literal, $name:literal, $handler:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 1,
            handler: Handler::Narrow($handler),
            flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    // `.DEPARSE(*@roles)`: rakudo's own `RakuAST::Deparse`, run as Raku code
    // (ADR-14845); `run_prelude_deparse.rs` declares the routine called here.
    MethodRow {
        owner: "RakuAST::Node",
        name: "DEPARSE",
        arity: 0,
        handler: Handler::Interp(deparse),
        flags: RowFlags::OWNER_ONLY
            .or(RowFlags::ANY_ARGS)
            .or(RowFlags::SLURPY),
        named: &[],
    },
    // `.fileline`: an undocumented rakudo method that `static::CLI`'s analysis
    // reads (maintainer-approved, #15531): `"<file>:<line>"` of the node's
    // origin, `Nil` for a node without one.
    origin_row!("RakuAST::Node", "fileline", 0, fileline),
    MethodRow {
        owner: "RakuAST::Node",
        name: "visit-children",
        arity: 1,
        handler: Handler::Interp(visit_children),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    // The builders that change a node in place (`$list.add-statement($s)`):
    // each answers only the node class Rakudo declares it on
    // (`Value::rakuast_add_child`, `Value::rakuast_set_field`).
    node_row!("RakuAST::StatementList", "add-statement", |t, a| t
        .rakuast_add_child("add-statement", a[0].clone())),
    node_row!("RakuAST::StatementList", "unshift-statement", |t, a| t
        .rakuast_add_child("unshift-statement", a[0].clone())),
    node_row!("RakuAST::ArgList", "push", |t, a| t
        .rakuast_add_child("push", a[0].clone())),
    node_row!("RakuAST::CompUnit", "replace-statement-list", |t, a| t
        .rakuast_set_field("replace-statement-list", a[0].clone())),
    // `$*CU.add-check-code({ ... })`: an undocumented rakudo method that
    // `static::CLI` calls (maintainer-approved, #15513); queues the closure
    // for the unit's CHECK phase.
    node_row!("RakuAST::CompUnit", "add-check-code", |t, a| {
        crate::unit_source_text::add_check_code(t.clone(), a[0].clone());
        Some(Ok(Value::NIL))
    }),
    node_row!(
        "RakuAST::Statement::Expression",
        "set-expression",
        |t, a| t.rakuast_set_field("set-expression", a[0].clone())
    ),
    // One row serves both classes (a name is declared once); the handler
    // refuses any other node.
    node_row!("RakuAST::ApplyInfix", "add-colonpair", |t, a| t
        .rakuast_add_colonpair(a[0].clone())),
    origin_row!("RakuAST::Origin", "from", 0, origin_position),
    origin_row!("RakuAST::Origin", "to", 0, origin_end),
    origin_row!("RakuAST::Origin", "source", 0, origin_source),
    origin_row!("RakuAST::Origin::Source", "original-line", 1, original_line),
    origin_row!("RakuAST::Origin::Source", "orig", 0, source_orig),
];

/// `Node.fileline`: `"<file>:<line>"` where the node's origin begins. The unit
/// itself starts on line 1; a node built by hand has no origin and answers
/// `Nil`, as in rakudo.
// Cost: O(f), f = fields of the node.
fn fileline(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let base = match target.view() {
        ValueView::RakuAst(_) => target.clone(),
        ValueView::Instance { attributes, .. } => attributes.rakuast_base()?,
        _ => return None,
    };
    let ValueView::RakuAst(node) = base.view() else {
        return None;
    };
    let line = if node.class == crate::rakuast::RakuAstClass::CompUnit {
        Some(1)
    } else {
        crate::rakuast::origin_line_of(node)
    };
    let Some(line) = line else {
        return Some(Ok(Value::NIL));
    };
    let file = crate::unit_source_file::current()
        .map(|file| file.resolve().to_string())
        .unwrap_or_default();
    Some(Ok(Value::str(format!("{file}:{line}"))))
}

/// `Origin.from` / `Origin.to`: the character range the node occupies.
// Cost: O(1).
fn origin_position(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let _ = args;
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    attributes.as_map().get("from").cloned().map(Ok)
}

/// `Origin.to`.
// Cost: O(1).
fn origin_end(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    attributes.as_map().get("to").cloned().map(Ok)
}

/// `Origin.source`: the unit source an origin's positions index.
// Cost: O(1).
fn origin_source(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if let ValueView::Instance { attributes, .. } = target.view()
        && let Some(source) = attributes.as_map().get("source")
    {
        return Some(Ok(source.clone()));
    }
    Some(Ok(Value::make_instance(
        Symbol::intern("RakuAST::Origin::Source"),
        std::collections::HashMap::new(),
    )))
}

/// `Origin::Source.orig`: the text of the unit.
// Cost: O(1).
fn source_orig(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    attributes
        .as_map()
        .get(crate::rakuast::unit_source::ORIG_ATTR)
        .cloned()
        .map(Ok)
}

/// `Origin::Source.original-line($position)`: the 1-based line the character
/// offset `$position` is on, by binary search over the unit's line starts.
/// A source without an index (one built by hand) answers the position itself.
// Cost: O(log n), n = lines of the unit.
fn original_line(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Int(pos) = args.first()?.view() else {
        return None;
    };
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    let map = attributes.as_map();
    let Some(starts) = map.get(crate::rakuast::unit_source::LINE_STARTS_ATTR) else {
        return Some(Ok(args[0].clone()));
    };
    let ValueView::Array(starts, _) = starts.view() else {
        return Some(Ok(args[0].clone()));
    };
    let line = starts
        .as_slice()
        .partition_point(|start| matches!(start.view(), ValueView::Int(s) if s <= pos));
    Some(Ok(Value::int(line.max(1) as i64)))
}

/// The child nodes of `node`, in field order: every field value that is itself
/// a RakuAST node, and every such element of a list field (a conditional's
/// `elsifs` clauses contribute their own children). Leaf payloads (a
/// name, a number) and the hidden `origin` are not nodes.
// Cost: O(f + c), f = fields of `node`, c = child nodes listed in them.
pub(super) fn children(node: &RakuAstNode) -> Vec<Value> {
    // A field Rakudo fills with an empty node is visited too.
    if let Some(filled) = crate::rakuast::empty_fields::materialized(node) {
        return children(&filled);
    }
    if let Some(shaped) = rakudo_shaped_children(node) {
        return shaped;
    }
    field_children(node)
}

/// [`children`] for the nodes whose children are exactly their node-valued
/// fields.
// Cost: O(f + c), f = fields of `node`, c = child nodes listed in them.
fn field_children(node: &RakuAstNode) -> Vec<Value> {
    let mut out = Vec::new();
    // Rakudo's loops visit their condition before the body, whatever order the
    // constructor takes them in (`repeat { } while COND` has `body` first).
    let repeat = matches!(
        node.class,
        crate::rakuast::RakuAstClass::StatementLoopRepeatWhile
            | crate::rakuast::RakuAstClass::StatementLoopRepeatUntil
    );
    let visited = node
        .fields
        .iter()
        .filter(|field| !crate::rakuast::is_unvisited_field(node, field));
    let fields: Vec<_> = if repeat {
        let (condition, rest): (Vec<_>, Vec<_>) =
            visited.partition(|f| f.name == Some("condition"));
        condition.into_iter().chain(rest).collect()
    } else {
        visited.collect()
    };
    for field in fields {
        match &field.value {
            RakuAstFieldValue::Node(v) if crate::rakuast::is_node_value(v) => {
                out.push(v.clone());
            }
            // Rakudo's conditionals visit each `elsif` / `orwith` clause's
            // condition and block directly, never the clause node itself.
            RakuAstFieldValue::List(items) if field.name == Some("elsifs") => {
                for item in items {
                    if let ValueView::RakuAst(clause) = item.view() {
                        out.extend(children(clause));
                    }
                }
            }
            RakuAstFieldValue::List(items) => out.extend(
                items
                    .iter()
                    .filter(|v| crate::rakuast::is_node_value(v))
                    .cloned(),
            ),
            _ => {}
        }
    }
    out
}

/// Rakudo's `visit-children` shape for the three nodes whose children are not
/// simply their field values (#14696):
///
/// - `ApplyInfix` visits its `Infix`, then an `ArgList` holding both operands
///   (mutsu stores `left`, `infix`, `right`);
/// - `ParameterTarget::Var` visits the `VarDeclaration::Simple` it declares,
///   which carries the parameter's explicit type (the `Parameter` hands that
///   down in a `declaration` field of the target it visits);
/// - `VarDeclaration::Simple` visits no `desigilname`.
// Cost: O(f), f = fields of `node`.
fn rakudo_shaped_children(node: &RakuAstNode) -> Option<Vec<Value>> {
    use crate::rakuast::{RakuAstClass, RakuAstField};
    let node_value = |name: &str| {
        node.fields.iter().find_map(|f| match &f.value {
            RakuAstFieldValue::Node(v)
                if f.name == Some(name) && crate::rakuast::is_node_value(v) =>
            {
                Some(v.clone())
            }
            _ => None,
        })
    };
    // The declaration a parameter's target visits is written where the
    // target is, so it has the target's range.
    let rakuast = |class, fields: Vec<RakuAstField>, origin: &crate::rakuast::NodeOrigin| {
        Value::rakuast(Box::new(RakuAstNode {
            class,
            fields,
            origin: origin.clone(),
        }))
    };
    match node.class {
        RakuAstClass::ApplyInfix => {
            let infix = node_value("infix")?;
            let (left, right) = (node_value("left")?, node_value("right")?);
            let args = [left, right]
                .into_iter()
                .map(|value| RakuAstField {
                    name: None,
                    value: RakuAstFieldValue::Node(value),
                })
                .collect();
            // Rakudo's `ArgList` of an infix application spans it.
            let arg_list = Value::rakuast(Box::new(RakuAstNode {
                class: RakuAstClass::ArgList,
                fields: args,
                origin: node.origin.clone(),
            }));
            let mut out = vec![infix, arg_list];
            out.extend(
                node.fields
                    .iter()
                    .filter(|f| f.name == Some("colonpairs"))
                    .filter_map(|f| match &f.value {
                        RakuAstFieldValue::List(items) => Some(items.iter().cloned()),
                        _ => None,
                    })
                    .flatten()
                    .filter(crate::rakuast::is_node_value),
            );
            Some(out)
        }
        RakuAstClass::ParameterTargetVar => {
            Some(vec![node_value("declaration").unwrap_or_else(|| {
                rakuast(target_declaration(node), Vec::new(), &node.origin)
            })])
        }
        // Rakudo visits a quoted regex's adverbs before its body.
        RakuAstClass::QuotedRegex | RakuAstClass::QuotedRegexRx
            if node.fields.iter().any(|f| f.name == Some("adverbs")) =>
        {
            let mut rest = node.clone();
            let (adverbs, mut fields): (Vec<_>, Vec<_>) = rest
                .fields
                .into_iter()
                .partition(|f| f.name == Some("adverbs"));
            let at = fields
                .iter()
                .position(|f| f.name == Some("body"))
                .unwrap_or(0);
            fields.splice(at..at, adverbs);
            rest.fields = fields;
            Some(field_children(&rest))
        }
        RakuAstClass::VarDeclarationSimple => {
            let mut rest = node.clone();
            rest.fields.retain(|f| f.name != Some("desigilname"));
            // Rakudo visits the traits after the initializer.
            let (traits, mut fields): (Vec<_>, Vec<_>) = rest
                .fields
                .into_iter()
                .partition(|f| f.name == Some("traits"));
            fields.extend(traits);
            rest.fields = fields;
            Some(field_children(&rest))
        }
        RakuAstClass::Parameter => {
            // An explicit type (a `Type::Simple`, not the implicit `Any`
            // setting type) belongs to the declaration of the target too.
            let explicit_type = node_value("type").filter(|t| {
                matches!(t.view(), ValueView::RakuAst(n) if n.class == RakuAstClass::TypeSimple)
            })?;
            let target = node_value("target")?;
            let ValueView::RakuAst(target_node) = target.view() else {
                return None;
            };
            if target_node.class != RakuAstClass::ParameterTargetVar {
                return None;
            }
            let declaration = rakuast(
                target_declaration(target_node),
                vec![RakuAstField {
                    name: Some("type"),
                    value: RakuAstFieldValue::Node(explicit_type),
                }],
                &target_node.origin,
            );
            let mut new_target = target_node.clone();
            new_target.fields.push(RakuAstField {
                name: Some("declaration"),
                value: RakuAstFieldValue::Node(declaration),
            });
            let mut param = node.clone();
            for field in &mut param.fields {
                if field.name == Some("target") {
                    field.value =
                        RakuAstFieldValue::Node(Value::rakuast(Box::new(new_target.clone())));
                }
            }
            Some(field_children(&param))
        }
        _ => None,
    }
}

/// The class of the declaration a `ParameterTarget::Var` visits: an
/// anonymous target (`$`, `@`) declares no name.
// Cost: O(f), f = fields of `target`.
fn target_declaration(target: &RakuAstNode) -> crate::rakuast::RakuAstClass {
    let anonymous = target.fields.iter().any(|f| {
        f.name == Some("name")
            && matches!(&f.value, RakuAstFieldValue::Node(name)
                if matches!(name.view(), ValueView::Str(s) if matches!(s.as_str(), "$" | "@" | "%" | "&")))
    });
    if anonymous {
        crate::rakuast::RakuAstClass::VarDeclarationAnonymous
    } else {
        crate::rakuast::RakuAstClass::VarDeclarationSimple
    }
}

/// `visit-children(&callable)`: call `callable` once with each direct child.
// Cost: O(f + c) plus c calls of the callable, f = fields of the node, c = its
// child nodes.
fn visit_children(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let base = match target.view() {
        ValueView::RakuAst(_) => target.clone(),
        ValueView::Instance { attributes, .. } => attributes.rakuast_base()?,
        _ => return None,
    };
    let ValueView::RakuAst(node) = base.view() else {
        return None;
    };
    let callable = args.first()?;
    for child in children(node) {
        if let Err(e) = interp.call_sub_value(callable.clone(), vec![child]) {
            return Some(Err(e));
        }
    }
    Some(Ok(Value::NIL))
}

/// `DEPARSE(*@roles)`: hand the node and the roles to the deparser routine.
// Cost: O(1) plus the deparser's own cost, linear in the tree it prints.
fn deparse(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let mut call_args = Vec::with_capacity(args.len() + 1);
    call_args.push(target.clone());
    call_args.extend(args.iter().cloned());
    Some(interp.call_function("MUTSU_DEPARSE::run", call_args))
}
