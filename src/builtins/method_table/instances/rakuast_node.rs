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
    // `$*CU.add-sorry($ex)` / `add-worry($ex)`: queue a compile-time
    // diagnostic, reported at the end of the unit's compile phase (#15666).
    node_row!("RakuAST::CompUnit", "add-sorry", |_t, a| {
        crate::unit_diagnostics::add(&a[0], false);
        Some(Ok(Value::NIL))
    }),
    node_row!("RakuAST::CompUnit", "add-worry", |_t, a| {
        crate::unit_diagnostics::add(&a[0], true);
        Some(Ok(Value::NIL))
    }),
    // `$*CU.sorries` / `worries` / `has-check-time-problems`: what was queued so far.
    MethodRow {
        owner: "RakuAST::CompUnit",
        name: "sorries",
        arity: 0,
        handler: Handler::Narrow(|_, _| Some(Ok(Value::array(crate::unit_diagnostics::peek().0)))),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::CompUnit",
        name: "worries",
        arity: 0,
        handler: Handler::Narrow(|_, _| Some(Ok(Value::array(crate::unit_diagnostics::peek().1)))),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::CompUnit",
        name: "has-check-time-problems",
        arity: 0,
        handler: Handler::Narrow(|_, _| {
            let (sorries, worries) = crate::unit_diagnostics::peek();
            Some(Ok(Value::truth(!sorries.is_empty() || !worries.is_empty())))
        }),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    node_row!(
        "RakuAST::Statement::Expression",
        "set-expression",
        |t, a| t.rakuast_set_field("set-expression", a[0].clone())
    ),
    // One row serves both classes (a name is declared once); the handler
    // refuses any other node.
    node_row!("RakuAST::ApplyInfix", "add-colonpair", |t, a| t
        .rakuast_add_colonpair(a[0].clone())),
    // `Declaration::External::*.compile-time-value`: what a core name
    // resolves to, read from the interpreter on demand (#15610).
    MethodRow {
        owner: "RakuAST::Declaration::External::Setting",
        name: "compile-time-value",
        arity: 0,
        handler: Handler::Interp(external_value),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Declaration::External::Constant",
        name: "compile-time-value",
        arity: 0,
        handler: Handler::Interp(external_value),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    // `RakuAST::Sub.compile-time-value`: the routine a resolved call of a unit
    // sub stands for (#15610), read from the interpreter on demand.
    MethodRow {
        owner: "RakuAST::Sub",
        name: "compile-time-value",
        arity: 0,
        handler: Handler::Interp(sub_value),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    // `RakuAST::Parameter.meta-object`: the `Parameter` the node declares
    // (`static::Analyzer` reads its type, `rw`, `copy`, ...; #15610).
    MethodRow {
        owner: "RakuAST::Parameter",
        name: "meta-object",
        arity: 0,
        handler: Handler::Interp(parameter_meta_object),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    // An imported routine (`use Test; plan`) and a unit-declared package.
    MethodRow {
        owner: "RakuAST::Declaration::Import",
        name: "compile-time-value",
        arity: 0,
        handler: Handler::Interp(external_value),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Declaration::LexicalPackage",
        name: "compile-time-value",
        arity: 0,
        handler: Handler::Interp(external_value),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    // `Origin.as-match`: an undocumented rakudo method that `static::CLI`'s
    // analysis calls (maintainer-approved, #15597): the span as a match-like
    // object carrying the file, line and offsets.
    origin_row!("RakuAST::Origin", "as-match", 0, origin_as_match),
    origin_row!("RakuAST::Origin::Match", "file", 0, match_file),
    origin_row!("RakuAST::Origin::Match", "line", 0, match_line),
    origin_row!("RakuAST::Origin::Match", "from", 0, origin_position),
    origin_row!("RakuAST::Origin::Match", "to", 0, origin_end),
    origin_row!("RakuAST::Origin::Match", "orig", 0, match_orig),
    super::narrow_row!("RakuAST::Origin::Match", "Str", 0, match_str),
    origin_row!("RakuAST::Origin::Match", "chars", 0, match_chars),
    origin_row!("RakuAST::Origin", "from", 0, origin_position),
    origin_row!("RakuAST::Origin", "to", 0, origin_end),
    origin_row!("RakuAST::Origin", "source", 0, origin_source),
    origin_row!("RakuAST::Origin::Source", "original-line", 1, original_line),
    origin_row!("RakuAST::Origin::Source", "orig", 0, source_orig),
    origin_row!(
        "RakuAST::Origin::Source",
        "prepost-of-pos",
        1,
        prepost_of_pos
    ),
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

/// `compile-time-value` of a core declaration stub: the core routine named by
/// its `lexical-name` (`&say`), or the type object of a core type.
// Cost: O(1) plus the interpreter's lookup of the name.
fn external_value(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    let name = attributes
        .as_map()
        .get("lexical-name")
        .map(|n| n.to_string_value())?;
    if let Some(routine) = name.strip_prefix('&') {
        return Some(Ok(interp.resolve_code_var(routine)));
    }
    Some(Ok(Value::package(Symbol::intern(&name))))
}

/// `compile-time-value` of a `RakuAST::Method` declared in a package of the
/// unit: the method object its class holds.
// Cost: O(1) plus the interpreter's method lookup.
fn method_value(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ValueView::RakuAst(node) = target.view() else {
        return None;
    };
    let (owner, name) = crate::rakuast::declared_method(node)?;
    Some(interp.call_method_with_values(
        Value::package(Symbol::intern(&owner)),
        "^find_method",
        vec![Value::str(name)],
    ))
}

/// `compile-time-value` of a unit-declared `RakuAST::Sub`: the routine of that name.
// Cost: O(1) plus the interpreter's lookup of the name.
fn sub_value(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ValueView::RakuAst(node) = target.view() else {
        return None;
    };
    // One row serves the declaration kinds (the first owner that declares a
    // name is the only one the walk asks).
    use crate::rakuast::RakuAstClass as K;
    match node.class {
        K::Method | K::Submethod => return method_value(interp, target, _args, _named),
        // A subset's type: declared now when it is an anonymous one.
        K::TypeSubset => {
            if let Some((statement, name)) = crate::rakuast::anonymous_subset_declaration(node) {
                if let Err(err) = interp.eval_rakuast_node(&statement) {
                    return Some(Err(err));
                }
                return Some(Ok(Value::package(Symbol::intern(&name))));
            }
            let name = node
                .origin
                .2
                .as_ref()
                .map(|n| n.to_string_value())
                .or_else(|| crate::rakuast::declared_package_name(node))?;
            // A named subset is declared when the unit runs; the analysis needs
            // its type (and `.^refinee`) at CHECK time, as rakudo has it.
            if !interp.registry().subsets.contains_key(&name) {
                let statement = crate::rakuast::as_statement(target);
                if let Err(err) = interp.eval_rakuast_node(&statement) {
                    return Some(Err(err));
                }
            }
            return Some(Ok(Value::package(Symbol::intern(&name))));
        }
        // A block's value is its closure.
        K::Block | K::PointyBlock => {
            return Some(interp.eval_rakuast_node(&crate::rakuast::as_item_call_statement(target)));
        }
        // `::T` stands for a type nobody has bound yet.
        K::TypeCapture => return Some(Ok(Value::package(Symbol::intern("Any")))),
        // A constant's value is its initializer's, evaluated now.
        K::VarDeclarationConstant => {
            let init = crate::rakuast::node_accessor(node, "initializer")?;
            let ValueView::RakuAst(init) = init.view() else {
                return None;
            };
            let expression = crate::rakuast::node_accessor(init, "expression")?;
            return Some(interp.eval_rakuast_node(&expression));
        }
        _ => {}
    }
    let Some(name) = crate::rakuast::declared_sub_name(node) else {
        // An anonymous sub is its own closure.
        return (node.class == K::Sub).then(|| interp.eval_rakuast_node(target));
    };
    let routine = interp.resolve_code_var(&name);
    // A CHECK phaser runs before the unit's subs are installed as lexicals,
    // but they are already in the function table (preregistered).
    if !matches!(routine.view(), ValueView::Sub(_))
        && !interp.has_proto(&name)
        && let Some(def) = interp.resolve_function(&name).map(|a| (*a).clone())
    {
        return Some(Ok(interp.sub_value_from_function_def(def)));
    }
    Some(Ok(routine))
}

/// `Parameter.meta-object` of a `RakuAST::Parameter` node.
// Cost: O(size of the parameter node).
fn parameter_meta_object(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ValueView::RakuAst(node) = target.view() else {
        return None;
    };
    // A parameter of a routine the unit declares is that routine's own
    // `Parameter`, so the analysis can match it with the signature it dispatches
    // over (`$routine.signature.params[$i] === $node.meta-object`).
    if let Some(own) = routine_parameter(interp, node) {
        return Some(Ok(own));
    }
    Some(crate::rakuast::parameter_param_def(node).map(|def| {
        crate::value::signature::make_parameter_value_from_param_def(&def, Some(&*interp))
    }))
}

/// The `Parameter` the declared routine holds for the node's position.
// Cost: O(p) for the routine's signature, p = its parameters.
fn routine_parameter(
    interp: &mut Interpreter,
    node: &crate::rakuast::RakuAstNode,
) -> Option<Value> {
    let tag = crate::rakuast::declared_parameter_owner(node)?;
    let index: usize = tag.last()?.parse().ok()?;
    let routine = match tag.first()?.as_str() {
        "sub" => {
            let name = tag.get(1)?;
            let routine = interp.resolve_code_var(name);
            if matches!(routine.view(), ValueView::Sub(_)) {
                routine
            } else {
                let def = interp.resolve_function(name).map(|a| (*a).clone())?;
                interp.sub_value_from_function_def(def)
            }
        }
        "method" => interp
            .call_method_with_values(
                Value::package(Symbol::intern(tag.get(1)?)),
                "^find_method",
                vec![Value::str(tag.get(2)?.clone())],
            )
            .ok()?,
        _ => return None,
    };
    let signature = interp
        .call_method_with_values(routine, "signature", Vec::new())
        .ok()?;
    let params = interp
        .call_method_with_values(signature, "params", Vec::new())
        .ok()?;
    let ValueView::Array(items, _) = params.view() else {
        return None;
    };
    let items = items.as_slice();
    // A method's signature starts with its invocant, which the node omits.
    let first_is_invocant = items.first().is_some_and(|first| {
        interp
            .call_method_with_values(first.clone(), "invocant", Vec::new())
            .is_ok_and(|v| v.truthy())
    });
    let offset = usize::from(tag.first().is_some_and(|k| k == "method") && first_is_invocant);
    items.get(index + offset).cloned()
}

/// `Origin.as-match`: a `RakuAST::Origin::Match` for the span: the unit's file,
/// the line the span begins on, its character offsets and the unit's text.
// Cost: O(1) plus a copy of the unit text handle.
fn origin_as_match(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    let origin = attributes.as_map();
    let mut match_attrs = std::collections::HashMap::new();
    for key in ["from", "to"] {
        match_attrs.insert(key.to_string(), origin.get(key).cloned()?);
    }
    match_attrs.insert(
        "line".to_string(),
        origin
            .get(crate::rakuast::unit_source::LINE_ATTR)
            .cloned()
            .unwrap_or_else(|| Value::int(1)),
    );
    let file = crate::unit_source_file::current()
        .map(|file| file.resolve().to_string())
        .unwrap_or_default();
    match_attrs.insert("file".to_string(), Value::str(file));
    if let Some(source) = origin.get("source")
        && let ValueView::Instance { attributes, .. } = source.view()
        && let Some(orig) = attributes
            .as_map()
            .get(crate::rakuast::unit_source::ORIG_ATTR)
    {
        match_attrs.insert("orig".to_string(), orig.clone());
    }
    Some(Ok(Value::make_instance(
        Symbol::intern("RakuAST::Origin::Match"),
        match_attrs,
    )))
}

/// A `Match`-shaped origin's attribute, by name.
// Cost: O(1).
fn match_attr(target: &Value, name: &str) -> Option<Value> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    attributes.as_map().get(name).cloned()
}

/// `Match.file`.
// Cost: O(1).
fn match_file(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match_attr(target, "file").map(Ok)
}

/// `Match.line`: the 1-based line the span begins on.
// Cost: O(1).
fn match_line(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match_attr(target, "line").map(Ok)
}

/// `Match.orig`: the whole unit text.
// Cost: O(1).
fn match_orig(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match_attr(target, "orig").map(Ok)
}

/// `Match.Str`: the text of the span.
// Cost: O(n), n = chars of the unit up to the span's end.
fn match_str(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let (ValueView::Int(from), ValueView::Int(to)) = (
        match_attr(target, "from")?.view(),
        match_attr(target, "to")?.view(),
    ) else {
        return None;
    };
    let orig = match_attr(target, "orig")?.to_string_value();
    let text: String = orig
        .chars()
        .skip(from.max(0) as usize)
        .take((to - from).max(0) as usize)
        .collect();
    Some(Ok(Value::str(text)))
}

/// `Match.chars`: the length of the span.
// Cost: O(1).
fn match_chars(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let (ValueView::Int(from), ValueView::Int(to)) = (
        match_attr(target, "from")?.view(),
        match_attr(target, "to")?.view(),
    ) else {
        return None;
    };
    Some(Ok(Value::int((to - from).max(0))))
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

/// How many characters of the line `prepost-of-pos` shows on each side.
const PREPOST_CHARS: usize = 40;

/// `Origin::Source.prepost-of-pos($position)`: the text before and after the
/// character offset `$position`, each at most 40 characters and cut at the line
/// boundary, with `<BOL>` / `<EOL>` standing for an empty side. This is the
/// excerpt a `===SORRY!===` shows around `<HERE>`.
// Cost: O(n), n = length of the unit's text.
fn prepost_of_pos(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Int(pos) = args.first()?.view() else {
        return None;
    };
    let orig = source_orig(target, &[])?.ok()?.to_string_value();
    let chars: Vec<char> = orig.chars().collect();
    let pos = (pos.max(0) as usize).min(chars.len());
    let before = &chars[pos.saturating_sub(PREPOST_CHARS)..pos];
    let pre: String = before
        .iter()
        .skip(before.iter().rposition(|c| *c == '\n').map_or(0, |i| i + 1))
        .collect();
    let post: String = chars[pos..chars.len().min(pos + PREPOST_CHARS)]
        .iter()
        .take_while(|c| **c != '\n')
        .collect();
    let side = |text: String, empty: &str| {
        Value::str(if text.is_empty() {
            empty.to_string()
        } else {
            text
        })
    };
    Some(Ok(Value::array(vec![
        side(pre, "<BOL>"),
        side(post, "<EOL>"),
    ])))
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
        RakuAstClass::ParameterTargetVar => Some(vec![
            node_value("declaration")
                .or_else(|| crate::rakuast::annotated_target_declaration(node))
                .unwrap_or_else(|| rakuast(target_declaration(node), Vec::new(), &node.origin)),
        ]),
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
            let declaration = crate::rakuast::annotated_target_declaration(target_node)
                .unwrap_or_else(|| {
                    rakuast(
                        target_declaration(target_node),
                        vec![RakuAstField {
                            name: Some("type"),
                            value: RakuAstFieldValue::Node(explicit_type),
                        }],
                        &target_node.origin,
                    )
                });
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
