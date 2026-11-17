//! `compile-time-value` of a named `RakuAST::Sub` the CHECK phase cannot find
//! yet: a `sub` declared inside a nested block (Red t/091's
//! `sub increment($value) { ... }` inside a subtest block).
//!
//! rakudo answers the routine's static code object, which every closure of the
//! declaration clones: `$code.static_id` equals the `static_id` of the `&name`
//! the block later binds. mutsu installs such a routine when its block runs, so
//! at CHECK time it is neither a lexical nor in the function table. The value
//! here is the routine compiled from the node itself, as an anonymous sub (so
//! nothing is installed), carrying the declaration's identity: its name, the
//! unit's package, file and declaration line. `Code.static_id` keys a named
//! routine on exactly those (`code::static_id`), so Red's
//! `validate-code($actual, $expected)` compares it equal to the routine the
//! block binds.

use crate::rakuast::{RakuAstClass, RakuAstNode};
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

/// The routine a named `Sub` node declares, built from the node for a
/// CHECK-time `compile-time-value` (see the module docs). `None` when the node
/// is no plain named sub, or its declaration cannot be compiled on its own here
/// (its body reads a lexical of the block it is declared in): the caller then
/// answers as before.
// Cost: O(n) to compile the declaration, n = its size.
pub(super) fn check_time_routine(
    interp: &mut Interpreter,
    node: &RakuAstNode,
    name: &str,
) -> Option<Result<Value, RuntimeError>> {
    // A `proto` declares the dispatcher itself, which this cannot build; a
    // `multi` candidate is built like a plain sub (`declared_candidate`).
    if node.class != RakuAstClass::Sub || multiness(node).as_deref() == Some("proto") {
        return None;
    }
    let origin = crate::rakuast::node_accessor(node, "origin");
    let origin_attr = |key: &str| match origin.as_ref()?.view() {
        ValueView::Instance { attributes, .. } => attributes.as_map().get(key).cloned(),
        _ => None,
    };
    let line = declaration_line(node);
    // The declaration is the unit's own code, already judged with it,
    // whichever module's CHECK-time code asks (Red's `red-compile`).
    let _recompile = crate::unit_source_file::UnitDeclarationRecompileGuard::enter();
    let closure = if node.fields.iter().any(|f| f.name == Some("body")) {
        // The same declaration without its name (and multiness) is an
        // anonymous sub, as an expression statement (a statement-level `Sub` is
        // a declaration): evaluating it installs no `&name` in the scope that
        // asks.
        let mut anonymous = node.clone();
        anonymous
            .fields
            .retain(|field| !matches!(field.name, Some("name" | "multiness")));
        let statement = crate::rakuast::as_statement(&Value::rakuast(Box::new(anonymous)));
        interp.eval_rakuast_node(&statement).ok()?
    } else {
        // A resolution's declaration is held without its body (the resolver
        // keeps no copy of it): compile the declaration's own source text,
        // as an `anon sub`.
        let text = declaration_text(
            &origin_attr("source")?,
            &origin_attr("from")?,
            &origin_attr("to")?,
        )?;
        interp.eval_code_string(&format!("anon {text}")).ok()?
    };
    let ValueView::Sub(data) = closure.view() else {
        return None;
    };
    let mut routine = crate::value::SubData::clone(&data);
    routine.name = Symbol::intern(name);
    // The unit's own package: a `unit module X` file declares its
    // routines in `X`, as the restarted unit's copy does (#17886).
    routine.package = Symbol::intern(unit_package().as_deref().unwrap_or("GLOBAL"));
    routine.source_file = crate::unit_source_file::current().map(|file| file.resolve().to_string());
    routine.source_line = line;
    routine.source_file_sym_cache = Default::default();
    Some(Ok(Value::sub_value(crate::gc::Gc::new(routine))))
}

/// The package the active unit declares with a `unit` package declarator
/// (`unit module X;`), whose body is the rest of the unit.
// Cost: O(s), s = top-level statements of the unit (the declarator is first).
fn unit_package() -> Option<String> {
    let node_of = |value: &Value| match value.view() {
        ValueView::RakuAst(node) => Some(node.clone()),
        _ => None,
    };
    crate::unit_source_text::with_active_cu(|cu| {
        let cu = node_of(cu)?;
        let list = node_of(&crate::rakuast::node_accessor(&cu, "statement-list")?)?;
        let statements = crate::rakuast::node_accessor(&list, "statements")?;
        let ValueView::Array(items, _) = statements.view() else {
            return None;
        };
        items.iter().find_map(|statement| {
            let statement = node_of(statement)?;
            let declarator = node_of(&crate::rakuast::node_accessor(&statement, "expression")?)?;
            let scope = crate::rakuast::node_accessor(&declarator, "scope")?;
            if scope.to_string_value() != "unit" {
                return None;
            }
            let name = node_of(&crate::rakuast::node_accessor(&declarator, "name")?)?;
            Some(crate::rakuast::node_accessor(&name, "canonicalize")?.to_string_value())
        })
    })
}

/// The dispatcher a scope's `proto` stub stands for at CHECK time, when its
/// scope (a nested block) has not run and installed it: a dispatcher over the
/// scope's candidates, each built from its node as [`check_time_routine`]
/// builds one (#17826). rakudo answers the scope's dispatcher there
/// (`is_dispatcher` True, its candidates). `None` when the stub carries no
/// candidates or one cannot be built here.
// Cost: O(c * n), c = candidates, n = the size of each declaration compiled.
pub(super) fn check_time_dispatcher(
    interp: &mut Interpreter,
    node: &RakuAstNode,
    name: &str,
) -> Option<Result<Value, RuntimeError>> {
    let candidates = crate::rakuast::dispatcher_stub_candidates(node)?;
    let mut built = Vec::with_capacity(candidates.len());
    for candidate in &candidates {
        let ValueView::RakuAst(candidate) = candidate.view() else {
            return None;
        };
        match check_time_routine(interp, candidate, name)? {
            Ok(routine) => built.push(routine),
            Err(err) => return Some(Err(err)),
        }
    }
    if built.is_empty() {
        return None;
    }
    Some(Ok(family_with_candidates(interp, None, name, built)))
}

/// The CHECK-time value of a derived family: the proto rakudo derives for a
/// scope's `multi` candidates of a name the outer scope or the setting already
/// has a dispatcher for (a `VarDeclaration::Implicit::Constant` resolution,
/// #17826). It holds the outer family's candidates, then the scope's own, each
/// built from its node as [`check_time_routine`] builds one. `None` for any
/// other node, or when a candidate cannot be built here.
// Cost: O(o + c * n), o = candidates of the outer family, c = the scope's
// candidates, n = the size of each declaration compiled.
pub(super) fn check_time_family(
    interp: &mut Interpreter,
    node: &RakuAstNode,
) -> Option<Result<Value, RuntimeError>> {
    let (outer, candidates) = crate::rakuast::derived_family_parts(node)?;
    let name = node.fields.iter().find_map(|field| match &field.value {
        crate::rakuast::RakuAstFieldValue::Node(value) if field.name == Some("name") => {
            Some(value.to_string_value())
        }
        _ => None,
    })?;
    let name = name.strip_prefix('&').unwrap_or(&name).to_string();
    let mut built = Vec::with_capacity(candidates.len());
    for candidate in &candidates {
        let ValueView::RakuAst(candidate) = candidate.view() else {
            return None;
        };
        match check_time_routine(interp, candidate, &name)? {
            Ok(routine) => built.push(routine),
            Err(err) => return Some(Err(err)),
        }
    }
    let family = if outer.is_nil() {
        None
    } else {
        match super::rakuast_lookup_values::resolution_value(interp, &outer)? {
            Ok(family) => Some(family),
            Err(err) => return Some(Err(err)),
        }
    };
    Some(Ok(family_with_candidates(interp, family, &name, built)))
}

/// `family` (the outer dispatcher of `name`, if any) with the `built`
/// candidates added after its own. An outer family that holds its candidates
/// (a user `multi`'s dispatcher) gets them added; otherwise (none, or a
/// setting routine that dispatches by itself) the value is a dispatcher over
/// `built`, falling back to the setting's operator as mutsu's run-time family
/// of a block-local operator candidate does.
// Cost: O(o + c), o = candidates of the outer family, c = built candidates.
fn family_with_candidates(
    interp: &Interpreter,
    family: Option<Value>,
    name: &str,
    built: Vec<Value>,
) -> Value {
    if let Some(family) = &family
        && let ValueView::Sub(data) = family.view()
        && data.env.contains_key("__mutsu_multi_dispatch_candidates")
    {
        return built.iter().fold(family.clone(), |family, candidate| {
            interp.dispatcher_containing(family, candidate)
        });
    }
    let Some(first) = built.first() else {
        return family.unwrap_or(Value::NIL);
    };
    let ValueView::Sub(first) = first.view() else {
        return family.unwrap_or(Value::NIL);
    };
    let mut dispatcher = crate::value::SubData::clone(&first);
    dispatcher.env.insert(
        "__mutsu_multi_dispatch_candidates".to_string(),
        Value::array_with_kind(
            crate::gc::Gc::new(crate::value::ArrayData::new(built)),
            crate::value::ArrayKind::List,
        ),
    );
    dispatcher.env.insert(
        "__mutsu_multi_dispatch_name".to_string(),
        Value::str(name.to_string()),
    );
    let dispatcher = Value::sub_value(crate::gc::Gc::new(dispatcher));
    if family.is_some() {
        Interpreter::with_builtin_operator_fallback(dispatcher)
    } else {
        dispatcher
    }
}

/// The `multiness` a routine declaration node spells (`multi`, `proto`).
// Cost: O(f), f = fields of the node.
fn multiness(node: &RakuAstNode) -> Option<String> {
    node.fields.iter().find_map(|field| match &field.value {
        crate::rakuast::RakuAstFieldValue::Node(value) if field.name == Some("multiness") => {
            Some(value.to_string_value())
        }
        _ => None,
    })
}

/// Whether the routine declaration node declares a `multi` candidate.
// Cost: O(f), f = fields of the node.
pub(super) fn declares_multi(node: &RakuAstNode) -> bool {
    multiness(node).as_deref() == Some("multi")
}

/// The line of the unit a declaration node starts on, from its origin.
// Cost: O(1).
fn declaration_line(node: &RakuAstNode) -> Option<u32> {
    let origin = crate::rakuast::node_accessor(node, "origin")?;
    let ValueView::Instance { attributes, .. } = origin.view() else {
        return None;
    };
    let line = attributes
        .as_map()
        .get(crate::rakuast::unit_source::LINE_ATTR)
        .cloned()?;
    u32::try_from(crate::value::numeric_coerce::to_int(&line)).ok()
}

/// The candidate a `multi sub` node declares, of the dispatcher `routine` its
/// name has where the CHECK phase asks: the one compiled from this declaration
/// (the unit's file, the declaration's line). A candidate of a block-nested
/// `multi` is installed only when its block runs, and an operator's dispatcher
/// also holds the setting's candidates, so the position among the unit's own
/// `multi`s (`index`) picks it only when the dispatcher holds nothing else.
/// When it is not installed yet, it is built from the node
/// (`check_time_routine`), as rakudo's static code object of the candidate.
// Cost: O(c + n), c = candidates of the dispatcher, n = size of the declaration.
pub(super) fn declared_candidate(
    interp: &mut Interpreter,
    node: &RakuAstNode,
    name: &str,
    routine: &Value,
    index: Option<usize>,
) -> Option<Result<Value, RuntimeError>> {
    let line = declaration_line(node);
    let file = crate::unit_source_file::current().map(|file| file.resolve().to_string());
    if let Ok(candidates) =
        interp.call_method_with_values(routine.clone(), "candidates", Vec::new())
        && let ValueView::Array(items, _) = candidates.view()
    {
        let items = items.as_slice().to_vec();
        // A candidate's position is the one `.line`/`.file` report.
        let mut position = |candidate: &Value| {
            let mut at = |what| {
                interp
                    .code_line_file(candidate, what)
                    .and_then(Result::ok)
                    .filter(|v| !v.is_nil())
            };
            let line = at("line").map(|l| crate::value::numeric_coerce::to_int(&l));
            (line, at("file").map(|f| f.to_string_value()))
        };
        let positions: Vec<_> = items.iter().map(&mut position).collect();
        let here = line.map(i64::from);
        if let Some(found) = positions
            .iter()
            .position(|(l, f)| here.is_some() && *l == here && *f == file)
        {
            return Some(Ok(items[found].clone()));
        }
        // No candidate is at the declaration's line (a top-level `multi`'s
        // candidate reports none): the unit's own dispatcher holds its
        // candidates in declaration order, so the one at the declaration's
        // index is it -- when that one is the unit's, not the setting's or
        // an imported module's.
        if let Some(index) = index
            && let Some((_, candidate_file)) = positions.get(index)
            && (candidate_file.is_none() || *candidate_file == file)
        {
            return Some(Ok(items[index].clone()));
        }
    }
    check_time_routine(interp, node, name)
}

/// The source text of a `sub` declaration, from its origin's unit text and
/// character range, without a leading `my`/`our` scope or `multi`: `None`
/// when the range is not a `sub` declaration.
// Cost: O(t), t = characters of the unit text up to the range's end.
fn declaration_text(source: &Value, from: &Value, to: &Value) -> Option<String> {
    let ValueView::Instance { attributes, .. } = source.view() else {
        return None;
    };
    let orig = attributes
        .as_map()
        .get(crate::rakuast::unit_source::ORIG_ATTR)?
        .to_string_value();
    let from = usize::try_from(crate::value::numeric_coerce::to_int(from)).ok()?;
    let to = usize::try_from(crate::value::numeric_coerce::to_int(to)).ok()?;
    let text: String = orig
        .chars()
        .skip(from)
        .take(to.checked_sub(from)?)
        .collect();
    let text = text.trim();
    let text = text
        .strip_prefix("my ")
        .or_else(|| text.strip_prefix("our "))
        .map_or(text, str::trim_start);
    // `multi sub f` and `multi f` declare a candidate: built as a plain sub.
    if let Some(candidate) = text.strip_prefix("multi ").map(str::trim_start) {
        return Some(match candidate.strip_prefix("sub ") {
            Some(_) => candidate.to_string(),
            None => format!("sub {candidate}"),
        });
    }
    text.starts_with("sub ").then(|| text.to_string())
}
