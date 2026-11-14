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
    // A `multi`/`proto` declaration answers its dispatcher's candidate
    // (`declared_multi_candidate`), which this cannot build.
    if node.class != RakuAstClass::Sub || node.fields.iter().any(|f| f.name == Some("multiness")) {
        return None;
    }
    let origin = crate::rakuast::node_accessor(node, "origin");
    let origin_attr = |key: &str| match origin.as_ref()?.view() {
        ValueView::Instance { attributes, .. } => attributes.as_map().get(key).cloned(),
        _ => None,
    };
    let line = origin_attr(crate::rakuast::unit_source::LINE_ATTR)
        .map(|line| crate::value::numeric_coerce::to_int(&line))
        .and_then(|line| u32::try_from(line).ok());
    let closure = if node.fields.iter().any(|f| f.name == Some("body")) {
        // The same declaration without its name is an anonymous sub, as an
        // expression statement (a statement-level `Sub` is a declaration):
        // evaluating it installs no `&name` in the scope that asks.
        let mut anonymous = node.clone();
        anonymous.fields.retain(|field| field.name != Some("name"));
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
    routine.package = Symbol::intern("GLOBAL");
    routine.source_file = crate::unit_source_file::current().map(|file| file.resolve().to_string());
    routine.source_line = line;
    routine.source_file_sym_cache = Default::default();
    Some(Ok(Value::sub_value(crate::gc::Gc::new(routine))))
}

/// The source text of a `sub` declaration, from its origin's unit text and
/// character range, without a leading `my`/`our` scope: `None` when the range
/// is not a `sub` declaration.
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
    text.starts_with("sub ").then(|| text.to_string())
}
