//! `Code`'s `of`, `returns`, `arity` and `count` (ADR-12523, slices 1 and 2).
//!
//! A code object is a `Sub` (a routine, closure or block, with or without a
//! body of its own), a `&name` handle on a routine reached through the
//! registry, or a regex. All of them are `Code` and answer the declared return
//! type, `Mu` without one. They have no dispatch shape, so the rows are
//! reached through their owner: [`answer`] is the entry for the callable
//! dispatch and for the regex fallback.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($name:literal, $handler:ident) => {
        MethodRow {
            owner: "Code",
            name: $name,
            arity: 0,
            handler: Handler::Interp($handler),
            flags: RowFlags::OWNER_ONLY,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("of", of),
    row!("returns", of),
    row!("arity", arity),
    row!("count", count),
];

/// The answer of the `Code` row for `method`, or `None` when `target` is not a
/// code object or `Code` declares no such row.
// Cost: O(1) to find the row, plus the handler's own cost.
pub(crate) fn answer(
    interp: &mut Interpreter,
    target: &Value,
    method: &str,
) -> Option<Result<Value, RuntimeError>> {
    if !matches!(method, "of" | "returns" | "arity" | "count") || !is_code(target) {
        return None;
    }
    crate::builtins::method_table::invoke_owner(interp, &["Code"], method, &[], || target.clone())
}

fn is_code(target: &Value) -> bool {
    matches!(
        target.view(),
        ValueView::Sub(_)
            | ValueView::WeakSub(_)
            | ValueView::Routine { .. }
            | ValueView::Regex(_)
            | ValueView::RegexWithAdverbs(..)
    )
}

/// `Code.of` and `Code.returns`: the return type the routine declares, the type
/// object of its spelling in the scope it was declared in, or `Mu`.
// Cost: O(1) for a code object with no declared return type; otherwise one
// lookup of the type's spelling in the closure's scope, O(e), e = bindings it
// searches.
fn of(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let mu = || Some(Ok(Value::package(Symbol::intern("Mu"))));
    let data = match target.view() {
        ValueView::Sub(data) => data.clone(),
        ValueView::WeakSub(weak) => weak.upgrade()?,
        // A name-based handle and a regex carry no return type of their own.
        _ => return mu(),
    };
    if let Some(ty) = data.routine_cell.return_type() {
        return Some(Ok(ty));
    }
    let type_name = interp
        .callable_return_type(target)
        .unwrap_or_else(|| "Mu".to_string());
    // `--> C[T]` for a `C` with its own `^parameterize` denotes the type object
    // that meta-method returns, not a name.
    if let Some(ty) = interp.meta_parameterized_type(&type_name) {
        return Some(Ok(ty));
    }
    // The return constraint is recorded by its source spelling; a lexical type
    // (`my subset ofTest ...; --> ofTest`) lives under a mangled storage name
    // (ADR-0047), so answer the type object the spelling is bound to in the
    // closure's scope, which is the one the bare `ofTest` term evaluates to.
    // Otherwise it is the type object the bare spelling evaluates to here.
    let type_name = match data.env.get(&type_name).map(Value::view) {
        Some(ValueView::Package(p)) if p.as_str().contains('\u{0}') => p.resolve().to_string(),
        _ => {
            if let Some(term) = interp.imported_type_term(&type_name) {
                return Some(Ok(term));
            }
            interp.lexical_env_remap_name(&type_name)
        }
    };
    Some(Ok(Value::package(Symbol::intern(&type_name))))
}

/// `Code.arity`: the number of required positional parameters (the smallest over
/// a multi dispatcher's candidates).
// Cost: O(p) for a routine with p parameters; O(c * p) for a multi dispatcher
// with c candidates.
fn arity(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    interp.code_arity_count(target, "arity")
}

/// `Code.count`: the number of positional parameters (the largest over a multi
/// dispatcher's candidates), `Inf` with a slurpy.
// Cost: O(p) for a routine with p parameters; O(c * p) for a multi dispatcher
// with c candidates.
fn count(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    interp.code_arity_count(target, "count")
}
