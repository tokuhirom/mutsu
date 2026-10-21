//! The rows of `HyperSeq` and `RaceSeq` (ADR-11276 §9.137): `hyper`, `race`,
//! `configuration`, `is-lazy`, `iterator`, `map` and `grep`.
//!
//! Their bodies were a by-name block in `call_method_with_values`. Both kinds
//! have an opaque dispatch shape (`DispatchShape::is_opaque`), so every call
//! entry reaches the rows through the table; every other method of a hyper or race sequence still reads it as its
//! `List`, as Rakudo's do through `self.List`.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

type Answer = Option<Result<Value, RuntimeError>>;

/// The owners of a hyper or race sequence's rows (its opaque shape's);
/// `None` for any other value.
// Cost: O(1), a tag probe.
pub(crate) fn owners_of(target: &Value) -> Option<&'static [&'static str]> {
    use crate::value::DispatchShape;
    match target.receiver_shape()? {
        shape @ (DispatchShape::HyperSeq | DispatchShape::RaceSeq) => shape.opaque_owners(),
        _ => None,
    }
}

const OWN: RowFlags = RowFlags::ANY_ARGS.or(RowFlags::OBJECT_ARGS);

macro_rules! rows {
    ($owner:literal) => {
        [
            MethodRow {
                owner: $owner,
                name: "hyper",
                arity: 0,
                handler: Handler::Interp(hyper),
                flags: OWN.or(RowFlags::ANY_NAMED),
                named: &[],
            },
            MethodRow {
                owner: $owner,
                name: "race",
                arity: 0,
                handler: Handler::Interp(race),
                flags: OWN.or(RowFlags::ANY_NAMED),
                named: &[],
            },
            MethodRow {
                owner: $owner,
                name: "configuration",
                arity: 0,
                handler: Handler::Interp(configuration),
                flags: OWN,
                named: &[],
            },
            MethodRow {
                owner: $owner,
                name: "is-lazy",
                arity: 0,
                handler: Handler::Interp(is_lazy),
                flags: OWN,
                named: &[],
            },
            MethodRow {
                owner: $owner,
                name: "iterator",
                arity: 0,
                handler: Handler::Interp(iterator),
                flags: OWN,
                named: &[],
            },
            MethodRow {
                owner: $owner,
                name: "map",
                arity: 0,
                handler: Handler::Interp(map),
                flags: OWN.or(RowFlags::SLURPY),
                named: crate::builtins::method_table::MAP_NAMED,
            },
            MethodRow {
                owner: $owner,
                name: "grep",
                arity: 0,
                handler: Handler::Interp(grep),
                flags: OWN.or(RowFlags::SLURPY).or(RowFlags::ANY_NAMED),
                named: &[],
            },
        ]
    };
}

static HYPER: [MethodRow; 7] = rows!("HyperSeq");
static RACE: [MethodRow; 7] = rows!("RaceSeq");
pub(super) static HYPER_ROWS: &[MethodRow] = &HYPER;
pub(super) static RACE_ROWS: &[MethodRow] = &RACE;

/// The methods a hyper or race sequence answers as itself rather than as its
/// `List`: `Mu`'s type and identity introspection. Rakudo's `HyperSeq` and
/// `RaceSeq` reach every other method through `self.List`, but these are
/// `Mu`'s and see the sequence (`(1..3).hyper.isa(HyperSeq)` is `True`,
/// `.isa(List)` is `False`).
const ANSWERS_AS_ITSELF: [&str; 8] = [
    "WHAT", "^name", "isa", "does", "defined", "DEFINITE", "WHICH", "WHERE",
];

/// Whether a call of `method` on a hyper or race sequence reads the sequence
/// itself (see [`ANSWERS_AS_ITSELF`]) rather than its `List`.
// Cost: O(n), n = 8 names.
pub(crate) fn answers_as_itself(method: &str) -> bool {
    ANSWERS_AS_ITSELF.contains(&method)
}

/// A lazy `.hyper` / `.race` (#13918) is a `LazyList` that reports
/// `HyperSeq`, so the owner walk reaches these rows with it: they hold a
/// settled body, so most decline and the lazy step answers; `configuration`
/// and `is-lazy` answer from the list's own tags.
// Cost: O(1).
fn lazy_hyper(t: &Value) -> bool {
    matches!(t.view(), crate::value::ValueView::LazyList(_))
}

/// The call's arguments as the routines take them.
// Cost: O(a), a = arguments of the call.
fn joined(args: &[Value], named: Named<'_>) -> Vec<Value> {
    let mut all = Vec::with_capacity(args.len() + named.pairs().len());
    all.extend(args.iter().cloned());
    all.extend(named.pairs().iter().cloned());
    all
}

// Cost: O(1).
fn hyper(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    if lazy_hyper(t) {
        return None;
    }
    Some(i.hyper_seq_rewrap(t, true))
}

// Cost: O(1).
fn race(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    if lazy_hyper(t) {
        return None;
    }
    Some(i.hyper_seq_rewrap(t, false))
}

// Cost: O(1).
fn configuration(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    if let crate::value::ValueView::LazyList(ll) = t.view() {
        let (batch, degree) = ll.hyper_config().unwrap_or((None, None));
        return Some(Ok(Interpreter::make_hyper_configuration(batch, degree)));
    }
    Some(i.hyper_seq_configuration(t))
}

// Cost: O(1).
fn is_lazy(_: &mut Interpreter, _: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(Ok(Value::FALSE))
}

// Cost: O(n), n = elements of the sequence.
fn iterator(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    if lazy_hyper(t) {
        return None;
    }
    Some(i.hyper_seq_iterator(t))
}

// Cost: O(n), n = elements of the sequence (one mapper call each).
fn map(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    if lazy_hyper(t) {
        return None;
    }
    Some(i.hyper_seq_map_grep(t, "map", joined(args, named)))
}

// Cost: O(n), n = elements of the sequence (one test call each).
fn grep(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    if lazy_hyper(t) {
        return None;
    }
    Some(i.hyper_seq_map_grep(t, "grep", joined(args, named)))
}
