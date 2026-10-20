//! `Supply.list` (ADR-11276 §9.55) and the rest of `Supply`'s methods
//! (§9.130).
//!
//! A `Supply` has an opaque dispatch shape (`DispatchShape::is_opaque`), so
//! every call entry reaches its rows through the table; each handler is
//! `Interpreter::supply_call`: the routine that answered the name before, one
//! per method, so a variable and a temporary receiver reach the same answer.
//! The methods read their positional and named arguments as one list, so the
//! rows are slurpy and the handler hands both back. `merge`, `zip` and
//! `zip-latest` are class methods too (`TYPE_OBJECT_OK`).
//!
//! A materialized `Supply` instance keeps its emitted values in the `values`
//! attribute; `.list` and `.Array` read them. A supply that has no values yet
//! (an on-demand `supply { ... }` block, a stream backed by a supplier or a
//! channel) declines, so the stateful slow path runs its body or drains it.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::runtime::methods_supply_rows::SupplyOp;
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

macro_rules! row {
    ($name:literal, $op:ident $(, named: $named:expr)?) => {
        row!(@row $name, $op, RowFlags::SLURPY.or(ARGS) $(, $named)?)
    };
    // `Supply:D:` (`Supply.tap` is `X::Parameter::InvalidConcreteness`).
    ($name:literal, $op:ident, definite $(, named: $named:expr)?) => {
        row!(
            @row $name,
            $op,
            RowFlags::SLURPY
                .or(ARGS)
                .or(RowFlags::DEFINITE)
            $(, $named)?
        )
    };
    // A `multi` whose every candidate is `Supply:D:` (`X::Multi::NoMatch`).
    ($name:literal, $op:ident, definite_multi $(, named: $named:expr)?) => {
        row!(
            @row $name,
            $op,
            RowFlags::SLURPY
                .or(ARGS)
                .or(RowFlags::DEFINITE_MULTI)
            $(, $named)?
        )
    };
    ($name:literal, $op:ident, class $(, named: $named:expr)?) => {
        row!(
            @row $name,
            $op,
            RowFlags::SLURPY
                .or(ARGS)
                .or(RowFlags::TYPE_OBJECT_OK)
            $(, $named)?
        )
    };
    // Rakudo's `|c` or `*%others`: every named argument is the method's.
    ($name:literal, $op:ident, any_named) => {
        row!(
            @row $name,
            $op,
            RowFlags::SLURPY
                .or(ARGS)
                .or(RowFlags::ANY_NAMED)
        )
    };
    (@row $name:literal, $op:ident, $flags:expr) => {
        row!(@row $name, $op, $flags, &[])
    };
    (@row $name:literal, $op:ident, $flags:expr, $named:expr) => {
        MethodRow {
            owner: "Supply",
            name: $name,
            arity: 0,
            handler: Handler::Interp({
                fn handler(
                    i: &mut Interpreter,
                    t: &Value,
                    args: &[Value],
                    named: Named<'_>,
                ) -> Answer {
                    let mut all = args.to_vec();
                    all.extend(named.pairs().iter().cloned());
                    i.supply_call(t, SupplyOp::$op, all)
                }
                handler
            }),
            flags: $flags,
            named: $named,
        }
    };
}

/// A handler reads its arguments itself, as `supply_call` did before the
/// shape existed.
const ARGS: RowFlags = RowFlags::ANY_ARGS.or(RowFlags::OBJECT_ARGS);

/// The named arguments of `unique`, and of `repeated` and `squish` without
/// `:expires`.
const AS_WITH: &[&str] = &["as", "with"];

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Supply",
        name: "list",
        arity: 0,
        handler: Handler::Interp(list),
        flags: RowFlags::NONE,
        named: &[],
    },
    row!("map", Map, definite),
    row!("grep", Grep, definite),
    row!("first", First, any_named),
    row!("reduce", Reduce, definite),
    row!("produce", Produce, definite),
    row!("elems", Elems),
    row!("min", Min, definite),
    row!("max", Max, definite),
    row!("minmax", Minmax, definite),
    row!("grab", Grab, class),
    row!("skip", Skip, definite),
    row!("live", Live),
    row!("collate", Collate),
    row!("delayed", Delayed, definite, named: &["scheduler"]),
    row!("tap", Tap, definite, named: &["done", "quit", "tap"]),
    row!("act", Act, any_named),
    row!("on-close", OnClose),
    row!("serialize", Serialize),
    row!("sanitize", Sanitize),
    row!("merge", Merge, class),
    row!("zip", Zip, class, named: &["with"]),
    row!("zip-latest", ZipLatest, class, named: &["initial", "with"]),
    row!("rotor", Rotor, definite_multi, named: &["partial"]),
    row!("squish", Squish, named: AS_WITH),
    row!("sort", Sort),
    row!("head", Head),
    row!("flat", Flat),
    row!("batch", Batch, definite, named: &["elems", "emit-timed", "seconds"]),
    row!(
        "throttle",
        Throttle,
        definite_multi,
        named: &["bleed", "control", "scheduler", "status", "vent-at"]
    ),
    row!("comb", Comb, definite_multi, named: &["match"]),
    row!("words", Words, definite),
    row!("wait", Wait, definite),
    row!("stable", Stable, named: &["scheduler"]),
    row!("rotate", Rotate, definite),
    row!("share", Share),
    row!("migrate", Migrate),
    row!("encode", Encode),
    row!("decode", Decode),
    row!("tail", Tail),
    row!("split", Split),
    row!("reverse", Reverse),
    row!("repeated", Repeated, named: AS_WITH),
    row!("unique", Unique, named: &["as", "expires", "with"]),
    row!("do", Do),
    row!("Promise", Promise),
    row!("schedule-on", ScheduleOn),
    row!("lines", Lines, definite_multi, named: &["chomp"]),
    row!("classify", Classify, definite_multi),
    row!("categorize", Categorize),
    row!("Channel", Channel),
    row!("Supply", Supply),
];

/// `Supply.list`: the values materialized, drained from a live channel, or
/// collected by running an on-demand body (`Interpreter::supply_materialize`).
// Cost: O(e), e = values the supply emits until done.
fn list(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    interp.supply_materialize(target, false)
}

/// The values of a `Supply` as a List (`want_array` false) or a real Array, or
/// `None` when only the stateful path can produce them.
// Cost: O(e), e = values emitted so far.
pub(crate) fn listify(target: &Value, want_array: bool) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    // `.Array` builds a REAL Array, whose elements are `Scalar` containers, so
    // aggregates itemize on the way in; `.list` builds a List, which must not.
    let wrap = |items: Vec<Value>| {
        if want_array {
            crate::runtime::utils::itemize_real_array_elements(Value::real_array(items))
        } else {
            Value::array(items)
        }
    };
    // An on-demand supply has no materialized values; its body must be run by
    // the stateful slow path (`supply_list_values`).
    if attributes.as_map().contains_key("on_demand_callback") {
        return None;
    }
    // Likewise `.list` on a live, channel-backed supply (an `IO::Socket::Async`
    // read stream): its values arrive on a channel that only the stateful path
    // can drain, and answering the empty `values` attribute here would report
    // an open stream as an empty one.
    if !want_array {
        let attrs = attributes.as_map();
        if attrs.get("live").is_some_and(Value::truthy)
            && let Some(supplier_id) = attrs.get("supplier_id").and_then(Value::as_int)
            && supplier_id > 0
        {
            return Some(
                crate::runtime::native_methods::collect_supplier_values(
                    supplier_id as u64,
                    Vec::new(),
                    false,
                    true,
                )
                .map(wrap),
            );
        }
        if attrs.contains_key("supply_id") && !attrs.contains_key("proc_output") {
            return None;
        }
    }
    let items = match attributes.as_map().get("values").map(Value::view) {
        Some(ValueView::Array(items, ..)) => items.to_vec(),
        _ => Vec::new(),
    };
    Some(Ok(wrap(items)))
}
