//! `Supply.list` (ADR-11276 §9.55) and the rest of `Supply`'s methods
//! (§9.130).
//!
//! Every method of a `Supply` instance is an `OWNER_ONLY` row found by its
//! owner (`owners_of`) before the by-name dispatch, whose handler is
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
    ($name:literal, $op:ident) => {
        row!($name, $op, RowFlags::OWNER_ONLY.or(RowFlags::SLURPY))
    };
    ($name:literal, $op:ident, class) => {
        row!(
            $name,
            $op,
            RowFlags::OWNER_ONLY
                .or(RowFlags::SLURPY)
                .or(RowFlags::TYPE_OBJECT_OK)
        )
    };
    ($name:literal, $op:ident, $flags:expr) => {
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
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Supply",
        name: "list",
        arity: 0,
        handler: Handler::Interp(list),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    row!("map", Map),
    row!("grep", Grep),
    row!("first", First),
    row!("reduce", Reduce),
    row!("produce", Produce),
    row!("elems", Elems),
    row!("min", Min),
    row!("max", Max),
    row!("minmax", Minmax),
    row!("grab", Grab),
    row!("skip", Skip),
    row!("live", Live),
    row!("collate", Collate),
    row!("delayed", Delayed),
    row!("tap", Tap),
    row!("act", Act),
    row!("on-close", OnClose),
    row!("serialize", Serialize),
    row!("sanitize", Sanitize),
    row!("merge", Merge, class),
    row!("zip", Zip, class),
    row!("zip-latest", ZipLatest, class),
    row!("rotor", Rotor),
    row!("squish", Squish),
    row!("sort", Sort),
    row!("head", Head),
    row!("flat", Flat),
    row!("batch", Batch),
    row!("throttle", Throttle),
    row!("comb", Comb),
    row!("words", Words),
    row!("wait", Wait),
    row!("stable", Stable),
    row!("rotate", Rotate),
    row!("share", Share),
    row!("migrate", Migrate),
    row!("encode", Encode),
    row!("decode", Decode),
    row!("tail", Tail),
    row!("split", Split),
    row!("reverse", Reverse),
    row!("repeated", Repeated),
    row!("unique", Unique),
    row!("do", Do),
    row!("Promise", Promise),
    row!("schedule-on", ScheduleOn),
    row!("lines", Lines),
    row!("classify", Classify),
    row!("categorize", Categorize),
    row!("Channel", Channel),
    row!("Supply", Supply),
];

/// `Supply`'s rows for a `Supply` instance or the `Supply` type object; `None`
/// for anything else.
// Cost: O(1).
pub(crate) fn owners_of(target: &Value) -> Option<&'static [&'static str]> {
    match target.view() {
        ValueView::Instance { class_name, .. } if class_name == "Supply" => Some(&["Supply"]),
        ValueView::Package(name) if name == "Supply" => Some(&["Supply"]),
        _ => None,
    }
}

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
