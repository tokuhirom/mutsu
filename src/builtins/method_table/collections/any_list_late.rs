//! Late rows of `Any`, `Mu`, `List`, `Seq` and `Thread` for the calls the
//! native paths decline (ADR-11276 §9.136): `toggle`, `tree` with a depth
//! spec, `is-lazy`, `rotate`, `join`, a `Supply`'s `Array`, `elems` and the
//! quant hashes' `Setty`/`Baggy`/`Mixy`.
//!
//! Each is `OWNER_ONLY`, registered after any pure row of the same owner, name
//! and arity (a §9.135 twin when there is one), so the guard step's answers
//! are unchanged and only the by-name cascade's MRO walk
//! (`any_list_method_by_mro`) reaches it. The handlers are the routines the
//! deleted arms called.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

/// Every positional and adverb, handed to the routine, which validates them.
const LATE_ANY: RowFlags = RowFlags::OWNER_ONLY
    .or(RowFlags::ANY_ARGS)
    .or(RowFlags::SLURPY)
    .or(RowFlags::ANY_NAMED)
    .or(RowFlags::TYPE_OBJECT_OK);
/// [`LATE_ANY`] for a method that declares no named argument (`tree`): one
/// the call passes is swallowed by the implicit `*%_`.
const LATE_SLURPY: RowFlags = RowFlags::OWNER_ONLY
    .or(RowFlags::ANY_ARGS)
    .or(RowFlags::SLURPY)
    .or(RowFlags::TYPE_OBJECT_OK);
const LATE: RowFlags = RowFlags::OWNER_ONLY
    .or(RowFlags::ANY_ARGS)
    .or(RowFlags::TYPE_OBJECT_OK);

macro_rules! late {
    ($owner:literal, $name:literal, $arity:literal, $flags:expr, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: Handler::Interp($handler),
            flags: $flags,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    late!("Any", "toggle", 0, LATE_ANY, toggle),
    // `tree()` is `Any.tree`'s pure row; a depth spec is the interpreter's.
    late!("Any", "tree", 1, LATE_SLURPY, tree),
    late!("Mu", "is-lazy", 0, LATE, is_lazy),
    late!("List", "rotate", 0, LATE_ANY, rotate),
    late!("Seq", "rotate", 0, LATE_ANY, rotate),
    late!("Thread", "join", 0, LATE, thread_join),
    // `Any.Array` is `self.list.Array`: a `Supply` materializes through its
    // `list` (§9.55); every other receiver keeps the pure row's answer.
    late!("Any", "Array", 0, LATE, supply_array),
    late!("Any", "elems", 0, LATE, elems),
    late!("Set", "Setty", 0, LATE, setty),
    late!("SetHash", "Setty", 0, LATE, setty),
    late!("Bag", "Setty", 0, LATE, setty),
    late!("BagHash", "Setty", 0, LATE, setty),
    late!("Mix", "Setty", 0, LATE, setty),
    late!("MixHash", "Setty", 0, LATE, setty),
    late!("Set", "Baggy", 0, LATE, baggy),
    late!("SetHash", "Baggy", 0, LATE, baggy),
    late!("Bag", "Baggy", 0, LATE, baggy),
    late!("BagHash", "Baggy", 0, LATE, baggy),
    late!("Mix", "Baggy", 0, LATE, baggy),
    late!("MixHash", "Baggy", 0, LATE, baggy),
    late!("Set", "Mixy", 0, LATE, mixy),
    late!("SetHash", "Mixy", 0, LATE, mixy),
    late!("Bag", "Mixy", 0, LATE, mixy),
    late!("BagHash", "Mixy", 0, LATE, mixy),
    late!("Mix", "Mixy", 0, LATE, mixy),
    late!("MixHash", "Mixy", 0, LATE, mixy),
    // `join` is an only-method: its routine reports a surplus argument
    // (`Too many positionals passed`), so the row takes every arity.
    late!("Any", "join", 0, LATE.or(RowFlags::SLURPY), join),
];

/// Joins positional and named arguments the way the cascade's arms took them.
// Cost: O(a), a = arguments of the call.
fn joined(args: &[Value], named: Named<'_>) -> Vec<Value> {
    let mut all = Vec::with_capacity(args.len() + named.pairs().len());
    all.extend(args.iter().cloned());
    all.extend(named.pairs().iter().cloned());
    all
}

// Cost: O(n), n = elements of the receiver (one test call each).
fn toggle(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    Some(i.dispatch_toggle(t.clone(), &joined(args, named)))
}

// Cost: O(n), n = elements of the receiver at every level the spec reaches.
fn tree(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    Some(i.dispatch_tree(t.clone(), &joined(args, named)))
}

// Cost: O(1).
fn is_lazy(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(Ok(i.dispatch_is_lazy_method(t)))
}

// Cost: O(n), n = elements of the receiver.
fn rotate(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    Some(i.dispatch_rotate(t.clone(), &joined(args, named)))
}

/// `Thread.join` blocks until the thread completes and syncs its shared
/// captured variables back, rather than stringifying the thread.
// Cost: O(1) plus the wait for the thread.
fn thread_join(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = t.view()
    else {
        return None;
    };
    if class_name != "Thread" {
        return None;
    }
    let attrs = attributes.as_map().clone();
    Some(i.dispatch_thread_finish(&attrs))
}

/// `Any.join($separator = '')`: an only-method, so a surplus positional is
/// Rakudo's arity error rather than a multi-dispatch miss.
// Cost: O(n), n = characters of the joined result.
fn join(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    if args.len() > 1 {
        return Some(Err(RuntimeError::new(format!(
            "Too many positionals passed; expected 1 or 2 arguments but got {}",
            args.len() + 1
        ))));
    }
    i.dispatch_join_method(t.clone(), args.to_vec())
}

// Cost: O(e), e = values the supply emits until done.
fn supply_array(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    i.supply_materialize(t, true)
}

/// The receivers `Any.elems`'s pure row declines (an instance's elements,
/// a `Seq` it caches).
// Cost: O(n), n = elements of the receiver.
fn elems(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    i.dispatch_elems_method(t.clone(), Vec::new())
}

/// `QuantHash.Setty`/`Baggy`/`Mixy`: the family type object of the same
/// hashiness (`SetHash.Baggy` is `BagHash`).
// Cost: O(1).
fn setty(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    i.dispatch_setty_baggy_mixy(t, "Setty")
}

// Cost: O(1).
fn baggy(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    i.dispatch_setty_baggy_mixy(t, "Baggy")
}

// Cost: O(1).
fn mixy(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    i.dispatch_setty_baggy_mixy(t, "Mixy")
}
