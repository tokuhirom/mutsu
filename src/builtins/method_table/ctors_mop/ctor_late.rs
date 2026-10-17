//! Late rows of the constructors and of `Failure.handled` (ADR-11276 §9.141).
//! They were the by-name match of `dispatch_new_and_constructors`.
//!
//! - `Range.new($min, $max)`;
//! - `IO::ArgFiles.new(@files)`;
//! - `Failure.handled`, read or set;
//! - `new-from-pairs` of the `Set`/`Mix` kinds (`Bag`'s rows are in
//!   `any_coerce_late`);
//! - `Mu`'s `new` and `bless`, which every other receiver reaches.
//!
//! Each is `OWNER_ONLY`. `call_method_with_values` reaches them by walking
//! the receiver's MRO for a late row, after the by-name cascades declined.
//! `Range.new` and `IO::ArgFiles.new` answer their own type object only; a
//! subclass is `Mu.new`'s.

use crate::builtins::method_table::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

const CTOR: RowFlags = RowFlags::OWNER_ONLY
    .or(RowFlags::ANY_ARGS)
    .or(RowFlags::SLURPY)
    .or(RowFlags::ANY_NAMED)
    .or(RowFlags::JUNCTION_ARGS)
    .or(RowFlags::TYPE_OBJECT_OK);

macro_rules! late {
    ($owner:literal, $name:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Interp($handler),
            flags: CTOR,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    late!("Range", "new", range_new),
    late!("IO::ArgFiles", "new", argfiles_new),
    MethodRow {
        owner: "Failure",
        name: "handled",
        arity: 0,
        handler: Handler::Interp(handled),
        flags: RowFlags::OWNER_ONLY
            .or(RowFlags::ANY_ARGS)
            .or(RowFlags::SLURPY),
        named: &[],
    },
    late!("Set", "new-from-pairs", new_from_pairs),
    late!("SetHash", "new-from-pairs", new_from_pairs),
    late!("Mix", "new-from-pairs", new_from_pairs),
    late!("MixHash", "new-from-pairs", new_from_pairs),
    late!("Mu", "new", new),
    late!("Mu", "bless", bless),
];

/// The call's arguments, named ones after the positional ones.
// Cost: O(a), a = arguments of the call.
fn joined(args: &[Value], named: Named<'_>) -> Vec<Value> {
    let mut all = Vec::with_capacity(args.len() + named.pairs().len());
    all.extend(args.iter().cloned());
    all.extend(named.pairs().iter().cloned());
    all
}

/// Whether `target` is the type object `name` itself.
// Cost: O(1).
fn is_type(target: &Value, name: &str) -> bool {
    matches!(target.view(), ValueView::Package(n) if n == name)
}

// Cost: O(a), a = arguments of the call.
fn range_new(_: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    is_type(t, "Range").then(|| Interpreter::range_new_late(&joined(args, named)))
}

// Cost: O(f), f = files named.
fn argfiles_new(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    is_type(t, "IO::ArgFiles").then(|| Ok(i.argfiles_new_late(&joined(args, named))))
}

// Cost: O(1).
fn handled(_: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    if !matches!(t.view(), ValueView::Instance { class_name, .. } if class_name == "Failure") {
        return None;
    }
    Interpreter::failure_handled_late(t, args).map(Ok)
}

// Cost: O(p), p = pairs given.
fn new_from_pairs(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    Some(i.dispatch_new_from_pairs(t, joined(args, named)))
}

// Cost: O(a) plus the class's construction (BUILD/TWEAK chain).
fn new(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    Some(i.dispatch_new(t.clone(), joined(args, named)))
}

// Cost: O(a) plus the class's construction (BUILD/TWEAK chain).
fn bless(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    Some(i.dispatch_bless(t, joined(args, named)))
}
