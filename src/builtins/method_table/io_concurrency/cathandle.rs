//! `IO::CatHandle`'s rows (ADR-11276 §9.132): the 42 methods mutsu implements
//! of those Rakudo declares on `IO::CatHandle`.
//!
//! A cat handle is an instance whose read state (the current handle, the
//! position, the queue of sources) lives in its shared attribute cell; every
//! method may advance it, so the one implementation runs on a working copy and
//! commits through the cell (`call_native_instance_method_mut_in_place`), as
//! both dispatch entries did. No row needs the receiver's place (§9.126). The
//! lazy `.lines` and `.handles` hand out a list backed by the live cat, so
//! later `.chomp`/`.nl-in`/`.encoding` changes apply to it. The rows are
//! `OWNER_ONLY`, found by owner from the instance dispatch and the mutating
//! method call; the methods read their positional and named arguments as one
//! list, so the rows are slurpy.

use super::super::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

macro_rules! rows {
    ($($name:literal),* $(,)?) => {
        &[$(MethodRow {
            owner: "IO::CatHandle",
            name: $name,
            arity: 0,
            handler: Handler::Interp({
                fn handler(
                    i: &mut Interpreter,
                    t: &Value,
                    args: &[Value],
                    named: Named<'_>,
                ) -> Answer {
                    call(i, t, $name, args, named)
                }
                handler
            }),
            flags: RowFlags::OWNER_ONLY.or(RowFlags::SLURPY),
            named: named_of($name),
        }),*]
    };
}

/// The named arguments Rakudo's `IO::CatHandle` method `name` declares.
// Cost: O(1).
const fn named_of(name: &str) -> &'static [&'static str] {
    use crate::value::const_str_eq;
    if const_str_eq(name, "lines") {
        &["chomp", "close"]
    } else if const_str_eq(name, "words") {
        &["close"]
    } else if const_str_eq(name, "slurp") {
        &["bin"]
    } else if const_str_eq(name, "Supply") {
        &["size"]
    } else {
        &[]
    }
}

pub(super) static ROWS: &[MethodRow] = rows![
    "get",
    "getc",
    "lines",
    "words",
    "comb",
    "split",
    "slurp",
    "readchars",
    "read",
    "next-handle",
    "open",
    "close",
    "DESTROY",
    "eof",
    "opened",
    "chomp",
    "nl-in",
    "encoding",
    "on-switch",
    "path",
    "IO",
    "handles",
    "Str",
    "gist",
    "Supply",
    "native-descriptor",
    "seek",
    "tell",
    "t",
    "lock",
    "unlock",
    "raku",
    "flush",
    "out-buffer",
    "print",
    "printf",
    "print-nl",
    "put",
    "say",
    "write",
    "WRITE",
    "READ",
];

/// `IO::CatHandle`'s rows for a cat handle; `None` for anything else, an
/// instance of a user subclass included (it keeps the inherited native path).
// Cost: O(1).
pub(crate) fn owners_of(target: &Value) -> Option<&'static [&'static str]> {
    match target.view() {
        ValueView::Instance { class_name, .. } if class_name == "IO::CatHandle" => {
            Some(&["IO::CatHandle"])
        }
        _ => None,
    }
}

// Cost: the method's own (a read advances the cat by what it reads).
fn call(
    interp: &mut Interpreter,
    target: &Value,
    method: &'static str,
    args: &[Value],
    named: Named<'_>,
) -> Answer {
    let mut all = args.to_vec();
    all.extend(named.pairs().iter().cloned());
    interp.cathandle_call(target, method, all)
}
