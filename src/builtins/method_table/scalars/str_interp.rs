//! `Str`'s and `Cool`'s text rows that need the interpreter (ADR-11276 §9.75):
//! `match` on `Str`; `subst`, `trans`, `indices` and `IO` on `Str` and `Cool`;
//! `sprintf` and `printf` on `Cool`; `split` on `Str` and `Cool` (§9.76).
//!
//! A regex argument may close over its defining scope, a replacement may be a
//! closure, `subst`/`match` publish the caller's `$/`, `IO` reads `$*CWD` and
//! `printf` writes to `$*OUT`, so every row is a `Handler::Interp` row. Each
//! handler calls the one `Interpreter` routine the by-name arm of
//! `dispatch_method_by_name_1` also calls. The arguments are passed as the arm
//! took them: the positional ones, then the named ones as pairs.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

/// Any positional argument (a `Regex`, a `Pair`, a closure), and every named
/// one: the adverbs belong to the pattern or the format.
const ARGS_NAMED: RowFlags = RowFlags::ANY_ARGS.or(RowFlags::ANY_NAMED);

macro_rules! row {
    ($owner:literal, $name:literal, $arity:literal, $flags:expr, $handler:ident) => {
        row!($owner, $name, $arity, $flags, $handler, &[])
    };
    ($owner:literal, $name:literal, $arity:literal, $flags:expr, $handler:ident, $named:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: Handler::Interp($handler),
            flags: $flags,
            named: $named,
        }
    };
}

/// The rows `Str` and `Cool` both declare. `$split` flags the `split` rows:
/// every candidate of `Str.split` is `Str:D` (`Str.split(",")` finds none),
/// while `Cool.split` stringifies a type object.
macro_rules! shared_rows {
    ($owner:literal, $split:expr) => {
        &[
            row!($owner, "subst", 1, ARGS_NAMED, subst),
            row!($owner, "subst", 2, ARGS_NAMED, subst),
            row!($owner, "trans", 0, ARGS_NAMED.or(RowFlags::SLURPY), trans),
            row!($owner, "indices", 1, ARGS_NAMED, indices),
            row!($owner, "indices", 2, ARGS_NAMED, indices),
            row!(
                $owner,
                "IO",
                0,
                RowFlags::TYPE_OBJECT_OK,
                io,
                &["CWD", "SPEC"]
            ),
            row!($owner, "split", 1, ARGS_NAMED.or($split), split),
            row!($owner, "split", 2, ARGS_NAMED.or($split), split),
        ]
    };
}

pub(super) static STR_ROWS: &[MethodRow] = shared_rows!("Str", RowFlags::DEFINITE_MULTI);
pub(super) static STR_MATCH_ROWS: &[MethodRow] = &[row!("Str", "match", 1, ARGS_NAMED, match_)];
pub(super) static COOL_ROWS: &[MethodRow] = shared_rows!("Cool", RowFlags::NONE);
/// A `Cool` type object formats as its (empty, warning) string, as Rakudo's
/// `Cool.sprintf` does (`Int.sprintf("%s")` is a directive-count error, not a
/// missing method).
pub(super) static COOL_FORMAT_ROWS: &[MethodRow] = &[
    row!(
        "Cool",
        "sprintf",
        0,
        RowFlags::ANY_ARGS
            .or(RowFlags::SLURPY)
            .or(RowFlags::TYPE_OBJECT_OK),
        sprintf
    ),
    row!(
        "Cool",
        "printf",
        0,
        RowFlags::ANY_ARGS
            .or(RowFlags::SLURPY)
            .or(RowFlags::TYPE_OBJECT_OK),
        printf
    ),
];

/// The call's arguments as the by-name arms take them: the positional ones,
/// then the named ones as string-keyed pairs.
// Cost: O(a), a = arguments of the call.
fn joined(args: &[Value], named: Named<'_>) -> Vec<Value> {
    let mut all = Vec::with_capacity(args.len() + named.pairs().len());
    all.extend(args.iter().cloned());
    all.extend(named.pairs().iter().cloned());
    all
}

/// `Str.match`.
// Cost: O(n) in the haystack for a plain pattern.
fn match_(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    Some(interp.dispatch_match_row(target.clone(), joined(args, named)))
}

/// `.subst`: the VM's native substitution for a plain pattern and a literal
/// replacement first (the same fast path a call that misses the table takes),
/// then the interpreter's.
// Cost: O(n) in the haystack for a plain pattern, plus one replacement call per
// match for a closure replacement.
fn subst(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let args = joined(args, named);
    if let Some(answer) = interp.try_native_subst(target, "subst", &args) {
        return Some(answer);
    }
    Some(interp.dispatch_subst_row(target.clone(), args))
}

/// `.split`.
// Cost: O(n) in the haystack, plus the splitter's own cost.
fn split(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    Some(interp.dispatch_split_row(target.clone(), joined(args, named)))
}

/// `.trans`.
// Cost: see `Interpreter::dispatch_trans` (src/runtime/methods_trans.rs).
fn trans(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    // Only `:s` / `:d` / `:c` (and their long names) are adverbs of `trans`;
    // any other named argument (`:g`) is ignored rather than read as a
    // mapping pair.
    let mut all = args.to_vec();
    all.extend(
        named
            .pairs()
            .iter()
            .filter(|arg| {
                matches!(
                    arg.view(),
                    ValueView::Pair(key, _)
                        if matches!(
                            key.as_str(),
                            "s" | "squash" | "d" | "delete" | "c" | "complement"
                        )
                )
            })
            .cloned(),
    );
    Some(interp.dispatch_trans(target.clone(), &all))
}

/// `.indices`.
// Cost: O(n * m), n = chars of the invocant, m = chars of the needle.
fn indices(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    Some(interp.dispatch_indices(target.clone(), &joined(args, named)))
}

/// `.IO`.
// Cost: O(n), n = chars of the invocant's string form.
fn io(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    interp.dispatch_io_coerce(target, &joined(args, named))
}

/// `Cool.sprintf`.
// Cost: O(n), n = chars of the formatted text.
fn sprintf(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    interp.dispatch_sprintf_row(target, args)
}

/// `Cool.printf`.
// Cost: O(n), n = chars of the formatted text (rendered, then written).
fn printf(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    interp.dispatch_printf_row(target, args)
}
