//! `Routine`'s `wrap`, `unwrap`, `add_dispatchee`, `candidates`,
//! `dispatchees`, `rw` and `yada`, and `Code`'s `assuming`, `cando`, `prec`,
//! `precedence`, `associative` and `is-implementation-detail` (ADR-11276
//! §9.166).
//!
//! A code object is a `&name` handle on a routine the registry holds or a
//! `Sub` carrying its own data, and the two answer each of these methods from
//! different state, so a row has one body per kind
//! (`Interpreter::routine_handle_*` and `sub_*`, `runtime/methods_code_*rows.rs`)
//! and its handler picks by kind ([`by_kind`]). The rows are reached through
//! the owner chain of [`super::code_class::owners_of`] from the callable
//! dispatch, ahead of the kind's own dispatcher. A method object (an instance
//! of a code class) is the third kind (`Interpreter::method_object_code_row`,
//! §9.167), and a regex the fourth ([`super::code_regex`]).

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, SubData, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;
type HandleFn = fn(&mut Interpreter, &Value, &str, &str, &str, Vec<Value>, bool) -> Answer;
type SubFn = fn(&mut Interpreter, &Value, &SubData, &str, Vec<Value>) -> Answer;

const NULLARY: RowFlags = RowFlags::OWNER_ONLY;
const ARGS: RowFlags = RowFlags::OWNER_ONLY
    .or(RowFlags::ANY_ARGS)
    .or(RowFlags::OBJECT_ARGS)
    .or(RowFlags::JUNCTION_ARGS);

macro_rules! row {
    ($owner:literal, $name:literal, $arity:literal, $flags:expr, $handle:ident, $sub:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: Handler::Interp({
                fn handler(
                    interp: &mut Interpreter,
                    target: &Value,
                    args: &[Value],
                    named: Named<'_>,
                ) -> Answer {
                    let mut all = args.to_vec();
                    all.extend_from_slice(named.pairs());
                    by_kind(
                        interp,
                        target,
                        $name,
                        all,
                        Interpreter::$handle,
                        Interpreter::$sub,
                    )
                }
                handler
            }),
            flags: $flags,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!(
        "Routine",
        "wrap",
        0,
        ARGS.or(RowFlags::SLURPY),
        routine_handle_wrap,
        sub_wrap
    ),
    row!(
        "Routine",
        "unwrap",
        0,
        ARGS.or(RowFlags::SLURPY),
        routine_handle_wrap,
        sub_wrap
    ),
    row!(
        "Routine",
        "add_dispatchee",
        1,
        ARGS,
        routine_handle_dispatchees,
        sub_dispatchees
    ),
    row!(
        "Routine",
        "candidates",
        0,
        NULLARY,
        routine_handle_dispatchees,
        sub_dispatchees
    ),
    row!(
        "Routine",
        "dispatchees",
        0,
        NULLARY,
        routine_handle_dispatchees,
        sub_dispatchees
    ),
    row!("Routine", "rw", 0, NULLARY, routine_handle_rw, sub_rw),
    row!(
        "Code",
        "assuming",
        0,
        ARGS.or(RowFlags::SLURPY).or(RowFlags::ANY_NAMED),
        routine_handle_assuming,
        sub_assuming
    ),
    row!("Code", "cando", 1, ARGS, routine_handle_cando, sub_cando),
    row!("Code", "prec", 0, NULLARY, routine_handle_prec, sub_prec),
    row!(
        "Code",
        "precedence",
        0,
        NULLARY,
        routine_handle_prec,
        sub_prec
    ),
    row!(
        "Code",
        "associative",
        0,
        NULLARY,
        routine_handle_prec,
        sub_prec
    ),
    row!(
        "Code",
        "is-implementation-detail",
        0,
        NULLARY,
        routine_handle_prec,
        sub_prec
    ),
    MethodRow {
        owner: "Routine",
        name: "yada",
        arity: 0,
        handler: Handler::Interp(yada),
        flags: NULLARY,
        named: &[],
    },
];

/// `Routine.yada`.
// Cost: as `Interpreter::code_yada`.
fn yada(interp: &mut Interpreter, target: &Value, _args: &[Value], _named: Named<'_>) -> Answer {
    if super::code_regex::is_regex(target) {
        return super::code_regex::answer(target, "yada");
    }
    interp.code_yada(target).map(Ok)
}

/// The body for `target`'s kind: `handle` for a `&name` handle, `sub` for a
/// `Sub`; `None` for any other receiver.
// Cost: O(1), plus the body's own cost.
fn by_kind(
    interp: &mut Interpreter,
    target: &Value,
    method: &str,
    args: Vec<Value>,
    handle: HandleFn,
    sub: SubFn,
) -> Answer {
    match target.view() {
        ValueView::Routine {
            name,
            package,
            is_regex,
            ..
        } => {
            let (name, package) = (name.resolve(), package.resolve());
            handle(interp, target, &name, &package, method, args, is_regex)
        }
        ValueView::Sub(data) => {
            let data = data.clone();
            sub(interp, target, &data, method, args)
        }
        ValueView::Instance { .. } => interp.method_object_code_row(target, method, args),
        _ if super::code_regex::is_regex(target) && method == "cando" => {
            Some(Ok(super::code_regex::cando(target, &args)))
        }
        _ if super::code_regex::is_regex(target) => super::code_regex::answer(target, method),
        _ => None,
    }
}
