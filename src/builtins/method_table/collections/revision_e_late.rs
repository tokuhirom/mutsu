//! The methods language revision 6.e adds to `Any`, `Supply`, `Cool`, `Str`
//! and `IO::Path` (ADR-11276 §9.146): `Any.snip`, `Any.snitch`,
//! `Supply.snip`, `Cool.nomark`, `Str.nomark` and `IO::Path.stem`.
//!
//! Each row is [`RowFlags::REVISION_E`]: under an older revision every owner
//! lookup skips it, so the call finds no method and `.^can` denies it, as in
//! Rakudo, whose `core.e` setting declares them. Each is also `OWNER_ONLY`
//! and `Handler::Interp`, reached by the late MRO walk of
//! `dispatch_method_by_name_1` after every earlier answer declined.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

const E: RowFlags = RowFlags::OWNER_ONLY.or(RowFlags::REVISION_E);
/// A `*@conditions` method: every positional is a matcher.
const E_SLURPY: RowFlags = E.or(RowFlags::ANY_ARGS).or(RowFlags::SLURPY);

macro_rules! late_e {
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
    late_e!("Any", "snip", 1, E_SLURPY, snip),
    late_e!("Any", "snitch", 0, E.or(RowFlags::TYPE_OBJECT_OK), snitch),
    late_e!("Any", "snitch", 1, E.or(RowFlags::TYPE_OBJECT_OK), snitch),
    late_e!("Supply", "snip", 1, E_SLURPY, supply_snip),
    late_e!("Cool", "nomark", 0, E, nomark),
    late_e!("Str", "nomark", 0, E, nomark),
    late_e!("IO::Path", "stem", 0, E, stem),
    late_e!("IO::Path", "stem", 1, E.or(RowFlags::ANY_ARGS), stem),
    late_e!(
        "Mu",
        "Callable",
        1,
        E.or(RowFlags::TYPE_OBJECT_OK),
        mu_callable
    ),
    late_e!("Grammar", "locprepost", 0, E, grammar_locprepost),
    late_e!(
        "Grammar",
        "typed_exception",
        1,
        E.or(RowFlags::ANY_ARGS),
        grammar_typed_exception
    ),
    late_e!(
        "Grammar",
        "SETFAIL",
        1,
        E.or(RowFlags::ANY_ARGS).or(RowFlags::TYPE_OBJECT_OK),
        grammar_setfail
    ),
];

/// `Mu.Callable($method)`: the method object `.^find_method($method)` answers.
// Cost: O(m), m = the members of the type the lookup reads.
fn mu_callable(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    Some(i.mu_callable(t, args))
}

/// `Grammar.locprepost`: the text around the cursor, `[$pre, $post]`.
// Cost: O(n), n = characters of the parsed text.
fn grammar_locprepost(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(i.grammar_locprepost(t))
}

/// `Grammar.typed_exception(X::Comp $ex, *%opts)`.
// Cost: O(n), n = characters of the parsed text.
fn grammar_typed_exception(
    i: &mut Interpreter,
    t: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Answer {
    let all: Vec<Value> = args.iter().chain(named.pairs()).cloned().collect();
    Some(i.grammar_typed_exception(t, &all, None))
}

/// `Grammar.SETFAIL($failed, :$filename)`.
// Cost: O(n), n = characters of the parsed text.
fn grammar_setfail(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    let all: Vec<Value> = args.iter().chain(named.pairs()).cloned().collect();
    Some(i.grammar_setfail(t, &all))
}

/// `Any.snip(*@conditions)`: the invocant's elements cut into lists, each
/// ending before the first element that fails its condition; the conditions
/// are used in turn. One argument is one condition or a list of them.
// Cost: O(e) condition calls, e = elements of the invocant.
fn snip(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    let matcher = match args {
        [one] => one.clone(),
        _ => Value::array(args.to_vec()),
    };
    let items = crate::runtime::utils::value_to_list(t);
    Some(i.eval_snip(matcher, items))
}

/// `Any.snitch(&snitcher = &note)`: the invocant, after handing it to the
/// snitcher (`note` by default).
// Cost: O(1) plus the snitcher's own cost.
fn snitch(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    i.dispatch_snitch(t, args)
}

/// `Supply.snip(*@tests)`.
// Cost: O(v) test calls, v = values of the supply.
fn supply_snip(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    if !matches!(t.view(), ValueView::Instance { class_name, .. } if class_name == "Supply") {
        return None;
    }
    Some(i.dispatch_supply_transform(t.clone(), "snip", args))
}

/// `Str.nomark` (and `Cool.nomark`, `self.Str.nomark`): the string with every
/// grapheme's combining marks dropped, the fold of the `:m` adverb.
// Cost: O(n), n = bytes of the string.
fn nomark(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    let text = t.to_string_value();
    Some(Ok(Value::str(crate::builtins::str_prim::nomark(&text))))
}

/// `IO::Path.stem($parts = *)`: the basename without its last `$parts`
/// extensions, all of them by default; at least the part before the first
/// dot is kept. `$parts` is taken as a number, so a `Range` counts its
/// elements, as in Rakudo.
// Cost: O(p), p = chars of the path.
fn stem(_: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    if !matches!(t.view(), ValueView::Instance { .. }) {
        return None;
    }
    let basename = crate::builtins::method_table::io_path_basename(t)?;
    let parts: Vec<&str> = basename.split('.').collect();
    let strip = match args.first() {
        None => parts.len(),
        Some(arg) if matches!(arg.view(), ValueView::Whatever) => parts.len(),
        Some(arg) if arg.is_range() => crate::runtime::utils::value_to_list(arg).len(),
        Some(arg) => {
            let n = arg.to_f64();
            if n.is_nan() || n >= parts.len() as f64 {
                parts.len()
            } else {
                n.max(0.0) as usize
            }
        }
    };
    let keep = parts.len().saturating_sub(strip).max(1);
    Some(Ok(Value::str(parts[..keep].join("."))))
}
