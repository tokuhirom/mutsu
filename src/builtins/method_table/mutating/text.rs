//! `Str.subst-mutate` and `Str.substr-rw` (ADR-11276 §9.23): the two `Str`
//! methods that write the variable they were called on.
//!
//! Both need a binding. A string is immutable, so `subst-mutate` replaces the
//! value held under the name (`ReceiverPlace::assign`, which writes both halves
//! of the VM's dual store), and `substr-rw` hands back a write-through `Proxy`
//! that splices into the variable. A receiver with no name, a package-qualified
//! name or a receiver that is not a plain `Str` declines and takes the
//! cascades, as the arms they replace did.

use crate::builtins::method_table::{Handler, MethodRow, Named, ReceiverPlace, RowFlags};
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

/// `subst-mutate` takes the pattern, the replacement and the `.subst` adverbs
/// (`:g`, `:i`, `:x`, ...), which are the pattern's own named arguments, so the
/// row binds every named argument; the row is slurpy from zero so a call with
/// too few positionals reaches `.subst`'s own error. `substr-rw` takes the
/// window's offset and length.
pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Str",
        name: "subst-mutate",
        arity: 0,
        handler: Handler::Mut(subst_mutate_row),
        flags: RowFlags::SLURPY.or(RowFlags::ANY_NAMED),
        named: &[],
    },
    MethodRow {
        owner: "Str",
        name: "substr-rw",
        arity: 0,
        handler: Handler::Mut(substr_rw_row),
        flags: RowFlags::SLURPY,
        named: &[],
    },
];

/// `$s.subst-mutate(pattern, replacement, ...)` substitutes in place (like
/// `s///`) and returns the value `s///` would set in `$/`: a Match for a
/// single hit, `Nil` when nothing matched, or a List of
/// Matches under `:g`. Reuses the `.subst` machinery for the new string and the
/// `.match` machinery for the return, then writes the new string back to the
/// variable.
// Cost: O(n + r), n = chars of the string, r = chars of the replacement, plus
// the cost of the match (the string is rebuilt, as Rakudo's strings are
// immutable too).
fn subst_mutate_row(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    if !matches!(place.value().view(), ValueView::Str(_)) {
        return None;
    }
    place.name()?;
    let target = place.value().clone();
    // `.subst` and `.match` read the adverbs among their arguments.
    let mut all = args.to_vec();
    all.extend_from_slice(named.pairs());
    Some(subst_mutate(interp, place, target, &all))
}

/// The body of [`subst_mutate_row`].
// Cost: see `subst_mutate_row`.
fn subst_mutate(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    target: Value,
    args: &[Value],
) -> Result<Value, RuntimeError> {
    let new_str = interp.dispatch_subst(target.clone(), args)?;
    // `.match` takes the pattern + adverbs but not the replacement (the 2nd
    // positional), so drop the replacement when building its args.
    let mut match_args: Vec<Value> = Vec::new();
    let mut positional_seen = 0;
    for arg in args {
        if arg.is_string_pair_value() {
            match_args.push(arg.clone());
        } else {
            positional_seen += 1;
            if positional_seen != 2 {
                match_args.push(arg.clone());
            }
        }
    }
    let literal_string_pattern = args
        .iter()
        .find(|arg| !matches!(arg.view(), ValueView::Pair(..)))
        .is_some_and(|arg| matches!(arg.deref_container().view(), ValueView::Str(_)));
    let ret = if literal_string_pattern {
        // `dispatch_subst` already selected the grapheme-safe literal matches
        // and published them in `$/`. Re-running them through the regex engine
        // could accept a codepoint inside a grapheme. A failed literal `s///`
        // leaves `$/` as `Any`; the method answers `Nil`.
        match interp.env().get("/") {
            Some(m) if !m.is_nil() && !matches!(m.view(), ValueView::Package(_)) => m.clone(),
            _ => Value::NIL,
        }
    } else {
        // Rakudo answers `Nil` for a miss (`:g`/`:x` answer an empty list),
        // which is exactly what `.match` returns.
        interp.dispatch_match_method(target, &match_args)?
    };
    place.assign(interp, new_str);
    Ok(ret)
}

/// `$s.substr-rw(...)` outside an assignment: hand back the same write-through
/// `Proxy` the sub form `substr-rw($s, ...)` returns, so a bound
/// `my $r := $s.substr-rw(1, 1); $r = "Y"` splices into `$s` (#9200). Only a
/// `Str` held by a plain (not package-qualified) variable takes it.
// Cost: O(n), n = chars of the receiver (the Proxy's range is resolved against
// it once).
fn substr_rw_row(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    if !matches!(place.value().descalarize().view(), ValueView::Str(_)) {
        return None;
    }
    let name = place.name()?.to_string();
    if crate::qualified::is_qualified(Symbol::intern(&name)) {
        return None;
    }
    Some(interp.make_substr_rw_proxy(&name, args))
}
