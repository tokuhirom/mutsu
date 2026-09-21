//! The `(receiver kind, method symbol)` table for zero-argument native
//! dispatch (issue #8888).
//!
//! # What this replaces
//!
//! `@a.elems` on a plain array was 8,314 instructions, of which the operation
//! itself is about 200. The rest is the walk to it: a ~32-probe gauntlet in
//! [`try_native_method_raw`](crate::runtime::Interpreter), then
//! [`native_method_0arg`](super::methods_0arg::native_method_0arg)'s prologue,
//! then `dispatch_core`'s, and only then
//! the family cascade that answers the call. Every stage re-decodes the same
//! NaN-box tag (57 decodes per call) and re-compares the same method name
//! against its own list of special cases.
//!
//! None of those probes can claim `@a.elems`. Each one is gated either on a
//! method NAME this table does not contain, or on a receiver KIND this table
//! does not cover. That is a fact about the code, decided once — so this module
//! decides it once, and the call jumps straight to
//! [`dispatch_core_families`](super::methods_0arg::dispatch_core_families).
//!
//! # What this is not
//!
//! It is **not** a reimplementation of any method. The table authorizes a
//! *skip*; the answer still comes from the same family cascade, so a method's
//! semantics live in exactly one place as before. Adding an entry can therefore
//! only be wrong by authorizing a pair some skipped guard would have claimed —
//! which is what the debug-build cross-check below exists to catch.
//!
//! # Adding an entry
//!
//! A `(shape, method)` pair may be added when, for every receiver of that
//! shape, the method name appears in none of these gates:
//!
//! * `try_native_method_raw`'s probes — `gist`, `raku`, `perl`, `clone`,
//!   `decode`, `cache`, `sink`, `WHICH`, `WHY`, `REPR`, `WHERE`, `Capture`,
//!   `map`, `grep`, `first`, `sort`, `head`, `tail`, `flat`, `squish`,
//!   `classify`, `categorize`, `combinations`, `permutations`, `pick`, `roll`,
//!   `keyof`, `contains`, `starts-with`, `ends-with`, `substr-eq`, `base`,
//!   `int-bounds`, `List`, `values`, `Slip`, `Seq`, `Array`, and the
//!   `write-int*`/`write-uint*`/`write-num*` family;
//! * `native_method_0arg`'s prologue — `dynamic`, `pending`, `nl-out`, `VAR`,
//!   and the native-int coercion family (`byte`, `int8`, `uint16`, ...);
//! * `dispatch_core`'s prologue — `AST`, `Instant`, `resume`, `throw`,
//!   `int-bounds`, and (for an array receiver) `to`, `pos`, `from`, `ast`.
//!
//! Everything else in those three prologues is gated on a receiver kind
//! [`DispatchShape`] already excludes (`Instance`, `Package`, `Mixin`,
//! `Scalar`, `Seq`, `LazyList`, `Proxy`, a lazy `Match`, `Uni`, `Version`,
//! `Capture`, `CompUnitDepSpec`, a shaped or lazy array, an itemized hash).

use crate::symbol::Symbol;
use crate::value::{DispatchShape, RuntimeError, Value};

/// The method names the table keys on, interned once.
///
/// Interned rather than compared as `&str`: the whole point of the table is
/// that the receiver's tag and the method's identity are both integers by the
/// time dispatch asks about them, so the lookup must not reintroduce the
/// string comparison the cascade already pays at every level.
struct Names {
    elems: Symbol,
    end: Symbol,
    chars: Symbol,
    bool_: Symbol,
}

fn names() -> &'static Names {
    static NAMES: std::sync::OnceLock<Names> = std::sync::OnceLock::new();
    NAMES.get_or_init(|| Names {
        elems: Symbol::intern("elems"),
        end: Symbol::intern("end"),
        chars: Symbol::intern("chars"),
        bool_: Symbol::intern("Bool"),
    })
}

/// Whether the family cascade alone decides `method_sym` for a receiver of
/// `shape` — see the module docs for what a new entry has to satisfy.
#[inline]
fn authorized(shape: DispatchShape, method_sym: Symbol) -> bool {
    let n = names();
    match shape {
        DispatchShape::Array => {
            method_sym == n.elems || method_sym == n.end || method_sym == n.bool_
        }
        DispatchShape::Hash => method_sym == n.elems || method_sym == n.bool_,
        DispatchShape::Str => method_sym == n.chars || method_sym == n.bool_,
    }
}

/// Answer a zero-argument method straight from the family cascade, or `None`
/// to take the ordinary path.
///
/// `None` is always safe: it just means the call walks the gauntlet as it did
/// before. The table only ever removes a walk whose outcome is already known.
#[inline]
pub(crate) fn try_dispatch(
    target: &Value,
    method_sym: Symbol,
) -> Option<Result<Value, RuntimeError>> {
    let shape = target.dispatch_shape()?;
    if !authorized(shape, method_sym) {
        return None;
    }
    let result = super::methods_0arg::dispatch_core_families(target, method_sym.as_str());
    debug_assert_matches_full_path(target, method_sym, result.as_ref());
    result
}

/// In debug builds, re-answer the call through the *full* pure-native path and
/// assert the two agree.
///
/// This is the maintenance net for the table. Its entries are authorized by an
/// argument about which guards can fire, and a later commit adding a guard has
/// no way of knowing it invalidated one. Running both paths over the whole TAP
/// suite (CI's `gc-stress-tap` / `jit-stress-tap` jobs build debug) turns that
/// silent divergence into a failing assertion.
///
/// It re-runs [`native_method_0arg`](super::methods_0arg::native_method_0arg),
/// which carries its own prologue and `dispatch_core`'s — the two the table
/// skips that could produce a *different value*. The VM-side gauntlet can only
/// ever decline (`return None`, handing the call to the interpreter), so a
/// divergence there is a missing-bypass bug rather than a wrong value, and is
/// covered by the suite itself rather than by this assertion.
///
/// Sound to run twice only because every authorized pair is side-effect free:
/// no `Seq` is consumed, no cache flag is set, no interpreter state is touched.
#[inline]
fn debug_assert_matches_full_path(
    target: &Value,
    method_sym: Symbol,
    fast: Option<&Result<Value, RuntimeError>>,
) {
    #[cfg(debug_assertions)]
    {
        let slow = super::methods_0arg::native_method_0arg(target, method_sym);
        let render = |r: Option<&Result<Value, RuntimeError>>| match r {
            None => "<declined>".to_string(),
            Some(Ok(v)) => format!("ok:{}", crate::runtime::gist_value(v)),
            Some(Err(e)) => format!("err:{}", e.message),
        };
        debug_assert_eq!(
            render(fast),
            render(slow.as_ref()),
            "fast_0arg table disagrees with the full path for .{} on a {:?} \
             receiver — an entry in `fast_0arg::authorized` is no longer valid \
             (a guard it assumed declines now claims this call)",
            method_sym.as_str(),
            target.dispatch_shape(),
        );
    }
    #[cfg(not(debug_assertions))]
    {
        let _ = (target, method_sym, fast);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Every name the table keys on must be one the family cascade actually
    /// answers for that shape — otherwise the entry buys nothing and the call
    /// falls through after paying for the lookup.
    #[test]
    fn authorized_pairs_are_answered_by_the_families() {
        let array = Value::array(vec![Value::int(1), Value::int(2)]);
        let hash = Value::hash(crate::value::ValueMap::default());
        let string = Value::str("hello".to_string());
        for target in [&array, &hash, &string] {
            let shape = target
                .dispatch_shape()
                .expect("fixture is one of the table's shapes");
            for name in ["elems", "end", "chars", "Bool"] {
                let sym = Symbol::intern(name);
                if !authorized(shape, sym) {
                    continue;
                }
                assert!(
                    try_dispatch(target, sym).is_some(),
                    "{shape:?}.{name} is authorized but the families decline it"
                );
            }
        }
    }

    /// The shape probe must refuse everything the gauntlet exists for.
    #[test]
    fn dispatch_shape_refuses_non_plain_receivers() {
        assert_eq!(
            Value::package(Symbol::intern("Any")).dispatch_shape(),
            None,
            "a type object must not take the table"
        );
        assert_eq!(
            Value::seq(vec![Value::int(1)]).dispatch_shape(),
            None,
            "a Seq must not take the table"
        );
        assert_eq!(Value::int(1).dispatch_shape(), None);
        assert_eq!(Value::NIL.dispatch_shape(), None);
    }
}
