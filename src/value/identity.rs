//! Value identity: `===` (`values_identical`), `nqp::eqaddr`
//! (`values_same_object`) and container-preserving identity. Pure functions of
//! the values, so they live in `value` (#10779); `runtime::utils` re-exports them.

use crate::value::types::is_stash_class_name;
use crate::value::which_key::value_which_key;
use crate::value::{Value, ValueView};

/// `nqp::eqaddr`: are these the same object? Unlike `===`
/// ([`values_identical`]), this never consults `.WHICH`: two instances of a
/// class with a constant user `WHICH` are `===` but not the same object, and
/// two separately built strings with equal text are not the same object
/// either (both measured against rakudo). `eqaddr` used to share `===`'s
/// helper, whose user-`WHICH` memo made its answer depend on whether a `===`
/// had run on the pair first.
///
/// Values mutsu stores unboxed (small integers, nums, rationals) have no
/// identity of their own and compare by value, as MoarVM's cached small
/// integers do.
// Cost: O(1).
pub(crate) fn values_same_object(left: &Value, right: &Value) -> bool {
    match (left.view(), right.view()) {
        (ValueView::Instance { id: a, .. }, ValueView::Instance { id: b, .. }) => a == b,
        (ValueView::Instance { .. }, _) | (_, ValueView::Instance { .. }) => false,
        (ValueView::Str(a), ValueView::Str(b)) => std::sync::Arc::ptr_eq(&a, &b),
        _ => values_identical(left, right),
    }
}

pub(crate) fn values_identical(left: &Value, right: &Value) -> bool {
    match (left.view(), right.view()) {
        // `.WHICH` is the identity of the value, not of the container holding
        // it: an array element read through its aliasing cell (a `.sort`
        // leaves `@b`'s slots as `ContainerRef`s) is still the same List.
        // Without this the pair fell through to the structural `eqv` arm, so
        // two distinct but equal Lists tested `∈` each other (Game::Entities'
        // t/sorting.t).
        (ValueView::Scalar(_) | ValueView::ContainerRef(_), _)
        | (_, ValueView::Scalar(_) | ValueView::ContainerRef(_)) => values_identical(
            &left.descalarize().deref_container(),
            &right.descalarize().deref_container(),
        ),
        // The public `Metamodel::*HOW` names and the internal
        // `Perl6::Metamodel::*HOW` names denote the same singleton type
        // objects.  The latter is what `.HOW.WHAT` reports, while user code
        // (including Method::Protected) compares it with the former.
        (ValueView::Package(a), ValueView::Package(b)) => {
            fn canonical_meta_name(name: &str) -> &str {
                match name.strip_prefix("Perl6::") {
                    Some(rest) if rest.starts_with("Metamodel::") && rest.ends_with("HOW") => rest,
                    _ => name,
                }
            }
            let a_name = a.resolve();
            let b_name = b.resolve();
            canonical_meta_name(&a_name) == canonical_meta_name(&b_name)
        }
        (ValueView::Package(name), ValueView::Int(0))
        | (ValueView::Int(0), ValueView::Package(name))
            if name.resolve() == "int" =>
        {
            true
        }
        (ValueView::Array(a, _), ValueView::Array(b, _)) => crate::gc::Gc::ptr_eq(&a, &b),
        (ValueView::Seq(a), ValueView::Seq(b)) => std::sync::Arc::ptr_eq(&a, &b),
        (ValueView::Slip(a), ValueView::Slip(b)) => {
            // Empty is a singleton semantic value in Raku even when represented
            // by distinct empty Slip allocations.
            (a.is_empty() && b.is_empty()) || std::sync::Arc::ptr_eq(&a, &b)
        }
        (ValueView::LazyList(a), ValueView::LazyList(b)) => crate::gc::Gc::ptr_eq(&a, &b),
        (ValueView::Hash(a), ValueView::Hash(b)) => crate::gc::Gc::ptr_eq(&a, &b),
        // RakuAST nodes are Arc-backed model objects. Cloning a Value keeps
        // the same node allocation, while separately constructed but
        // structurally equal nodes get different allocations.
        (ValueView::RakuAst(a), ValueView::RakuAst(b)) => std::ptr::eq(a, b),
        (ValueView::Sub(a), ValueView::Sub(b)) => {
            if crate::gc::Gc::ptr_eq(&a, &b) {
                return true;
            }
            // Named subs with the same package and name are identical
            // (e.g. &foo === &EXPORT::ALL::foo when both resolve to the same definition)
            let a_name = a.name.resolve();
            let b_name = b.name.resolve();
            !a_name.is_empty() && a_name == b_name && a.package == b.package
        }
        (ValueView::WeakSub(a), ValueView::WeakSub(b)) => crate::gc::WeakGc::ptr_eq(&a, &b),
        // A regex is a code object: identical only to an alias of itself, not
        // to another evaluation of the same literal (`eqv` stays structural).
        (
            ValueView::Regex(_) | ValueView::RegexWithAdverbs(_),
            ValueView::Regex(_) | ValueView::RegexWithAdverbs(_),
        ) => left.regex_identity() == right.regex_identity(),
        // `===` is `.WHICH eq .WHICH`: the BASE value's identity plus the
        // composed type. Comparing the raw `overrides` maps could never answer
        // True for two separately-built values, because every role application
        // stamps its own `__mutsu_role_seq__` — `(1 but A) === (1 but A)` was
        // False. `mixin_identity_key` drops that stamp (keeping only its order)
        // and the per-instance `__mutsu_attr__*` values, and keeps everything
        // else. The inner is compared by IDENTITY, not `eqv`: the base of
        // `[1, 2] but A` is a reference type, so two of them are not `===`
        // (raku agrees) even though they are `eqv`.
        (ValueView::Mixin(a_inner, a_mix), ValueView::Mixin(b_inner, b_mix)) => {
            values_identical(a_inner, b_inner)
                && crate::value::types::mixin_identity_key(a_mix)
                    == crate::value::types::mixin_identity_key(b_mix)
        }
        (ValueView::Mixin(_, _), _) | (_, ValueView::Mixin(_, _)) => false,
        (
            ValueView::Instance {
                class_name: a_class,
                id: a_id,
                attributes: a_attrs,
                ..
            },
            ValueView::Instance {
                class_name: b_class,
                id: b_id,
                attributes: b_attrs,
                ..
            },
        ) => {
            let a_name = a_class.resolve();
            let b_name = b_class.resolve();
            // `===` is `$a.WHICH eq $b.WHICH`. When either side's class
            // overrides `WHICH`, that user answer decides identity — two
            // distinct objects with the same `WHICH` are `===`. The interpreter
            // deposits the computed string on the instance (a pure function
            // cannot run the method); see `InstanceAttrs::which_memo`.
            let a_which = left.user_which_memo();
            let b_which = right.user_which_memo();
            if a_which.is_some() || b_which.is_some() {
                // One side overriding `WHICH` and the other not is a plain
                // mismatch, which `Option` equality already gives us.
                return a_which == b_which;
            }
            if a_name == b_name
                && (is_stash_class_name(a_name.as_str())
                    || a_name == "Supply"
                    || a_name == "IO::Special")
            {
                left.eqv(right)
            } else if a_name == b_name && (a_name == "ObjAt" || a_name == "ValueObjAt") {
                // ObjAt/ValueObjAt instances are === when their WHICH content matches
                let a_val = a_attrs.as_map().objat_which();
                let b_val = b_attrs.as_map().objat_which();
                a_val == b_val
            } else if a_name == b_name && a_name == "IO::Handle" {
                // An `IO::Handle` value is a thin wrapper around an entry in the
                // interpreter's handle table, and mutsu re-wraps the same entry
                // into a fresh instance whenever a handle is handed back --
                // `$*OUT.open(:w)` returns the standard handle itself, which
                // raku reports as `=== $*OUT`. Two wrappers over the same table
                // id are therefore the same handle. An unopened handle
                // (`IO::Handle.new(:path($p))`) carries no id and keeps plain
                // instance identity, so two of those are not `===`.
                match (
                    a_attrs.as_map().get("handle"),
                    b_attrs.as_map().get("handle"),
                ) {
                    (Some(a_h), Some(b_h)) => a_h.to_string_value() == b_h.to_string_value(),
                    _ => a_id == b_id,
                }
            } else if a_name == b_name
                && a_attrs.as_map().contains_key("__mutsu_version_value")
                && b_attrs.as_map().contains_key("__mutsu_version_value")
            {
                // A Version subclass is a value type, like Version itself.
                value_which_key(left) == value_which_key(right)
            } else if a_name == b_name && matches!(a_name.as_str(), "Date" | "DateTime") {
                // Date and DateTime are value types: their native WHICH is
                // derived from the calendar value, not the allocated instance.
                value_which_key(left) == value_which_key(right)
            } else if a_name == b_name
                && a_name.starts_with("Perl6::Metamodel::")
                && a_name.ends_with("HOW")
            {
                // A metaobject (`.HOW`) is canonical per introspected type in raku:
                // `1.HOW === 2.HOW === Int.HOW` are all True (the same ClassHOW),
                // while `1.HOW === Num.HOW` is False. mutsu allocates a fresh HOW
                // instance per `.HOW` call, so compare by the introspected type
                // name (stored in the `name` attribute) rather than instance id.
                let a_val = a_attrs.as_map().get("name").map(|v| v.to_string_value());
                let b_val = b_attrs.as_map().get("name").map(|v| v.to_string_value());
                a_val == b_val
            } else {
                a_id == b_id
            }
        }
        // A Version's identity is its canonical string (`.WHICH` is
        // `Version|1.02.3`), so two versions that compare equal but are spelled
        // differently are NOT `===`: `v1.02.3 == v1.2.3` and they are `eqv`,
        // but `v1.02.3 === v1.2.3` is False.
        (ValueView::Version { .. }, ValueView::Version { .. }) => {
            left.to_string_value() == right.to_string_value()
        }
        // Junction identity: each junction object is unique
        (
            ValueView::Junction { values: a_vals, .. },
            ValueView::Junction { values: b_vals, .. },
        ) => std::sync::Arc::ptr_eq(&a_vals, &b_vals),
        // Capture identity is NOT structural: a Capture's `.WHICH` keeps the
        // *container* identity of each captured element, so `\($a) === \($b)`
        // is False even when `$a` and `$b` hold equal values, while
        // `\(42) === \(42)` is True (both literal-value elements). This is
        // unlike `eqv`, which deconts and compares structurally.
        (
            ValueView::Capture {
                positional: ap,
                named: an,
            },
            ValueView::Capture {
                positional: bp,
                named: bn,
            },
        ) => {
            ap.len() == bp.len()
                && an.len() == bn.len()
                && ap
                    .iter()
                    .zip(bp.iter())
                    .all(|(x, y)| container_identity_identical(x, y))
                && an.iter().all(|(k, v)| {
                    bn.get(k)
                        .is_some_and(|bv| container_identity_identical(v, bv))
                })
        }
        _ => left.eqv(right),
    }
}

/// Identity comparison that retains the container identity of each side.
/// Unlike a top-level `===` (which deconts a bound scalar to its value), two
/// `ContainerRef` cells are identical only when they are the same `Arc` (i.e.
/// bound to the same container), and a container can never be identical to a
/// plain value. Non-container values fall back to the normal value identity
/// rules. Used for Capture elements, and by the closure-return caller-env
/// writeback to detect a *binding* change (a `:=` promotion of a captured
/// name to a shared cell) even when the cell's inner value still compares
/// equal to the plain capture-time snapshot.
pub(crate) fn container_identity_identical(a: &Value, b: &Value) -> bool {
    match (a.view(), b.view()) {
        (ValueView::ContainerRef(x), ValueView::ContainerRef(y)) => crate::gc::Gc::ptr_eq(&x, &y),
        (ValueView::ContainerRef(_), _) | (_, ValueView::ContainerRef(_)) => false,
        _ => values_identical(a, b),
    }
}
