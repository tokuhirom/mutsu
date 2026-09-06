//! User-defined `WHICH` identity, deposited on the instance for the pure layer.
//!
//! Raku object identity is `.WHICH`-based: `===` is `$a.WHICH eq $b.WHICH`, and
//! Set/Bag/Mix elements plus object-hash keys are stored under the element's
//! `.WHICH`. A class may override `WHICH` to give its instances *value*
//! semantics:
//!
//! ```raku
//! class A { has $.a; method WHICH { ValueObjAt.new("A|$!a.WHICH()") } }
//! Set(A.new(a => 5)) eqv Set(A.new(a => 5));   # True
//! ```
//!
//! The consumers of that identity — [`crate::runtime::utils::value_which_key`]
//! and [`crate::runtime::utils::values_identical`] — live in the pure value
//! layer and are plain functions with no interpreter, so they cannot run the
//! user's method. Rather than thread `&mut Interpreter` through the whole
//! Set/Bag/Mix keying layer (`builtins/quanthash_coerce.rs`, `runtime/ops_set.rs`
//! and friends are deliberately interpreter-free), the interpreter computes the
//! answer at the points where it *is* in hand and deposits it on the instance
//! (`InstanceAttrs::which_memo`), where the pure layer reads it.
//!
//! Depositing happens before any store borrow is taken, so running the user's
//! method here can never re-enter a half-built Set or hash.

use crate::runtime::Interpreter;
use crate::value::{Value, ValueView};

thread_local! {
    /// Set while a user `WHICH` body is running. The `.WHICH` method-dispatch
    /// entry point warms its receiver's identity, and warming *is* a `.WHICH`
    /// call — so without this flag the two would recurse forever. Skipping the
    /// warm inside a `WHICH` body loses nothing: a nested `$!other.WHICH()`
    /// goes through ordinary method dispatch, which already returns the user's
    /// answer; only the pure identity layer needs the deposited memo.
    static COMPUTING_WHICH: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Runs `f` with the "inside a user `WHICH`" flag set, restoring it after.
fn with_computing_which<R>(f: impl FnOnce() -> R) -> R {
    let previous = COMPUTING_WHICH.with(|c| c.replace(true));
    let result = f();
    COMPUTING_WHICH.with(|c| c.set(previous));
    result
}

impl Interpreter {
    /// Compute and deposit the user-defined `WHICH` identity of `value` (and,
    /// for list-shaped values, of every element), so that the pure identity
    /// layer sees the user's answer.
    ///
    /// A no-op — and, for the overwhelmingly common case of a class that does
    /// not override `WHICH`, only a type-tag check plus an MRO probe — for
    /// everything else.
    pub(crate) fn warm_which_identity(&mut self, value: &Value) {
        self.warm_which_identity_depth(value, 0);
    }

    /// The identity key of `value`, resolving a user-defined `WHICH` first.
    ///
    /// Use this instead of [`crate::runtime::utils::value_which_key`] wherever
    /// the interpreter is in hand and the key is about to select a store slot
    /// (object-hash subscripts, QuantHash membership): the bare function cannot
    /// run the user's method and would key such an object by its per-object id.
    pub(crate) fn which_key(&mut self, value: &Value) -> String {
        self.warm_which_identity(value);
        crate::runtime::utils::value_which_key(value)
    }

    /// As [`Self::warm_which_identity`], for each of `values`.
    pub(crate) fn warm_which_identity_all(&mut self, values: &[Value]) {
        for v in values {
            self.warm_which_identity_depth(v, 0);
        }
    }

    fn warm_which_identity_depth(&mut self, value: &Value, depth: u32) {
        // Guard against a pathologically deep (or cyclic) nesting: the element
        // list of a Set is flattened at most a few levels in practice.
        if depth > 16 || COMPUTING_WHICH.with(|c| c.get()) {
            return;
        }
        match value.view() {
            ValueView::Instance { class_name, .. } => {
                let class_name = class_name.resolve();
                if !self.has_user_method(&class_name, "WHICH") {
                    return;
                }
                let target = value.clone();
                let computed = with_computing_which(|| {
                    self.call_method_with_values(target, "WHICH", Vec::new())
                });
                let Ok(which) = computed else {
                    // A `WHICH` that dies leaves the object on the fallback
                    // (per-object id) identity rather than aborting the
                    // operation that asked for the key.
                    return;
                };
                // Rakudo compares `.WHICH` with `eq`, so any stringifiable
                // return value works — `ObjAt`, `ValueObjAt` and a plain `Str`
                // all give value semantics (measured against raku 2026.07).
                value.set_user_which_memo(which.to_string_value().into());
            }
            // A Set/Bag/Mix is built from list-shaped arguments, so warm the
            // elements the keying layer is about to ask about.
            ValueView::Array(items, _) => {
                for item in items.iter() {
                    self.warm_which_identity_depth(item, depth + 1);
                }
            }
            ValueView::Slip(items) => {
                for item in items.iter() {
                    self.warm_which_identity_depth(item, depth + 1);
                }
            }
            ValueView::Seq(items) => {
                for item in items.iter() {
                    self.warm_which_identity_depth(item, depth + 1);
                }
            }
            ValueView::Pair(_, v) => self.warm_which_identity_depth(v, depth + 1),
            ValueView::ValuePair(k, v) => {
                self.warm_which_identity_depth(k, depth + 1);
                self.warm_which_identity_depth(v, depth + 1);
            }
            ValueView::ContainerRef(_) => {
                let inner = value.deref_container();
                self.warm_which_identity_depth(&inner, depth + 1);
            }
            _ => {}
        }
    }
}
