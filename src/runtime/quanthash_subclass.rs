//! Backing storage for user classes that inherit a QuantHash base
//! (`Set`/`SetHash`/`Bag`/`BagHash`/`Mix`/`MixHash`).
//!
//! `class AccountableBagHash is BagHash { }` is the same shape as
//! `class C is Hash { }` and `class C is Array { }`, which already keep their
//! real data in a reserved backing attribute (`__mutsu_hash_storage` /
//! `__mutsu_array_storage`) and delegate the protocol methods to it. QuantHash
//! subclasses use `__baggy_data__` for the same purpose — the attribute the
//! `Bag`-like delegation in `builtins/methods_0arg` and `methods_narg` already
//! reads.
//!
//! The base picked out of the MRO decides both the element semantics
//! (`Setty` counts nothing, `Baggy` counts `Int` weights, `Mixy` counts real
//! ones) and the mutability: a `BagHash` subclass must be backed by a MUTABLE
//! bag so `%b<a> = 5` works, while a `Bag` subclass keeps the immutable one so
//! the same assignment still raises, exactly as
//! [`Interpreter::positional_base_storage`] picks `List` over `Array`.

use super::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

/// The QuantHash bases whose subclasses get `__baggy_data__` backing storage,
/// most-derived first so an MRO scan picks the mutable flavour when both are
/// present (`BagHash`'s own MRO does not contain `Bag`, but a user class can
/// name either).
const QUANTHASH_BASES: &[&str] = &["SetHash", "BagHash", "MixHash", "Set", "Bag", "Mix"];

impl Interpreter {
    /// The QuantHash base `class_key` inherits, if any — the name the backing
    /// store is built as.
    ///
    /// The DECLARED parent chain is walked, not the MRO, and a parent that is
    /// itself a user-declared class never counts as the base however it is
    /// spelled: a lexical `class Set is Hash { }` shadows the builtin, and so
    /// does anything inheriting it (`class Child is Set { }`), so neither is a
    /// QuantHash at all. An MRO scan cannot tell the two apart — the shadowing
    /// class's own name IS in its MRO.
    pub(crate) fn quanthash_base_kind(&mut self, class_key: &str) -> Option<&'static str> {
        self.quanthash_base_kind_seen(class_key, &mut Vec::new())
    }

    fn quanthash_base_kind_seen(
        &mut self,
        class_key: &str,
        seen: &mut Vec<String>,
    ) -> Option<&'static str> {
        let (key, parents) = self.resolved_class_parents_for_quanthash(class_key)?;
        if seen.contains(&key) {
            return None;
        }
        seen.push(key);
        for parent in &parents {
            // A user-declared parent is the shadowing case: recurse into ITS
            // own parents rather than reading its name as a builtin.
            if self.resolved_class_parents_for_quanthash(parent).is_some() {
                if let Some(found) = self.quanthash_base_kind_seen(parent, seen) {
                    return Some(found);
                }
                continue;
            }
            if let Some(found) = QUANTHASH_BASES.iter().copied().find(|b| *b == parent) {
                return Some(found);
            }
        }
        None
    }

    /// The backing store a fresh QuantHash-subclass instance gets, folded out
    /// of `items` (the constructor's positional arguments; empty for `C.new`).
    ///
    /// Built through the base type's OWN `.new`, not through the `.Set`/`.Bag`
    /// coercion: the two disagree on flattening. `Set.new([1,2], 3)` is two
    /// elements — each argument is taken whole — while `([1,2], 3).Set` is
    /// three (`roast/S02-types/set.t`'s "Can subclass Set").
    pub(crate) fn quanthash_base_storage(
        &mut self,
        base: &str,
        items: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        self.try_native_quanthash_construct(Symbol::intern(base), base, &None, items)
    }

    /// Seed `attrs` with `__baggy_data__` when `class_key` inherits a QuantHash
    /// base and the constructor did not already supply one. Shared by the two
    /// constructor paths (`dispatch_new` and `bless`), the same way the
    /// `Hash`/`Array` blocks beside each call site are.
    pub(crate) fn seed_quanthash_storage(
        &mut self,
        class_key: &str,
        attrs: &mut crate::runtime::AttrMap,
        positional_args: &[Value],
    ) -> Result<(), RuntimeError> {
        if attrs.contains_key("__baggy_data__") {
            return Ok(());
        }
        let Some(base) = self.quanthash_base_kind(class_key) else {
            return Ok(());
        };
        let storage = self.quanthash_base_storage(base, positional_args.to_vec())?;
        attrs.insert("__baggy_data__".to_string(), storage);
        Ok(())
    }
}

impl Interpreter {
    /// The QuantHash twin of `native_hash_storage_next_candidate`: when a user
    /// `is BagHash`/`is Set`/... subclass (or a role composed into one)
    /// overrides an Associative-protocol method and calls `nextsame`/`nextwith`
    /// (or `callsame`/`callwith`), the NATIVE QuantHash behavior on the
    /// instance's backing `__baggy_data__` is the final base candidate.
    ///
    /// This is what makes `AccountableBagHash`'s `multi method ASSIGN-KEY(...)
    /// { ... nextsame() ... }` land on the real bag: the role's candidate is
    /// the only user one in the MRO, so without a native base the deferral
    /// simply answered `Nil` and the weight was never written.
    pub(crate) fn native_baggy_storage_next_candidate(
        &mut self,
        override_args: Option<&[Value]>,
    ) -> Option<Result<Value, RuntimeError>> {
        let ctx = self.samewith_context_stack.last().cloned();
        let method_name = ctx.as_ref().map(|c| c.name.clone())?;
        let invocant = self
            .method_dispatch_stack
            .last()
            .map(|f| f.invocant.clone())
            .or_else(|| ctx.as_ref().and_then(|c| c.invocant.clone()))
            .or_else(|| self.env.get("self").cloned())?;
        let args: Vec<Value> = match override_args {
            Some(a) => a.to_vec(),
            None => self
                .method_dispatch_stack
                .last()
                .map(|f| f.args.clone())
                .or_else(|| ctx.as_ref().and_then(|c| c.args.clone()))
                .unwrap_or_default(),
        };
        let ValueView::Instance { attributes, .. } = invocant.view() else {
            return None;
        };
        if !attributes.contains_key("__baggy_data__") {
            return None;
        }
        // The mutating half writes into the SHARED backing storage in place
        // (interior mutability via `with_attr_mut`), so the invocant the user
        // method still holds sees the new weight — the same shape the
        // `__mutsu_hash_storage` analog uses.
        if matches!(method_name.as_str(), "ASSIGN-KEY" | "DELETE-KEY") && !args.is_empty() {
            let assigned = if method_name == "ASSIGN-KEY" {
                args.get(1).cloned().unwrap_or(Value::NIL)
            } else {
                Value::int(0)
            };
            let (key, elem) = crate::runtime::utils::quanthash_elem_entry(&args[0]);
            let mut outcome: Result<Value, RuntimeError> = Ok(assigned.clone());
            let applied = attributes.with_attr_mut("__baggy_data__", |storage| {
                match Self::quanthash_with_weight(storage, key, Some(&elem), &assigned) {
                    Ok(Some(updated)) => {
                        *storage = updated;
                    }
                    Ok(None) => {
                        outcome = Err(RuntimeError::assignment_ro_typename(
                            crate::runtime::value_type_name(storage),
                            &crate::runtime::utils::gist_value(storage),
                        ));
                    }
                    Err(e) => outcome = Err(e),
                }
            });
            applied?;
            return Some(outcome);
        }
        if method_name == "STORE" {
            let mut outcome: Result<Value, RuntimeError> = Ok(invocant.clone());
            let applied = attributes.with_attr_mut("__baggy_data__", |storage| {
                match crate::runtime::quanthash_store::quanthash_store(storage, &args) {
                    Some(stored) => *storage = stored,
                    None => {
                        outcome = Err(RuntimeError::assignment_ro_typename(
                            crate::runtime::value_type_name(storage),
                            &crate::runtime::utils::gist_value(storage),
                        ))
                    }
                }
            });
            applied?;
            return Some(outcome);
        }
        let method_sym = Symbol::intern(&method_name);
        attributes.with_attr_mut("__baggy_data__", |storage| {
            self.try_native_method(storage, method_sym, &args)
        })?
    }
}
