//! The thread-shared lane of `Array`'s mutator rows (ADR-11276 §9.169).
//!
//! A plain lexical `@name` that more than one thread can reach has one
//! cross-thread home: the decl-site `ContainerRef` cell (ADR-0039 / ADR-0055)
//! when the binding holds one, otherwise the atomic store keyed by the name
//! (`__mutsu_atomic_arr::`). A mutation must go there, or a sibling thread's
//! push is lost to a stale snapshot. The VM's `CallMethodMut` used to answer
//! these calls from a copy of the mutators of its own; they are the first step
//! of the rows' handlers now, so the place decides the lane and the rows keep
//! the one implementation of each method.

use super::*;

impl Interpreter {
    /// Whether a mutating call on the binding `name` holding `target` takes
    /// the thread-shared lane: the binding is held in a shared cell (`celled`),
    /// or threads share variables and the name is not one this lineage
    /// re-declared (the store's entry would belong to the shadowed outer
    /// binding).
    // Cost: O(1), one env read.
    pub(crate) fn shared_array_lane_applies(&self, name: &str, target: &Value) -> bool {
        if !name.starts_with('@') || !self.threads.shared_vars_active {
            return false;
        }
        let celled = self.env().get(name).is_some_and(Value::is_container_ref);
        if celled {
            return Self::is_plain_lexical_array_name(name);
        }
        matches!(target.view(), ValueView::Array(..)) && !self.container_name_is_redeclared(name)
    }

    /// `method` (`push`, `unshift`, `append`, `prepend`, `pop`, `shift`,
    /// `splice`) on the thread-shared binding `name`, or `None` when the call
    /// takes the ordinary path (another method, no shared entry yet for a
    /// removal, a twigil'd name).
    // Cost: O(k) for k elements added (one type check each), O(1) for a pop or
    // shift, O(n) for a splice of an n-element array, each under the store's
    // or the cell's lock.
    pub(crate) fn shared_array_lane(
        &mut self,
        name: &str,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !self.shared_array_lane_applies(name, target) {
            return None;
        }
        let real_array = matches!(
            target.view(),
            ValueView::Array(_, crate::value::ArrayKind::Array)
        );
        let pop_or_shift = |data: &mut crate::value::ArrayData| {
            if data.items().is_empty() {
                crate::runtime::utils::make_empty_array_failure_what(method, "Array")
            } else if method == "shift" {
                data.remove(0)
            } else {
                data.pop().unwrap_or(Value::NIL)
            }
        };
        // A binding held in a shared `ContainerRef` cell: the cell already IS
        // the container's one cross-thread home, so a mutation goes into it
        // under its structure lock. The name-keyed store only knows the names
        // seeded when the thread was spawned, so a worker running a closure
        // over an `@a` declared after the spawn pushed into a worker-local entry
        // the declaring frame never saw (#11343).
        if self.env().get(name).is_some_and(Value::is_container_ref) {
            return match method {
                "push" | "unshift" | "append" | "prepend" if !args.is_empty() => {
                    let items = shared_lane_items(method, args);
                    if let Err(err) = self.check_container_element_types(name, target, &items) {
                        return Some(Err(err));
                    }
                    let items = self.wrap_native_int_items(None, name, items);
                    let front = matches!(method, "unshift" | "prepend");
                    let result = self.celled_array_extend(name, items, front)?;
                    crate::vm::vm_stats::record_dispatch_entry_intercept(
                        "callmethodmut",
                        "celled-array-push",
                    );
                    Some(Ok(result))
                }
                "pop" | "shift" if args.is_empty() && real_array => {
                    let (result, _) = self.celled_array_mutate(name, pop_or_shift)?;
                    crate::vm::vm_stats::record_dispatch_entry_intercept(
                        "callmethodmut",
                        "celled-array-pop-shift",
                    );
                    Some(Ok(result))
                }
                "splice" if real_array => {
                    let (removed, _) = self.celled_array_mutate(name, |data| {
                        crate::runtime::Interpreter::splice_array_data(data, args)
                    })?;
                    crate::vm::vm_stats::record_dispatch_entry_intercept(
                        "callmethodmut",
                        "celled-array-splice",
                    );
                    Some(Ok(Value::real_array(removed)))
                }
                _ => None,
            };
        }
        // Only a plain *lexical* `@name` is a single variable shared across
        // threads. Instance-attribute arrays (`@!order` / `@.order`) and other
        // twigil'd forms (`@*dyn`) have per-instance / per-context identity, so
        // they must NOT funnel into the global atomic store keyed by name —
        // that would accumulate pushes across every object (roles-6e.t DESTROY:
        // each `C1` instance's `@!order` doubled).
        let plain = Self::is_plain_lexical_array_name(name);
        match method {
            // Route through the atomic shared store. The base-key
            // `push_to_existing_shared_array`/`push_to_shared_var` write the
            // plain `@a` shared entry, which `set_shared_var` can clobber with a
            // stale empty snapshot during env sync — losing concurrent
            // `start { @a.push(...) }` updates from sibling threads. The
            // `__mutsu_atomic_arr::` store is exempt from that clobber, so
            // concurrent push/unshift serialize and all land. append/prepend
            // MUST funnel here too: once a push created the atomic entry, reads
            // prefer it, so an append applied to the stale base/env copy is
            // silently invisible (the zef `populate-distributions` bug).
            "push" | "unshift" | "append" | "prepend" if plain && !args.is_empty() => {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethodmut",
                    "shared-array-push-atomic",
                );
                // Stored through a native slot, so each element wraps to the
                // element width (`my uint8 @e; @e.push(1, 300, 2)` -> 1, 44, 2).
                let items = shared_lane_items(method, args);
                if let Err(err) = self.check_container_element_types(name, target, &items) {
                    return Some(Err(err));
                }
                let items = self.wrap_native_int_items(None, name, items);
                let front = matches!(method, "unshift" | "prepend");
                Some(Ok(self.shared_array_extend(name, items, front)))
            }
            "push" | "unshift" if !args.is_empty() => {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethodmut",
                    "shared-array-push-legacy",
                );
                if let Err(err) = self.check_container_element_types(name, target, args) {
                    return Some(Err(err));
                }
                let result = self
                    .push_to_existing_shared_array(name, args.to_vec())
                    .unwrap_or_else(|| self.push_to_shared_var(name, args.to_vec(), target));
                Some(Ok(result))
            }
            // Removal ops only lose updates once the atomic entry shadows the
            // base copy, so gate on its existence and keep the richer ordinary
            // path (arity/lazy/immutable errors, callable splice args) for the
            // unshadowed case.
            "pop" | "shift"
                if plain
                    && args.is_empty()
                    && real_array
                    && self.atomic_array_entry_exists(name) =>
            {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethodmut",
                    "shared-array-pop-shift",
                );
                let (result, _) = self.shared_array_mutate(name, |data, _| pop_or_shift(data));
                Some(Ok(result))
            }
            "splice" if plain && real_array && self.atomic_array_entry_exists(name) => {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethodmut",
                    "shared-array-splice",
                );
                let (removed, _) = self.shared_array_mutate(name, |data, _| {
                    crate::runtime::Interpreter::splice_array_data(data, args)
                });
                Some(Ok(Value::real_array(removed)))
            }
            _ => None,
        }
    }
}

/// The elements a growing method adds: `push`/`unshift` take each argument as
/// one element, `append`/`prepend` flatten (the one-argument rule).
// Cost: O(a), a = arguments (plus the elements a flattened one holds).
fn shared_lane_items(method: &str, args: &[Value]) -> Vec<Value> {
    if matches!(method, "push" | "unshift") {
        crate::runtime::Interpreter::normalize_push_unshift_args(args.to_vec())
    } else {
        crate::runtime::flatten_append_args(args.to_vec())
    }
}
