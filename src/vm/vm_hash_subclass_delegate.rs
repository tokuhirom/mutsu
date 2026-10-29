//! `is Hash`/`is Map` subclass instance delegation: an `Instance` whose class
//! inherits from the builtin `Hash` (or `Map`) keeps its key/value data in the
//! backing `__mutsu_hash_storage` attribute (set up at construction time by
//! `Interpreter::associative_base_storage`, see `runtime/accessors_state.rs`)
//! and delegates every Associative-protocol method to it. This mirrors the
//! `__mutsu_array_storage` delegation the `is Array`/`is List` subclasses
//! already have (the big block in `vm_call_method_mut_ops.rs` starting near
//! `__mutsu_array_storage`, and the read-only twin in
//! `vm_call_method_ops.rs`/`vm_call_method_compiled_interpret.rs`).
//!
//! Unlike Array (whose mutators needed hand-written Rust fast paths —
//! `native_array_storage_mut` — because a plain `real_array` has no rich
//! native coverage of its own), a plain `Hash` value already has full native
//! method coverage (AT-KEY/ASSIGN-KEY/BIND-KEY/DELETE-KEY/EXISTS-KEY/keys/
//! values/kv/pairs/push/.../raku/gist/...) through the SAME dispatch a named
//! `%h` variable uses. So this delegation just re-targets that existing
//! dispatch at the storage value through a synthetic env binding (exactly the
//! fallback branch the Array block itself uses for its own non-fast-path
//! methods), then writes the (possibly mutated) storage back into the
//! instance's attribute cell — no new slow-path mechanism, just reuse of the
//! existing native Hash dispatch under a different receiver.

use super::*;
use crate::value::ValueMap;

impl Interpreter {
    /// Methods delegated to `__mutsu_hash_storage`. Broader than the Array
    /// allowlist needs to be defensive about (Hash's native coverage is
    /// already comprehensive), but still curated rather than "everything" so
    /// a method the class genuinely wants handled by ordinary Instance
    /// dispatch (`.new`, `.WHAT`, `does`, `isa`, an inherited/overridden user
    /// method — already excluded via `has_user_method` at the call site) is
    /// never intercepted here.
    fn is_hash_storage_method(method: &str) -> bool {
        matches!(
            method,
            "AT-KEY"
                | "ASSIGN-KEY"
                | "BIND-KEY"
                | "DELETE-KEY"
                | "EXISTS-KEY"
                | "push"
                | "append"
                | "elems"
                | "keys"
                | "values"
                | "kv"
                | "pairs"
                | "antipairs"
                | "invert"
                | "clear"
                | "list"
                | "List"
                | "array"
                | "Array"
                | "Seq"
                | "seq"
                | "Slip"
                | "slip"
                | "hash"
                | "Hash"
                | "Map"
                | "iterator"
                | "Bool"
                | "Numeric"
                | "Int"
                | "Str"
                | "gist"
                | "raku"
                | "Stringy"
                | "STORE"
                | "dynamic"
                | "min"
                | "max"
                | "minmax"
                | "sum"
                | "reduce"
                | "produce"
                | "sort"
                | "unique"
                | "squish"
                | "flat"
                | "map"
                | "grep"
                | "first"
                | "classify"
                | "categorize"
                | "classify-list"
                | "categorize-list"
                | "pick"
                | "roll"
                | "all"
                | "any"
                | "none"
                | "one"
        )
    }

    /// Hash-subclass instance delegation (mut path): when the Instance's
    /// class inherits from `Hash`/`Map` and the method isn't user-defined,
    /// delegate an Associative-protocol method call to the backing
    /// `__mutsu_hash_storage` attribute and write the result back into the
    /// instance. Returns `None` to fall through to the rest of `CallMethodMut`
    /// dispatch (a user override, an unrelated class, or a method this
    /// delegation doesn't cover).
    pub(super) fn try_hash_storage_delegate_mut(
        &mut self,
        target_name: &str,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        self.hash_storage_delegate_mut_inner(target_name, target, method, args, true)
    }

    /// A qualified call naming the Associative base (`self.Hash::BIND-KEY(...)`,
    /// `$o.Hash::AT-KEY(...)`) from a `Hash`/`Map` subclass: runs the base
    /// implementation on the backing storage even when the subclass overrides
    /// the method, since the qualifier names the parent's version on purpose.
    // Cost: O(1) dispatch plus the delegated method's own cost.
    pub(crate) fn try_hash_storage_delegate_qualified(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        self.hash_storage_delegate_mut_inner(
            "__mutsu_hash_qualified_tmp",
            target,
            method,
            args,
            false,
        )
    }

    fn hash_storage_delegate_mut_inner(
        &mut self,
        target_name: &str,
        target: &Value,
        method: &str,
        args: &[Value],
        honor_user_override: bool,
    ) -> Option<Result<Value, RuntimeError>> {
        let pun_inner = Self::hash_pun_inner(target);
        let outer_target = target;
        let target = pun_inner.as_ref().unwrap_or(target);
        let ValueView::Instance {
            class_name: inst_class,
            attributes,
            id: inst_id,
        } = target.view()
        else {
            return None;
        };
        if !Self::is_hash_storage_method(method) {
            return None;
        }
        let cn = inst_class.resolve();
        if honor_user_override
            && (self.has_user_method(&cn, method)
                || (self.registry().role_associative_base(&cn).is_some()
                    && self.has_user_method_including_role(&cn, method)))
        {
            return None;
        }
        if !attributes.contains_key(crate::symbol::wk::hash_storage()) {
            return None;
        }
        if !self
            .mro_readonly(&cn)
            .iter()
            .any(|n| Self::is_associative_base(n))
            && !self.registry().role_associative_base(&cn).is_some()
        {
            return None;
        }
        let storage = attributes
            .as_map()
            .get(crate::symbol::wk::hash_storage())
            .cloned()
            .unwrap_or_else(|| Value::hash(ValueMap::default()));
        // `%h = pairs` / `my %h is Bar = pairs` (the tied-variable declaration
        // and reassignment paths — `vm_var_trait_ops.rs`/`vm_var_assign_local.rs`
        // — both dispatch a `STORE(list, :INITIALIZE)` call): wholesale-replace
        // the backing storage from the flattened value list, mirroring how a
        // plain `%h = pairs` assignment repopulates a native Hash. Handled here
        // directly rather than by re-targeting the native dispatch (a plain
        // `Hash` value has no native `STORE` method of its own to delegate
        // to — see the module doc comment).
        if method == "STORE" {
            let items = args.first().map(crate::runtime::utils::value_to_list);
            let mut map = ValueMap::default();
            if let Some(items) = items {
                let mut iter = items.into_iter();
                while let Some(item) = iter.next() {
                    match item.view() {
                        ValueView::Pair(k, v) => {
                            map.insert(k.to_string(), v.clone());
                        }
                        ValueView::ValuePair(k, v) => {
                            map.insert(k.to_string_value(), v.clone());
                        }
                        // A flat (non-Pair) kv list: consecutive key/value pairs.
                        _ => {
                            let key = item.to_string_value();
                            let value = iter.next().unwrap_or(Value::NIL);
                            map.insert(key, value);
                        }
                    }
                }
            }
            let new_storage = Value::hash(map);
            // Replace the existing backing hash's contents in place: every
            // holder of this instance -- a method's `self`, the caller's
            // `%h` -- shares that node, so `self.STORE(...)` (or `self = ...`)
            // inside a method is seen by the caller. Rebuilding the instance
            // with a fresh storage node only reached the name written back.
            if let Some(old_storage) = attributes.as_map().get(crate::symbol::wk::hash_storage())
                && let ValueView::Hash(old_gc) = old_storage.view()
                && let ValueView::Hash(new_gc) = new_storage.view()
            {
                // Keep the node's declared type: a `Map` subclass's storage is
                // tagged `Map`, which is what makes it immutable.
                Self::hash_inplace_reassign_inheriting_meta(&old_gc, &new_gc);
                return Some(Ok(outer_target.clone()));
            }
            let updated_instance = self.write_back_hash_storage_instance(
                target_name,
                &inst_class,
                &attributes,
                inst_id,
                new_storage,
            );
            return Some(Ok(updated_instance));
        }
        // `self.BIND-KEY($k, $v)` from inside the subclass (WriteOnceHash's
        // `STORE`/`ASSIGN-KEY`): binds the key in the backing hash in place,
        // like `STORE` above, so every holder of the instance sees it. The
        // native `BIND-KEY` lives on the opcode path, which a re-targeted
        // by-value dispatch never reaches.
        if method == "BIND-KEY" && args.len() == 2 && matches!(storage.view(), ValueView::Hash(_)) {
            // Bound as the `Hash` row binds it (a cell, so `.values` hands the
            // bound value out and not a fresh Scalar), on the shared node.
            return Some(
                crate::builtins::method_table::bind_key_in(self, &storage, args, None)
                    .map(|(value, _)| value),
            );
        }
        // The renderings mutate nothing, and the read-only twin already names
        // an `is Map` subclass in them.
        if matches!(method, "raku" | "gist") && honor_user_override {
            return self.try_hash_storage_delegate(
                target,
                crate::symbol::Symbol::intern(method),
                args,
            );
        }
        // A qualified rendering (`self.Map::raku`, the user-override-bypassing
        // twin of the branch above) still names the instance's own class.
        if matches!(method, "raku" | "gist") {
            return Some(
                self.vm_call_method_with_values(storage.clone(), method, args.to_vec())
                    .map(|text| {
                        Value::str(crate::value::raku_repr::rename_map_subclass_repr(
                            &cn,
                            &storage,
                            text.to_string_value(),
                        ))
                    }),
            );
        }
        // `.values` hands out the backing hash's own element containers, as it
        // does for a plain `Hash` (a bound element stays a bound value rather
        // than a fresh Scalar).
        if method == "values"
            && args.is_empty()
            && !self
                .native_lever_a_user_override_sym(&storage, crate::symbol::Symbol::intern(method))
            && let Some(produced) = self.try_element_container_producer(&storage, method, args)
        {
            return Some(Ok(produced));
        }
        // Seed a synthetic binding so the native xxKEY fast paths (which
        // write back into `self.env` by NAME — see `vm_call_method_mut_ops.rs`)
        // have somewhere to write the mutated hash.
        self.env_mut()
            .insert("__mutsu_hash_tmp".to_string(), storage.clone());
        let dispatched = self.call_method_mut_with_values(
            "__mutsu_hash_tmp",
            storage.clone(),
            method,
            args.to_vec(),
        );
        let dispatched = match dispatched {
            Ok(v) => Ok(v),
            // Try non-mut dispatch for read-only methods the mut fast paths
            // above don't special-case.
            Err(_) => self.vm_call_method_with_values(storage.clone(), method, args.to_vec()),
        };
        let result = match dispatched {
            Ok(v) => v,
            Err(e) => {
                self.env_mut().remove("__mutsu_hash_tmp");
                return Some(Err(e));
            }
        };
        let updated_storage = self
            .env()
            .get("__mutsu_hash_tmp")
            .cloned()
            .unwrap_or_else(|| storage.clone());
        self.env_mut().remove("__mutsu_hash_tmp");
        let updated_instance = self.write_back_hash_storage_instance(
            target_name,
            &inst_class,
            &attributes,
            inst_id,
            updated_storage,
        );
        // Raku's base `Hash.push`/`.append` return the invocant itself,
        // matching the Array analog's `push`/`append`/`prepend`/`unshift`.
        Some(Ok(match method {
            "push" | "append" => updated_instance,
            _ => result,
        }))
    }

    /// Non-mut (read-only) twin of [`Self::try_hash_storage_delegate_mut`],
    /// used by the `CallMethod` opcode path (`vm_call_method_ops.rs`) and the
    /// compiled-dispatch entry (`vm_call_method_compiled_interpret.rs`) —
    /// callers that have no named receiver to write a mutation back through.
    /// Only ever dispatches read-only methods: it borrows the storage
    /// immutably via [`Self::try_native_method`], exactly like the Array
    /// analog's non-mut delegation.
    pub(super) fn try_hash_storage_delegate(
        &mut self,
        target: &Value,
        method_sym: crate::symbol::Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let method = method_sym.as_str();
        let pun_inner = Self::hash_pun_inner(target);
        let target = pun_inner.as_ref().unwrap_or(target);
        let ValueView::Instance {
            class_name: inst_class,
            attributes,
            ..
        } = target.view()
        else {
            return None;
        };
        if !Self::is_hash_storage_method(method) {
            return None;
        }
        let cn = inst_class.resolve();
        if self.has_user_method(&cn, method)
            || (self.registry().role_associative_base(&cn).is_some()
                && self.has_user_method_including_role(&cn, method))
        {
            return None;
        }
        if !attributes.contains_key(crate::symbol::wk::hash_storage()) {
            return None;
        }
        if !self
            .mro_readonly(&cn)
            .iter()
            .any(|n| Self::is_associative_base(n))
            && !self.registry().role_associative_base(&cn).is_some()
        {
            return None;
        }
        // `STORE` (bulk-replace) is a mutating method with no plain-`Value`
        // native counterpart to read from — route it through the mut
        // delegate's special-cased handling instead, via a synthetic
        // (unread) target name: this call site (the non-mut `CallMethod`
        // path / `try_compiled_method_or_interpret`) has no named receiver
        // of its own, but the mut delegate only needs *some* name to seed
        // its writeback bookkeeping — its actual RESULT (the updated
        // instance) is what callers here care about (e.g. the tied-variable
        // `STORE` dispatch in `vm_var_trait_ops.rs`/`vm_var_assign_local.rs`,
        // which binds the returned instance to the variable itself).
        if method == "STORE" {
            return self.try_hash_storage_delegate_mut(
                "__mutsu_hash_store_tmp",
                target,
                method,
                args,
            );
        }
        let storage = attributes
            .as_map()
            .get(crate::symbol::wk::hash_storage())
            .cloned()
            .unwrap_or_else(|| Value::hash(ValueMap::default()));
        let result = self.try_native_method(&storage, method_sym, args);
        // An `is Map` subclass names itself in its `.raku`/`.gist`.
        if matches!(method, "raku" | "gist")
            && let Some(Ok(text)) = &result
        {
            return Some(Ok(Value::str(
                crate::value::raku_repr::rename_map_subclass_repr(
                    &cn,
                    &storage,
                    text.to_string_value(),
                ),
            )));
        }
        result
    }

    /// Rebuild an `is Hash`/`is Map`-backed instance with its
    /// `__mutsu_hash_storage` attribute replaced by `storage` and write it
    /// back into `target_name`. Mirrors
    /// `write_back_array_storage_instance` in `vm_call_method_mut_ops.rs`.
    fn write_back_hash_storage_instance(
        &mut self,
        target_name: &str,
        inst_class: &crate::symbol::Symbol,
        attributes: &crate::gc::Gc<crate::value::InstanceAttrs>,
        inst_id: u64,
        storage: Value,
    ) -> Value {
        // Update the backing storage in the instance's shared attribute cell:
        // every holder of the instance -- a method's `self`, the caller's
        // variable -- sees it, and the attributes the running method wrote
        // since this call began are kept. Rebuilding the instance from the
        // attribute snapshot taken before the call dropped those writes (a
        // `%!attr<k> = v` after a `self.elems`, #12907).
        attributes.insert("__mutsu_hash_storage".to_string(), storage);
        let updated_instance =
            Value::instance_parts(*inst_class, crate::gc::Gc::clone(attributes), inst_id);
        // Through the shared cell, not over it: see `Env::insert_through`.
        self.env_mut()
            .insert_through(target_name.to_string(), updated_instance.clone());
        updated_instance
    }
}
