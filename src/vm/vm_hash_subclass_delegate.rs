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
//! native coverage of its own), a plain `Value::Hash` already has full native
//! method coverage (AT-KEY/ASSIGN-KEY/BIND-KEY/DELETE-KEY/EXISTS-KEY/keys/
//! values/kv/pairs/push/.../raku/gist/...) through the SAME dispatch a named
//! `%h` variable uses. So this delegation just re-targets that existing
//! dispatch at the storage value through a synthetic env binding (exactly the
//! fallback branch the Array block itself uses for its own non-fast-path
//! methods), then writes the (possibly mutated) storage back into the
//! instance's attribute cell — no new slow-path mechanism, just reuse of the
//! existing native Hash dispatch under a different receiver.

use super::*;

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
                | "perl"
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
        if self.has_user_method(&cn, method) {
            return None;
        }
        if !attributes.contains_key("__mutsu_hash_storage") {
            return None;
        }
        if !self
            .mro_readonly(&cn)
            .iter()
            .any(|n| Self::is_associative_base(n))
        {
            return None;
        }
        let storage = attributes
            .as_map()
            .get("__mutsu_hash_storage")
            .cloned()
            .unwrap_or_else(|| Value::hash(std::collections::HashMap::new()));
        // `%h = pairs` / `my %h is Bar = pairs` (the tied-variable declaration
        // and reassignment paths — `vm_var_trait_ops.rs`/`vm_var_assign_local.rs`
        // — both dispatch a `STORE(list, :INITIALIZE)` call): wholesale-replace
        // the backing storage from the flattened value list, mirroring how a
        // plain `%h = pairs` assignment repopulates a native Hash. Handled here
        // directly rather than by re-targeting the native dispatch (a plain
        // `Value::Hash` has no native `STORE` method of its own to delegate
        // to — see the module doc comment).
        if method == "STORE" {
            let items = args.first().map(crate::runtime::utils::value_to_list);
            let mut map = std::collections::HashMap::new();
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
            let updated_instance = self.write_back_hash_storage_instance(
                target_name,
                &inst_class,
                &attributes,
                inst_id,
                new_storage,
            );
            return Some(Ok(updated_instance));
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
        if self.has_user_method(&cn, method) {
            return None;
        }
        if !attributes.contains_key("__mutsu_hash_storage") {
            return None;
        }
        if !self
            .mro_readonly(&cn)
            .iter()
            .any(|n| Self::is_associative_base(n))
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
            .get("__mutsu_hash_storage")
            .cloned()
            .unwrap_or_else(|| Value::hash(std::collections::HashMap::new()));
        self.try_native_method(&storage, method_sym, args)
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
        let new_attrs = crate::value::InstanceAttrs::clone(attributes);
        new_attrs.insert("__mutsu_hash_storage".to_string(), storage);
        let updated_instance = Value::instance_parts(
            *inst_class,
            crate::gc::Gc::new(crate::value::InstanceAttrs::new(
                *inst_class,
                new_attrs.to_map(),
                inst_id,
                true,
            )),
            inst_id,
        );
        self.env_mut()
            .insert(target_name.to_string(), updated_instance.clone());
        updated_instance
    }
}
