//! `is Set`/`is Bag`/`is Mix` (and their mutable `*Hash`) subclass instance
//! delegation: an `Instance` whose class inherits one of the builtin QuantHash
//! types keys its data off the backing `__baggy_data__` attribute (seeded at
//! construction time by `Interpreter::seed_quanthash_storage`, see
//! `runtime/quanthash_subclass.rs`) and delegates the QuantHash protocol to it.
//!
//! This is the QuantHash twin of the `__mutsu_hash_storage` delegation in
//! `vm_hash_subclass_delegate.rs`, and works the same way: a plain `Bag`/`Set`/
//! `Mix` value already has full native method coverage, so the delegation
//! re-targets that existing dispatch at the storage value through a synthetic
//! env binding and writes the (possibly mutated) storage back into the
//! instance's attribute cell. No new slow-path mechanism.
//!
//! The read-only `builtins/methods_0arg` and `builtins/methods_narg`
//! delegations already forward *pure* `__baggy_data__` methods; what lives here
//! is the mutating half (`ASSIGN-KEY`, `DELETE-KEY`, `STORE`, `grab`, ...),
//! which needs the writeback those pure paths cannot do.

use super::*;

impl Interpreter {
    /// Methods delegated to `__baggy_data__`. Curated rather than "everything"
    /// so a method the class genuinely wants handled by ordinary `Instance`
    /// dispatch (`.new`, `.WHAT`, `does`, `isa`, a user-defined method —
    /// already excluded via `has_user_method` at the call site) is never
    /// intercepted here.
    fn is_baggy_storage_method(method: &str) -> bool {
        matches!(
            method,
            "AT-KEY"
                | "ASSIGN-KEY"
                | "BIND-KEY"
                | "DELETE-KEY"
                | "EXISTS-KEY"
                | "STORE"
                | "elems"
                | "total"
                | "keys"
                | "values"
                | "kv"
                | "pairs"
                | "antipairs"
                | "invert"
                | "list"
                | "List"
                | "Array"
                | "array"
                | "Seq"
                | "Slip"
                | "hash"
                | "Hash"
                | "Map"
                | "iterator"
                | "Bool"
                | "Numeric"
                | "Int"
                | "Str"
                | "Stringy"
                | "Set"
                | "SetHash"
                | "Bag"
                | "BagHash"
                | "Mix"
                | "MixHash"
                | "Setty"
                | "Baggy"
                | "Mixy"
                | "grab"
                | "grabpairs"
                | "pick"
                | "pickpairs"
                | "roll"
                | "sum"
                | "min"
                | "max"
                | "minmax"
                | "sort"
                | "map"
                | "grep"
                | "first"
                | "classify"
                | "categorize"
                | "ACCEPTS"
                // The internal element-lvalue protocol the native `AT-KEY`
                // base candidate is expressed in — see
                // `runtime::run::CONTAINER_ELEMENT_PROXY_SRC`. Not reachable
                // from user code by any ordinary name.
                | "__mutsu_container_at_key"
                | "__mutsu_container_assign_key"
        )
    }

    /// Shared precondition for both delegates: `target` is an `Instance` of a
    /// class that inherits a QuantHash base, carries the backing attribute, and
    /// does not define `method` itself.
    fn baggy_storage_target(&mut self, target: &Value, method: &str) -> Option<Value> {
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
        else {
            return None;
        };
        if !Self::is_baggy_storage_method(method) {
            return None;
        }
        if !attributes.contains_key("__baggy_data__") {
            return None;
        }
        let cn = class_name.resolve();
        if self.has_user_method_including_role(&cn, method) {
            return None;
        }
        attributes.as_map().get("__baggy_data__").cloned()
    }

    /// QuantHash-subclass instance delegation (mut path): delegate a
    /// QuantHash-protocol method call to the backing `__baggy_data__` attribute
    /// and write the mutated storage back into the instance. Returns `None` to
    /// fall through to the rest of dispatch.
    pub(super) fn try_baggy_storage_delegate_mut(
        &mut self,
        target_name: &str,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let storage = self.baggy_storage_target(target, method)?;
        let (class_name, attributes, inst_id) = match target.view() {
            ValueView::Instance {
                class_name,
                attributes,
                id,
            } => (class_name, attributes, id),
            _ => return None,
        };
        // The mutating half of the Associative protocol has no native
        // counterpart to delegate to: a named `%b<k> = v` on a plain QuantHash
        // is handled inline by the element-assign opcode, not by an
        // `ASSIGN-KEY` method. Apply it here through the shared pure helper
        // (`quanthash_with_weight`) so both routes agree on the weight
        // coercion and the zero-removes rule.
        //
        // The write goes IN PLACE through the shared attribute cell rather
        // than into a rebuilt instance: every holder of this object — the
        // tied variable, a closure that captured it, the `Proxy` the native
        // `AT-KEY` base hands out — shares one `Gc<InstanceAttrs>`, and only
        // an in-place write is visible to all of them.
        if matches!(
            method,
            "ASSIGN-KEY" | "DELETE-KEY" | "__mutsu_container_assign_key"
        ) && let Some(key_arg) = args.first()
        {
            let (key, elem) = crate::runtime::utils::quanthash_elem_entry(key_arg);
            let (assigned, result) = if method == "DELETE-KEY" {
                // `DELETE-KEY` answers the weight it removed.
                let old = match self.try_native_method(
                    &storage,
                    crate::symbol::Symbol::intern("AT-KEY"),
                    std::slice::from_ref(key_arg),
                ) {
                    Some(Ok(v)) => v,
                    Some(Err(e)) => return Some(Err(e)),
                    None => Value::NIL,
                };
                (Value::int(0), old)
            } else {
                let v = args.get(1).cloned().unwrap_or(Value::NIL);
                (v.clone(), v)
            };
            let mut outcome: Result<Value, RuntimeError> = Ok(result);
            let applied = attributes.with_attr_mut("__baggy_data__", |slot| {
                match Self::quanthash_with_weight(slot, key, Some(&elem), &assigned) {
                    Ok(Some(updated)) => *slot = updated,
                    // An immutable `Set`/`Bag`/`Mix` base: the same
                    // X::Assignment::RO a plain one raises.
                    Ok(None) => {
                        outcome = Err(RuntimeError::assignment_ro_typename(
                            crate::runtime::value_type_name(slot),
                            &crate::runtime::utils::gist_value(slot),
                        ))
                    }
                    Err(e) => outcome = Err(e),
                }
            });
            applied?;
            return Some(outcome);
        }
        // The read half of that internal protocol: the raw native weight,
        // never routed back through a user `AT-KEY` override.
        if method == "__mutsu_container_at_key" {
            let key_arg = args.first().cloned().unwrap_or(Value::NIL);
            return self.try_native_method(
                &storage,
                crate::symbol::Symbol::intern("AT-KEY"),
                &[key_arg],
            );
        }
        // `STORE` re-initializes the container wholesale, in place for the same
        // shared-identity reason as the element writes above.
        if method == "STORE" {
            let positional: Vec<Value> = args
                .iter()
                .filter(|a| !matches!(a.view(), ValueView::Pair(k, _) if k == "INITIALIZE"))
                .cloned()
                .collect();
            let mut outcome: Result<Value, RuntimeError> = Ok(Value::NIL);
            let applied = attributes.with_attr_mut("__baggy_data__", |slot| {
                match crate::runtime::quanthash_store::quanthash_store(slot, &positional) {
                    Some(stored) => *slot = stored,
                    None => {
                        outcome = Err(RuntimeError::assignment_ro_typename(
                            crate::runtime::value_type_name(slot),
                            &crate::runtime::utils::gist_value(slot),
                        ))
                    }
                }
            });
            applied?;
            if let Err(e) = outcome {
                return Some(Err(e));
            }
            // `STORE` answers the invocant: the tied-variable declaration path
            // binds the returned value to the variable.
            return Some(Ok(target.clone()));
        }
        // Seed a synthetic binding so the native mutating fast paths (which
        // write the updated container back into `self.env` by NAME) have
        // somewhere to write.
        self.env_mut()
            .insert("__mutsu_baggy_tmp".to_string(), storage.clone());
        let dispatched = self.call_method_mut_with_values(
            "__mutsu_baggy_tmp",
            storage.clone(),
            method,
            args.to_vec(),
        );
        let dispatched = match dispatched {
            Ok(v) => Ok(v),
            Err(_) => self.vm_call_method_with_values(storage.clone(), method, args.to_vec()),
        };
        let result = match dispatched {
            Ok(v) => v,
            Err(e) => {
                self.env_mut().remove("__mutsu_baggy_tmp");
                return Some(Err(e));
            }
        };
        let updated_storage = self
            .env()
            .get("__mutsu_baggy_tmp")
            .cloned()
            .unwrap_or_else(|| storage.clone());
        self.env_mut().remove("__mutsu_baggy_tmp");
        let updated_instance = self.write_back_baggy_storage_instance(
            target_name,
            &class_name,
            &attributes,
            inst_id,
            updated_storage,
        );
        let _ = updated_instance;
        Some(Ok(result))
    }

    /// Non-mut (read-only) twin of [`Self::try_baggy_storage_delegate_mut`],
    /// for the callers that have no named receiver to write a mutation back
    /// through.
    pub(super) fn try_baggy_storage_delegate(
        &mut self,
        target: &Value,
        method_sym: crate::symbol::Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let method = method_sym.as_str();
        let storage = self.baggy_storage_target(target, method)?;
        // The mutating members have no plain-`Value` native counterpart to
        // read from — route them through the mut delegate with a synthetic
        // (unread) target name, exactly like the Hash analog does for `STORE`.
        if matches!(
            method,
            "STORE"
                | "ASSIGN-KEY"
                | "BIND-KEY"
                | "DELETE-KEY"
                | "grab"
                | "grabpairs"
                | "__mutsu_container_at_key"
                | "__mutsu_container_assign_key"
        ) {
            return self.try_baggy_storage_delegate_mut(
                "__mutsu_baggy_store_tmp",
                target,
                method,
                args,
            );
        }
        self.try_native_method(&storage, method_sym, args)
    }

    /// Rebuild a QuantHash-backed instance with its `__baggy_data__` attribute
    /// replaced by `storage` and write it back into `target_name`. Mirrors
    /// `write_back_hash_storage_instance`.
    fn write_back_baggy_storage_instance(
        &mut self,
        target_name: &str,
        class_name: &crate::symbol::Symbol,
        attributes: &crate::gc::Gc<crate::value::InstanceAttrs>,
        inst_id: u64,
        storage: Value,
    ) -> Value {
        let new_attrs = crate::value::InstanceAttrs::clone(attributes);
        new_attrs.insert("__baggy_data__".to_string(), storage);
        let updated_instance = Value::instance_parts(
            *class_name,
            crate::gc::Gc::new(crate::value::InstanceAttrs::new(
                *class_name,
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
