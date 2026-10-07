use super::*;
use crate::meta_ns::MetaNs;
use crate::symbol::Symbol;
use crate::value::ValueView;
use crate::value::value_buf::{
    buf_elem_width, buf_raw_bytes_in, buf_raw_bytes_or_empty, set_buf_raw_bytes,
};
impl Interpreter {
    pub(crate) fn call_method_mut_with_values(
        &mut self,
        target_var: &str,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // A user `IO::Handle` subclass overriding `WRITE`/`READ`/`EOF` routes
        // its high-level text methods through those overrides. The read-side
        // methods (`.read`, `.eof`, `.getc`, `.get`, ...) are mut-path methods
        // and arrive HERE, not at the two dispatch entries that already carried
        // the hook -- so they fell through to the native `IO::Handle` arm and
        // died with "Expected IO::Handle" for want of a real file descriptor,
        // while `.print` on the same object worked. See
        // `try_user_io_handle_method`.
        if let Some(result) = self.try_user_io_handle_method(&target, method, &args) {
            return result;
        }
        // Augmented native-type dispatch (mut-path twin of the non-mut guard in
        // `call_method_with_values`): see `native_lever_a_user_override`'s doc
        // comment. A native-typed receiver never carries an attribute cell, so
        // there is nothing for an augmented method to write back through — a
        // plain value dispatch (like the non-mut path) is the correct shape.
        if !matches!(
            target.view(),
            ValueView::Instance { .. } | ValueView::Package(_)
        ) && self.native_lever_a_user_override(&target, method)
        {
            return self.call_method_with_values(target, method, args);
        }
        // ADR-0070 at the mutable dispatch entry -- the fourth place the builtin
        // layer is entered. The native array/hash mutator arms below run in
        // FRONT of the arity cascade and read `args` positionally, so a named
        // argument none of them accepts was appended as an ELEMENT
        // (`@a.push(:zzz)` stored the `Pair` where raku ignores it and leaves
        // the array alone) or counted as a positional (`@a.pop(:zzz)` died
        // "Too many positionals passed" where raku pops). Restricted to a
        // native container receiver: an `Instance`/`Package` may be a user class
        // whose own `push` genuinely declares a named parameter, and a `Mixin`
        // may carry a role method, so neither is touched here.
        let args = if matches!(target.view(), ValueView::Array(..) | ValueView::Hash(_)) {
            crate::builtins::strip_undeclared_nameds(method, &args).unwrap_or(args)
        } else {
            args
        };
        // Track B/Track C: an aggregate that lives in a shared `ContainerRef`
        // cell (a `state @a`/`state %h` under an active thread context — see
        // `exec_state_var_init_op`). Dispatch on the cell's CONTENT, then fold
        // the mutated aggregate back INTO the cell and re-point the env at the
        // cell, so every holder (other closures, other threads, the state
        // store) observes the mutation and the next op keeps cell semantics.
        // Without this, `@a.push` on a cell-held state array mis-dispatched
        // ("No such method 'push' for invocant of type 'Array'") whenever the
        // cell was seeded non-empty (pre-existing on the thread-spawn
        // migration path; also every second call once the state write-through
        // keeps the cell current).
        if let ValueView::ContainerRef(cell) = target.view() {
            let inner = cell.lock().unwrap_or_else(|e| e.into_inner()).clone();
            if matches!(inner.view(), ValueView::Array(..) | ValueView::Hash(..)) {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethodmutwithvalues",
                    "container-ref-cell",
                );
                let cell = cell.clone();
                let result = self.call_method_mut_with_values(target_var, inner, method, args)?;
                if let Some(updated) = self.env.get(target_var).cloned()
                    && !updated.is_container_ref()
                    && matches!(updated.view(), ValueView::Array(..) | ValueView::Hash(..))
                {
                    *cell.lock().unwrap_or_else(|e| e.into_inner()) = updated;
                    self.env
                        .insert(target_var.to_string(), Value::container_ref(cell));
                }
                return Ok(result);
            }
        }
        let readonly_key = crate::runtime::sigilless_readonly_key(target_var);
        let alias_key = crate::runtime::sigilless_alias_key(target_var);
        let has_sigilless_meta =
            self.env.contains_key_sym(readonly_key) || self.env.contains_key_sym(alias_key);
        let scalar_like_target = target_var.starts_with('$')
            || (!target_var.starts_with('@')
                && !target_var.starts_with('%')
                && !target_var.starts_with('&')
                && !has_sigilless_meta);
        if scalar_like_target
            && args.is_empty()
            && matches!(method, "postfix:<++>" | "postfix:<-->")
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmutwithvalues",
                "incdec",
            );
            self.check_readonly_for_increment(target_var)?;
            let current = self.env.get(target_var).cloned().unwrap_or(target);
            let current = Self::normalize_incdec_source_for_mut(current);
            let updated = if method == "postfix:<++>" {
                Self::increment_mut_target_value(&current)
            } else {
                Self::decrement_mut_target_value(&current)
            };
            self.env.insert(target_var.to_string(), updated);
            return Ok(current);
        }
        // .keyof on Mix/Set/Bag variables: check type constraint for parameterized type
        if method == "keyof"
            && args.is_empty()
            && matches!(
                target.view(),
                ValueView::Mix(_, _) | ValueView::Set(_, _) | ValueView::Bag(_, _)
            )
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmutwithvalues",
                "keyof",
            );
            if let Some(constraint) = self.var_type_constraint(target_var)
                && let Some(bracket_pos) = constraint.find('[')
            {
                let param = &constraint[bracket_pos + 1..constraint.len() - 1];
                return Ok(Value::package(Symbol::intern(param)));
            }
            return Ok(Value::package(Symbol::intern("Mu")));
        }
        if method == "VAR" && args.is_empty() {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmutwithvalues",
                "var-reflect",
            );
            // Proxy (including subclasses): .VAR returns the proxy wrapped as a
            // ProxyObject so that subsequent method calls don't auto-FETCH.
            if matches!(target.view(), ValueView::Proxy { .. }) {
                return Ok(Value::proxy_var_object(target, target_var.to_string()));
            }
            if matches!(
                target.view(),
                ValueView::Instance { attributes, .. }
                    if attributes
                        .as_map()
                        .get("__mutsu_var_target")
                        .is_some_and(|v| matches!(v.view(), ValueView::Str(_)))
            ) {
                return Ok(target);
            }
            // A tied `@`/`%` variable (`my @a is DNA`, `my %h is Tk`) IS its own
            // container: raku answers `@a.VAR.^name` with the tie's class, not
            // `Array`/`Hash`. Without this the generic container-object
            // construction below reported the base container type and erased the
            // tie from every `.VAR`-based reflection.
            if (target_var.starts_with('@') || target_var.starts_with('%'))
                && Self::tied_instance_type_name(&target).is_some()
                && self.instance_is_tied(&target)
            {
                return Ok(target);
            }
            // A `@`/`%` parameter's container descriptor carries its BINDING
            // source name, not the param's syntactic name: "element" for the
            // fresh anonymous container an unsupplied param binds (tagged in
            // its own data by `missing_optional_param_value`, so it survives
            // every bind path), or the caller's variable recorded by the slow
            // binder as `__mutsu_var_source_name::` env metadata. Text::CSV's
            // `method CSV` gates on `@kh.VAR.name ne "element"`.
            let container_source_name = (target_var.starts_with('@')
                || target_var.starts_with('%'))
            .then(|| {
                target
                    .with_deref(|v| match v.view() {
                        ValueView::Array(data, _) => {
                            data.descriptor_name.as_ref().map(|s| s.to_string())
                        }
                        ValueView::Hash(data) => {
                            data.descriptor_name.as_ref().map(|s| s.to_string())
                        }
                        _ => None,
                    })
                    .or_else(|| {
                        self.env
                            .get(MetaNs::VarSourceName.str_key_for_str(target_var))
                            .map(Value::to_string_value)
                    })
            })
            .flatten();
            // A `$` name bound directly to an immutable value has no container
            // at all in Raku -- `my $b := 1` / `my constant $PI = 3.14` / a
            // topic aliased to a literal all report `.VAR` as the *value*
            // (`Int`, `Rat`, ...), not `Scalar`. That is exactly the
            // `ReadonlyKind::Immutable`/`ImmutableValue` distinction recorded
            // when the name was marked readonly; a readonly *binding* that does
            // own a container (`ReadonlyKind::Alias`: a non-`is rw` parameter, a
            // `for @a -> $v` alias) still reports `Scalar`, as Raku does.
            //
            // Probed BEFORE the `var_meta_value` cache: "this name has no
            // container right now" is a live property of the current binding,
            // and the cache is keyed by name alone. `for @a { .VAR }` followed
            // by `for 1,2 { .VAR }` shares the key `_`, so a cached `Scalar`
            // meta from the first loop answered the second one.
            if self.scalar_name_has_no_container(target_var) {
                return Ok(target);
            }
            // A sigil-less constant — a term, reached here by its spelling when
            // the compiler could not see it (a `require`d import) — has no
            // container either (#9962).
            if !target_var.starts_with(['$', '@', '%', '&'])
                && !self.env.contains_key(target_var)
                && self.term_value(target_var).is_some()
            {
                return Ok(target);
            }
            // Also a live property of the CURRENT binding, so probed before the
            // name-keyed cache as well: `sub h(\p) { p.VAR }; h($x); h(1)` must
            // answer `Int` for the second call, not the first call's cached
            // `Scalar` (#9346, `nqp::iscont(p)`).
            let readonly_key = crate::runtime::sigilless_readonly_key(target_var);
            let alias_key = crate::runtime::sigilless_alias_key(target_var);
            let has_sigilless_meta =
                self.env.contains_key_sym(readonly_key) || self.env.contains_key_sym(alias_key);
            if has_sigilless_meta {
                // A sigilless raw parameter aliases an aggregate caller
                // directly (`sub f(\x) { x.VAR } ; f(@a)`).  Its local value
                // is still the Array/List/Hash itself, but the parameter name
                // has no sigil, so the generic reflection path below would
                // incorrectly manufacture a Scalar descriptor.  Preserve the
                // aggregate's own container identity, just as `@a.VAR` does.
                if !target_var.starts_with(['$', '@', '%', '&'])
                    && let Some(source) = self.env.get_sym(alias_key).and_then(|value| match value
                        .view()
                    {
                        ValueView::Str(source) => Some(source.to_string()),
                        _ => None,
                    })
                    && ((source.starts_with('@') && matches!(target.view(), ValueView::Array(..)))
                        || (source.starts_with('%')
                            && matches!(target.view(), ValueView::Hash(..))))
                {
                    return Ok(target);
                }
                let readonly = self
                    .env
                    .get_sym(readonly_key)
                    .is_some_and(|v| matches!(v.view(), ValueView::Bool(true)));
                let itemized_array =
                    matches!(target.view(), ValueView::Array(_, kind) if kind.is_real_array());
                if readonly && !itemized_array {
                    return Ok(target);
                }
            }
            // A `$` name whose container is a shared cell that knows the
            // variable it was declared as -- an `is rw` / `is raw` / `\x`
            // parameter aliasing the caller's `$a` -- reports that name, as
            // rakudo's container descriptor does (#11196).
            let cell_name = (!target_var.starts_with(['@', '%', '&']))
                .then(|| {
                    // The binder's record of the caller's variable, made in
                    // this very frame (a dynamic `f(my $*OUT)` gets no shared
                    // cell), first; then the descriptor of the shared cell the
                    // name holds.
                    self.env
                        .get(MetaNs::VarSourceName.str_key_for_str(target_var))
                        .map(Value::to_string_value)
                        .or_else(|| {
                            self.env
                                .get(target_var)
                                .and_then(|v| match v.view() {
                                    ValueView::ContainerRef(cell) => cell.descriptor_name(),
                                    _ => None,
                                })
                                .map(|s| s.resolve())
                        })
                })
                .flatten();
            let display_name = if let Some(src) = container_source_name.clone().or(cell_name) {
                src
            } else if target_var.starts_with('$')
                || target_var.starts_with('@')
                || target_var.starts_with('%')
                || target_var.starts_with('&')
            {
                target_var.to_string()
            } else {
                format!("${}", target_var)
            };
            if let Some(existing) = self.var_meta_value(target_var) {
                // The cached meta instance goes stale when the SAME param name
                // is re-bound differently on a later call of the sub (call 1
                // unsupplied -> "element", call 2 supplied -> the param /
                // caller name, or a `$` param aliasing a different caller
                // variable, #11196): only reuse it when its recorded name
                // matches the current binding's descriptor name.
                let expected = display_name.clone();
                let cached_name = match existing.view() {
                    ValueView::Instance { attributes, .. } => attributes
                        .as_map()
                        .get("name")
                        .map(Value::to_string_value)
                        .unwrap_or_default(),
                    _ => expected.clone(),
                };
                if cached_name == expected {
                    // ADR-0064: the descriptor delegates every non-container
                    // method to the value the container holds, so refresh that
                    // snapshot before handing the cached instance back --
                    // `my $x = 1; $x.VAR.raku; $x = 2; $x.VAR.raku` must read
                    // 2. Written in place through the shared attribute cell so
                    // the instance keeps its identity (ADR-0057) and any role
                    // mixed into it by `trait_mod:<does>`.
                    if let ValueView::Instance { attributes, .. } = existing.view() {
                        attributes.insert(
                            "__mutsu_var_value",
                            self.var_meta_contained_snapshot(target_var, &target),
                        );
                    }
                    return Ok(existing);
                }
            }
            // A scalar `:=`-bound to a container (`my $r := @a` / `:= %h` /
            // `:= (1,2,3)`) has no Scalar container of its own — the binding
            // aliases the container directly — so `.VAR` returns the bound value
            // itself and `.VAR.^name` reflects the container type (List/Array/
            // Hash/...), not Scalar. The `__mutsu_bound_decont` marker records
            // such binds.
            if !target_var.starts_with('@')
                && !target_var.starts_with('%')
                && !target_var.starts_with('&')
            {
                let decont_key = MetaNs::BoundDecont.owned_key_for_str(target_var);
                if self
                    .env
                    .get(&decont_key)
                    .is_some_and(|v| matches!(v.view(), ValueView::Bool(true)))
                {
                    return Ok(target);
                }
            }
            let class_name = if target_var.starts_with('@') {
                "Array"
            } else if target_var.starts_with('%') {
                "Hash"
            } else if target_var.starts_with('&') {
                "Sub"
            } else {
                "Scalar"
            };
            let mut attributes = HashMap::new();
            attributes.insert("name".to_string(), Value::str(display_name));
            attributes.insert(
                "__mutsu_var_target".to_string(),
                Value::str(target_var.to_string()),
            );
            attributes.insert(
                "dynamic".to_string(),
                Value::truth(self.is_var_dynamic(target_var)),
            );
            // Add .default: explicit `is default(...)` value, or type object
            // for typed variables, or (Any) for untyped. Prefer the value-carried
            // default (HashData/ArrayData) so it survives raw-parameter binding
            // and list construction, where the by-name `var_default` lookup
            // (the variable's original name) no longer resolves.
            let default_val = if let Some(def) = Self::value_carried_default(&target) {
                def
            } else if let Some(def) = self.var_default(target_var) {
                def.clone()
            } else if let Some(tc) = self.var_type_constraint(target_var) {
                Value::package(Symbol::intern(&tc))
            } else if matches!(target_var, "$/" | "$!" | "/" | "!") {
                // The match/error special vars default to Nil, not Any
                // (S02-types/nil.t: `$/.VAR.default === Nil`).
                Value::NIL
            } else {
                Value::package(crate::symbol::wk::any())
            };
            attributes.insert("default".to_string(), default_val);
            // ADR-0064: the value this container currently holds. The descriptor
            // is transparent for ordinary method dispatch, and this is what those
            // methods are asked about. Prefer the shared `ContainerRef` cell when
            // the variable has one (then every read through the descriptor is
            // live); otherwise snapshot the value the VM handed us, which is
            // authoritative at this instant even when the env half of the dual
            // store has not been synced from `locals` yet.
            attributes.insert(
                "__mutsu_var_value".to_string(),
                self.var_meta_contained_snapshot(target_var, &target),
            );
            // Add .of: type constraint of the variable (Mu for unconstrained)
            let of_val = if let Some(tc) = self.var_type_constraint(target_var) {
                Value::package(Symbol::intern(&tc))
            } else {
                Value::package(Symbol::intern("Mu"))
            };
            attributes.insert("of".to_string(), of_val);
            // ADR-0057: when `target_var` is CURRENTLY a shared `ContainerRef`
            // cell (the compiler forces this for a captured/free `.VAR`
            // target — see `register_container_ref_capture_if_free`'s call
            // site in `compile_expr_method_on_var`), derive the reflection
            // Instance's identity from the cell's own stable address instead
            // of the next value from the process-global monotonic counter.
            // `.WHICH` is purely `"{class_name}|{id}"` (see the `WHICH`
            // dispatch), so any two frames holding the SAME cell — the
            // declaring frame and every closure/named-sub/method that
            // captured it — independently derive the SAME id and therefore
            // an identical `.WHICH`, with no cross-frame cache write-back of
            // any kind: identity falls out of already-shared structure
            // rather than a synthetic, frame-local cache entry. `target` at
            // this point is always the already-dereferenced value (`GetGlobal`
            // /`GetUpvalue` never leave a cell on the stack), so the raw env
            // entry is peeked directly. A plain (never-captured-by-`.VAR`)
            // variable never gets boxed and keeps the existing same-frame
            // `var_meta_value` cache identity, unchanged.
            let cell_id = self.env.get(target_var).and_then(|v| match v.view() {
                ValueView::ContainerRef(cell) => Some(crate::gc::Gc::as_ptr(&cell) as usize as u64),
                _ => None,
            });
            let meta = match cell_id {
                Some(id) => {
                    Value::make_instance_with_id(Symbol::intern(class_name), attributes, id)
                }
                None => Value::make_instance(Symbol::intern(class_name), attributes),
            };
            self.set_var_meta_value(target_var, meta.clone());
            return Ok(meta);
        }
        // .of returns the element type constraint of a container
        if method == "of"
            && args.is_empty()
            && (target_var.starts_with('@') || target_var.starts_with('%'))
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept("callmethodmutwithvalues", "of");
            // An `@`/`%` param bound to a parametric TYPE OBJECT
            // (`sub g(@x) { @x.of }` called with `Positional[Dog]` — the
            // JSON::Unmarshal attribute-type shape) reads the element type
            // from the parametric name itself.
            if let ValueView::Package(name) = target.view() {
                let n = name.resolve();
                if let Some(inner) = n
                    .split_once('[')
                    .and_then(|(_, rest)| rest.strip_suffix(']'))
                {
                    // `ValueType{KeyType}` object-hash form: `.of` is the value
                    // type only (the key type may contain commas, so split the
                    // braces before any comma handling).
                    let (value_type, key) =
                        crate::runtime::types::split_object_hash_constraint(inner);
                    let of_type = if key.is_some() { value_type } else { inner };
                    return Ok(Value::package(Symbol::intern(of_type)));
                }
            }
            // `%`/`@` variables route their method calls through this mutable
            // dispatch path. A statically composed container role keeps its
            // type parameter on the class, not in the backing Hash/Array, so
            // consult that metadata before the generic variable constraint.
            if let ValueView::Instance { class_name, .. } = target.view()
                && let Some(value_type) =
                    self.composed_container_role_value_type(&class_name.resolve())
            {
                return Ok(Value::package(Symbol::intern(&value_type)));
            }
            if let Some(value_type) = self.mixin_container_role_value_type(&target) {
                return Ok(value_type);
            }
            // Embedded metadata first: it travels with the value, so it stays
            // correct when a recursive call clobbers the name-keyed constraint
            // store (`my @ret := Array[T].new` re-bound in an inner frame).
            let type_name = self
                .container_type_metadata(&target)
                .map(|info| info.value_type)
                .filter(|t| !t.is_empty())
                .or_else(|| self.var_type_constraint(target_var))
                .unwrap_or_else(|| "Mu".to_string());
            return Ok(Value::package(Symbol::intern(&type_name)));
        }

        // Collation.set — mutates the Collation instance in the variable
        if method == "set"
            && matches!(target.view(), ValueView::Instance { class_name, .. } if class_name == "Collation")
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmutwithvalues",
                "collation-set",
            );
            let result = self.dispatch_collation_method(target, method, &args)?;
            // Update the variable in the environment to reflect the mutation
            self.env.insert(target_var.to_string(), result.clone());
            return Ok(result);
        }

        // A receiver-mutating built-in method answered from its row
        // (ADR-11276 §9.23): `BagHash.add`/`remove` and the QuantHash mutators
        // (`SetHash.set`/`unset`, `.grab`, `.grabpairs`) write the shared node
        // in place, so there is no variable to write back: every holder
        // already sees the change.
        {
            let mut place = crate::builtins::method_table::ReceiverPlace::var(target_var, &target);
            if let Some(result) = crate::builtins::method_table::invoke_mut(
                self,
                &mut place,
                Symbol::intern(method),
                &args,
            ) {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethodmutwithvalues",
                    "mut-row",
                );
                return result;
            }
        }

        if let ValueView::Instance {
            class_name,
            attributes,
            id,
        } = target.view()
            && crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve())
        {
            let bytes = buf_raw_bytes_or_empty(&attributes);

            if (method == "read-ubits" || method == "read-bits") && args.len() == 2 {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethodmutwithvalues",
                    "buf-read-bits",
                );
                let Some(from) = Self::value_to_non_negative_i64(&args[0]) else {
                    return Err(RuntimeError::new("read-ubits/read-bits expects Int offset"));
                };
                let Some(bits) = Self::value_to_non_negative_i64(&args[1]) else {
                    return Err(RuntimeError::new(
                        "read-ubits/read-bits expects Int bit count",
                    ));
                };
                return crate::builtins::buf_bits::read_bits(
                    &bytes,
                    from,
                    bits,
                    method == "read-bits",
                );
            }

            if (method == "write-ubits" || method == "write-bits") && args.len() == 3 {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethodmutwithvalues",
                    "buf-write-bits",
                );
                if class_name == "Blob" {
                    return Err(RuntimeError::new(
                        "Cannot modify immutable Blob with write-bits/write-ubits",
                    ));
                }
                let Some(from) = Self::value_to_non_negative_i64(&args[0]) else {
                    return Err(RuntimeError::new(
                        "write-ubits/write-bits expects Int offset",
                    ));
                };
                let Some(bits) = Self::value_to_non_negative_i64(&args[1]) else {
                    return Err(RuntimeError::new(
                        "write-ubits/write-bits expects Int bit count",
                    ));
                };
                let written = crate::builtins::buf_bits::write_bits(&bytes, from, bits, &args[2])?;
                let mut updated_attrs = attributes.to_map();
                set_buf_raw_bytes(&mut updated_attrs, class_name, written);
                return Ok(Value::write_back_sharing(
                    &attributes,
                    class_name,
                    updated_attrs,
                    id,
                ));
            }
        }

        // Buf/Blob write-num32 / write-num64 — mutate an existing instance.
        if crate::builtins::buf_write_num::write_num_size(method).is_some()
            && let ValueView::Instance {
                class_name,
                attributes,
                id,
            } = target.view()
            && crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve())
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmutwithvalues",
                "buf-write-num-mut",
            );
            let cn = class_name.resolve();
            if crate::runtime::utils::is_blob_like_class(&cn) && !cn.starts_with("utf") {
                return Err(RuntimeError::new(format!(
                    "Cannot modify immutable {} with {}",
                    cn, method
                )));
            }
            if args.len() < 2 || args.len() > 3 {
                return Err(RuntimeError::new(format!(
                    "{} expects 2 or 3 arguments, got {}",
                    method,
                    args.len()
                )));
            }
            let offset_val = &args[0];
            let value_val = &args[1];
            let endian_val = if args.len() == 3 {
                crate::builtins::buf_write_num::decode_endian(&args[2])
            } else {
                0
            };
            let offset_i64 = match offset_val.view() {
                ValueView::Int(i) => i,
                ValueView::Num(f) => f as i64,
                _ => 0,
            };
            let mut bytes = buf_raw_bytes_or_empty(&attributes);
            crate::builtins::buf_write_num::apply_write_num(
                &mut bytes,
                method,
                offset_i64,
                value_val,
                endian_val,
                buf_elem_width(&cn),
            )?;
            let mut updated_attrs = attributes.to_map();
            set_buf_raw_bytes(&mut updated_attrs, class_name, bytes);
            let updated = Value::write_back_sharing(&attributes, class_name, updated_attrs, id);
            self.env
                .insert_through(target_var.to_string(), updated.clone());
            return Ok(updated);
        }

        // Buf/Blob write-num on type object: returns a fresh buf.
        if crate::builtins::buf_write_num::write_num_size(method).is_some()
            && let ValueView::Package(name) = target.view()
        {
            let cn = name.resolve();
            if crate::runtime::utils::is_buf_or_blob_class(&cn) {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethodmutwithvalues",
                    "buf-write-num-fresh",
                );
                if args.len() < 2 || args.len() > 3 {
                    return Err(RuntimeError::new(format!(
                        "{} expects 2 or 3 arguments, got {}",
                        method,
                        args.len()
                    )));
                }
                let offset_i64 = match args[0].view() {
                    ValueView::Int(i) => i,
                    ValueView::Num(f) => f as i64,
                    _ => 0,
                };
                let endian_val = if args.len() == 3 {
                    crate::builtins::buf_write_num::decode_endian(&args[2])
                } else {
                    0
                };
                let mut bytes: Vec<u8> = Vec::new();
                crate::builtins::buf_write_num::apply_write_num(
                    &mut bytes,
                    method,
                    offset_i64,
                    &args[1],
                    endian_val,
                    buf_elem_width(&cn),
                )?;
                let normalized = crate::runtime::utils::normalize_buf_type_name(&cn);
                return Ok(crate::builtins::buf_write_num::make_buf_value_from_raw(
                    &normalized,
                    bytes,
                ));
            }
        }

        // Buf/Blob write-int / write-uint -- mutate an existing instance.
        if crate::builtins::buf_write_int::write_int_info(method).is_some()
            && let ValueView::Instance {
                class_name,
                attributes,
                id,
            } = target.view()
            && crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve())
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmutwithvalues",
                "buf-write-int-mut",
            );
            let cn = class_name.resolve();
            if crate::runtime::utils::is_blob_like_class(&cn) && !cn.starts_with("utf") {
                return Err(RuntimeError::new(format!(
                    "Cannot modify immutable {} with {}",
                    cn, method
                )));
            }
            if args.len() < 2 || args.len() > 3 {
                return Err(RuntimeError::new(format!(
                    "{} expects 2 or 3 arguments, got {}",
                    method,
                    args.len()
                )));
            }
            let offset_val = &args[0];
            let value_val = &args[1];
            let endian_val = if args.len() == 3 {
                crate::builtins::buf_write_num::decode_endian(&args[2])
            } else {
                0
            };
            let offset_i64 = match offset_val.view() {
                ValueView::Int(i) => i,
                ValueView::Num(f) => f as i64,
                _ => 0,
            };
            let mut bytes = buf_raw_bytes_or_empty(&attributes);
            crate::builtins::buf_write_int::apply_write_int(
                &mut bytes,
                method,
                offset_i64,
                value_val,
                endian_val,
                buf_elem_width(&cn),
            )?;
            let mut updated_attrs = attributes.to_map();
            set_buf_raw_bytes(&mut updated_attrs, class_name, bytes);
            let updated = Value::write_back_sharing(&attributes, class_name, updated_attrs, id);
            self.env
                .insert_through(target_var.to_string(), updated.clone());
            return Ok(updated);
        }

        // Buf/Blob write-int on type object: returns a fresh buf.
        if crate::builtins::buf_write_int::write_int_info(method).is_some()
            && let ValueView::Package(name) = target.view()
        {
            let cn = name.resolve();
            if crate::runtime::utils::is_buf_or_blob_class(&cn) {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethodmutwithvalues",
                    "buf-write-int-fresh",
                );
                if args.len() < 2 || args.len() > 3 {
                    return Err(RuntimeError::new(format!(
                        "{} expects 2 or 3 arguments, got {}",
                        method,
                        args.len()
                    )));
                }
                let offset_i64 = match args[0].view() {
                    ValueView::Int(i) => i,
                    ValueView::Num(f) => f as i64,
                    _ => 0,
                };
                let endian_val = if args.len() == 3 {
                    crate::builtins::buf_write_num::decode_endian(&args[2])
                } else {
                    0
                };
                let mut bytes: Vec<u8> = Vec::new();
                crate::builtins::buf_write_int::apply_write_int(
                    &mut bytes,
                    method,
                    offset_i64,
                    &args[1],
                    endian_val,
                    buf_elem_width(&cn),
                )?;
                let normalized = crate::runtime::utils::normalize_buf_type_name(&cn);
                return Ok(crate::builtins::buf_write_num::make_buf_value_from_raw(
                    &normalized,
                    bytes,
                ));
            }
        }

        // Buf/Blob mutating methods: append, push, prepend, unshift, reallocate, pop, shift, splice
        if matches!(
            method,
            "append" | "push" | "prepend" | "unshift" | "reallocate" | "pop" | "shift" | "splice"
        ) && Self::is_buf_like_value(&target)
        {
            if method == "reallocate" {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethodmutwithvalues",
                    "buf-reallocate",
                );
                return self.buf_reallocate(target_var, target, &args);
            }
            if method == "pop" || method == "shift" || method == "splice" {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethodmutwithvalues",
                    "buf-pop-shift-splice",
                );
                return self.buf_pop_shift_splice(target_var, target, method, args);
            }
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmutwithvalues",
                "buf-mutate-append",
            );
            return self.buf_mutate_method(target_var, target, method, args);
        }

        // `@a.squish` (and `@a .= squish`) on an `@` variable. Not one of the
        // receiver-mutating rows: it answers a new `Seq` and only writes the
        // variable back inside an lvalue assignment.
        if method == "squish"
            && target_var.starts_with('@')
            && matches!(target.view(), ValueView::Array(..))
            && !self.mixin_role_has_method(&target, method)
        {
            let key = target_var.to_string();
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmutwithvalues",
                "array-squish",
            );
            // Read THROUGH a shared `ContainerRef` cell: a `$scalar = @arr`
            // share, an rw/`\(...)` argument capture or a `:=` rebind used as a
            // sub argument leaves the env entry holding the cell, and squishing
            // the cell itself wrapped the whole array as one element
            // (`@a.squish.List` gave `([...],)`).
            let current = self
                .env
                .get(&key)
                .map(Value::deref_container)
                .unwrap_or_else(|| target.clone());
            let squished = self.dispatch_squish(current, &args)?;
            if self.in_lvalue_assignment {
                let squished_items = match squished.view() {
                    ValueView::Array(items, ..) => items.to_vec(),
                    ValueView::Seq(items) => items.to_vec(),
                    _ => vec![squished.clone()],
                };
                self.env.insert(key, Value::real_array(squished_items));
            }
            return Ok(squished);
        }

        // map with rw binding: mutations to $_ inside map should write back to the
        // source array elements (Raku semantics: $_ is rw-bound in map).
        // The rw-map fast path materializes the source (for `$_`-mutating blocks
        // like `@a.map({ $_++ })`). An infinite sequence/closure spec must stay
        // lazy — fall through to the lazy `map` pipeline in `call_method_with_values`
        // (L2b). Writeback is meaningless on an unbounded array anyway.
        if method == "map"
            && target_var.starts_with('@')
            // A live gather/pipe has no eager item snapshot to put into the
            // deferred map body. Let the ordinary map dispatcher preserve
            // its lazy source instead of turning it into an empty Seq.
            && !matches!(target.view(), ValueView::LazyList(ll) if ll.needs_vm_lazy_dispatch())
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmutwithvalues",
                "map-rw-writeback",
            );
            let items = crate::value::MapGrepItems::of(&target, || {
                if crate::runtime::utils::is_shaped_array(&target) {
                    crate::runtime::utils::shaped_array_leaves(&target)
                } else {
                    Self::value_to_list(&target)
                }
            });
            // With no elements there is neither a callback invocation nor an
            // rw writeback to defer. Avoid creating a MapGrep source merely to
            // reify it immediately as empty; this is the mutable-array route
            // used by attribute TWEAKs such as `@!resources.map(*.flat)`.
            if items.len() == 0 {
                return Ok(Value::seq(Vec::new()));
            }
            // ADR-0058 §9.4: this used to run the map loop RIGHT HERE, which
            // made `@a.map({ ... })` -- the commonest `.map` spelling there
            // is -- the one receiver step 2 never reached, because a `.map`
            // on a named array variable compiles to `OpCode::CallMethodMut`
            // and lands in this rw dispatch instead of `dispatch_map_method`.
            // It now defers exactly like every other `.map`: the callback
            // runs at first consumption, through `SeqSource::MapGrep`.
            //
            // The rw write-back is what made deferring this one awkward, and
            // it is carried by `rw_source`: the pull runs
            // `eval_map_over_items_rw` and publishes any mutation by writing
            // the source `ArrayData` IN PLACE, so it needs no frame and no
            // name (`publish_rw_map_writeback`). That matters because the
            // pull happens wherever the Seq is consumed -- rakudo writes back
            // at consumption too (`my @a=1,2,3; @a.map({$_++})` leaves
            // `[2 3 4]` only because a sunk statement consumes the Seq).
            return Ok(Value::seq_deferred(crate::value::SeqSource::MapGrep {
                items,
                pos: 0,
                func: args.first().cloned(),
                fatal: self.module.fatal_mode,
                mode: crate::value::MapGrepMode::MapRw(target.clone()),
                plan: Default::default(),
            }));
        }

        // SharedPromise/SharedChannel are internally mutable — delegate to immutable dispatch
        if matches!(target.view(), ValueView::Promise(_) | ValueView::Channel(_)) {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmutwithvalues",
                "promise-channel-delegate",
            );
            return self.call_method_with_values(target, method, args);
        }

        if let Some((class_name, attributes, target_id)) = match target.view() {
            ValueView::Instance {
                class_name,
                attributes,
                id,
            } => Some((class_name, attributes.clone(), id)),
            _ => None,
        } {
            // A metaobject method (`$metaobject.name($obj)`) on a HOW instance is a
            // ClassHOW method, NOT an rw-accessor write. The HOW instance stores a
            // `name` attribute, so without this the `args.len() == 1` rw-accessor
            // setter below would treat `$mo.name(1)` as `name = 1` and return the
            // argument. Route it to the ClassHOW dispatcher (which mirrors the
            // non-mut `dispatch_instance_and_fallback` classhow path).
            if Self::is_classhow_method(method)
                && (Self::is_metamodel_how(&class_name)
                    || self.is_metamodel_how_class(&class_name.resolve()))
            {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethodmutwithvalues",
                    "classhow",
                );
                let how_args = self.how_dispatch_args(&attributes, method, &args);
                return self.dispatch_classhow_method(method, how_args);
            }
            if crate::runtime::utils::is_buf_like_class(&class_name.resolve())
                && matches!(method, "write-ubits" | "write-bits")
                && args.len() == 3
            {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethodmutwithvalues",
                    "buf-bits-instance-fallback",
                );
                let from = super::to_int(&args[0]);
                let bits = super::to_int(&args[1]);
                if from < 0 || bits < 0 {
                    return Err(RuntimeError::new("bit offset/length must be non-negative"));
                }
                let mut updated = attributes.to_map();
                let bytes = buf_raw_bytes_in(&updated).unwrap_or_default();
                let bytes = crate::builtins::buf_bits::write_bits(&bytes, from, bits, &args[2])?;
                set_buf_raw_bytes(&mut updated, class_name, bytes);
                let updated_instance =
                    Value::write_back_sharing(&attributes, class_name, updated, target_id);
                self.env
                    .insert(target_var.to_string(), updated_instance.clone());
                return Ok(updated_instance);
            }

            if class_name == "Iterator" {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethodmutwithvalues",
                    "iterator-protocol",
                );
                // A `.map`/`.grep` stream commits through the shared cell.
                if let Some(result) = self.map_grep_stream_protocol_call(&attributes, method, &args)
                {
                    return result;
                }
                // A detached working copy of the attribute map; written back into
                // the instance's live shared cell at the end.
                let mut updated = attributes.to_map();
                let squish_source = updated.get("squish_source").cloned();
                if let Some(sv) = &squish_source
                    && let ValueView::Array(source, ..) = sv.view()
                {
                    let mut scan_index = match updated.get("squish_scan_index").map(Value::view) {
                        Some(ValueView::Int(i)) if i >= 0 => i as usize,
                        _ => 0,
                    };
                    let mut prev_key = updated
                        .get("squish_prev_key")
                        .cloned()
                        .unwrap_or(Value::NIL);
                    let mut initialized = matches!(
                        updated.get("squish_initialized").map(Value::view),
                        Some(ValueView::Bool(true))
                    );
                    let as_func = updated
                        .get("squish_as")
                        .cloned()
                        .filter(|v| !matches!(v.view(), ValueView::Nil));
                    let with_func = updated
                        .get("squish_with")
                        .cloned()
                        .filter(|v| !matches!(v.view(), ValueView::Nil));

                    let mut pull_one_squish = |this: &mut Self| -> Result<Value, RuntimeError> {
                        if !initialized {
                            let Some(first) = source.first().cloned() else {
                                return Ok(Value::iteration_end());
                            };
                            prev_key = if let Some(func) = as_func.clone() {
                                this.call_sub_value(func, vec![first.clone()], true)?
                            } else {
                                first.clone()
                            };
                            initialized = true;
                            scan_index = 1;
                            return Ok(first);
                        }

                        while scan_index < source.len() {
                            let item = source[scan_index].clone();
                            let key = if let Some(func) = as_func.clone() {
                                this.call_sub_value(func, vec![item.clone()], true)?
                            } else {
                                item.clone()
                            };

                            let duplicate = if let Some(func) = with_func.clone() {
                                this.call_sub_value(
                                    func,
                                    vec![prev_key.clone(), key.clone()],
                                    true,
                                )?
                                .truthy()
                            } else {
                                crate::runtime::values_identical(&prev_key, &key)
                            };
                            prev_key = key;
                            scan_index += 1;
                            if !duplicate {
                                return Ok(item);
                            }
                        }
                        Ok(Value::iteration_end())
                    };

                    let ret = match method {
                        "count-only" => self
                            .iterator_count_only_from_attrs(&updated)?
                            .unwrap_or_else(|| Value::int(0)),
                        "bool-only" => self
                            .iterator_bool_only_from_attrs(&updated)?
                            .unwrap_or(Value::FALSE),
                        "pull-one" => pull_one_squish(self)?,
                        "push-all" | "push-until-lazy" => {
                            let mut collected = Vec::new();
                            loop {
                                let next = pull_one_squish(self)?;
                                if next.is_iteration_end() {
                                    break;
                                }
                                collected.push(next);
                            }
                            self.iterator_append_to_array_arg(&args, &collected);
                            Value::iteration_end()
                        }
                        "skip-one" => {
                            let next = pull_one_squish(self)?;
                            // Iterator.skip-one returns 1 (Int) on a skip, 0 at end.
                            Value::int(i64::from(!next.is_iteration_end()))
                        }
                        "skip-at-least" => {
                            let want = args.first().map(super::to_int).unwrap_or(0).max(0) as usize;
                            let mut ok = true;
                            for _ in 0..want {
                                let next = pull_one_squish(self)?;
                                if next.is_iteration_end() {
                                    ok = false;
                                    break;
                                }
                            }
                            Value::int(i64::from(ok))
                        }
                        "skip-at-least-pull-one" => {
                            let want = args.first().map(super::to_int).unwrap_or(0).max(0) as usize;
                            for _ in 0..want {
                                let next = pull_one_squish(self)?;
                                if next.is_iteration_end() {
                                    updated.insert(
                                        "squish_scan_index".to_string(),
                                        Value::int(scan_index as i64),
                                    );
                                    updated.insert("squish_prev_key".to_string(), prev_key.clone());
                                    updated.insert(
                                        "squish_initialized".to_string(),
                                        Value::truth(initialized),
                                    );
                                    attributes.commit_attrs(updated.clone());
                                    return Ok(Value::iteration_end());
                                }
                            }
                            pull_one_squish(self)?
                        }
                        "push-exactly" | "push-at-least" => {
                            let want = args.get(1).map(super::to_int).unwrap_or(1).max(0) as usize;
                            let mut collected = Vec::new();
                            for _ in 0..want {
                                let next = pull_one_squish(self)?;
                                if next.is_iteration_end() {
                                    break;
                                }
                                collected.push(next);
                            }
                            self.iterator_append_to_array_arg(&args, &collected);
                            if collected.len() >= want {
                                Value::NIL
                            } else {
                                Value::iteration_end()
                            }
                        }
                        "sink-all" => {
                            loop {
                                let next = pull_one_squish(self)?;
                                if next.is_iteration_end() {
                                    break;
                                }
                            }
                            Value::iteration_end()
                        }
                        "can" => {
                            let method_name = args
                                .first()
                                .map(|v| v.to_string_value())
                                .unwrap_or_default();
                            let supported = matches!(
                                method_name.as_str(),
                                "pull-one"
                                    | "count-only"
                                    | "bool-only"
                                    | "push-exactly"
                                    | "push-at-least"
                                    | "push-all"
                                    | "push-until-lazy"
                                    | "sink-all"
                                    | "skip-one"
                                    | "skip-at-least"
                                    | "skip-at-least-pull-one"
                            );
                            if supported {
                                Value::array(vec![Value::str(method_name)])
                            } else {
                                Value::array(Vec::new())
                            }
                        }
                        _ => self.call_method_with_values(target, method, args)?,
                    };

                    updated.insert(
                        "squish_scan_index".to_string(),
                        Value::int(scan_index as i64),
                    );
                    updated.insert("squish_prev_key".to_string(), prev_key);
                    updated.insert("squish_initialized".to_string(), Value::truth(initialized));
                    let updated_instance =
                        Value::write_back_sharing(&attributes, class_name, updated, target_id);
                    self.env.insert(target_var.to_string(), updated_instance);
                    return Ok(ret);
                }

                let mut items = match updated.get("items").map(Value::view) {
                    Some(ValueView::Array(values, ..)) => values.to_vec(),
                    _ => Vec::new(),
                };
                let index = match updated.get("index").map(Value::view) {
                    Some(ValueView::Int(i)) if i >= 0 => i as usize,
                    _ => 0,
                };
                // A lazy source may not have produced the elements this call
                // needs yet; pull them and keep the grown prefix on the
                // instance, so the next call starts from it.
                let lazy_source = updated.get("lazy_source").cloned();
                if let Some(more) = self.iterator_topup_from_lazy_source(
                    lazy_source.as_ref(),
                    method,
                    index,
                    &args,
                    items.len(),
                )? {
                    items = more;
                    updated.insert("items".to_string(), Value::array(items.clone()));
                }
                let len = items.len();
                // A known logical count (set for `LHS xx N` lazy repeats) overrides
                // the materialized prefix length, so `.count-only` / `.bool-only`
                // on a stored iterator report the true (possibly infinite) count.
                let known_count = updated.get("known_count").cloned();

                // The index-advancing protocol family shares one stepping
                // implementation with the read-only (temporary receiver) path.
                if let Some(step) = super::iterator_protocol::step(method, &items, index, &args) {
                    if let Some(range) = step.append {
                        let vals = items[range].to_vec();
                        self.iterator_append_to_array_arg(&args, &vals);
                    }
                    updated.insert("index".to_string(), Value::int(step.new_index as i64));
                    self.env.insert(
                        target_var.to_string(),
                        Value::write_back_sharing(&attributes, class_name, updated, target_id),
                    );
                    return Ok(step.ret);
                }

                // A lazy source with no known count cannot predict its length
                // (see `iterator_is_unpredictive_lazy`).
                let unpredictive = known_count.is_none() && updated.contains_key("lazy_source");
                let ret = match method {
                    "count-only" | "bool-only" if unpredictive => {
                        return Err(crate::runtime::did_you_mean::method_not_found(
                            method, "Iterator",
                        ));
                    }
                    "count-only" => {
                        known_count.unwrap_or_else(|| Value::int(len.saturating_sub(index) as i64))
                    }
                    "bool-only" => match &known_count {
                        Some(c) => Value::truth(c.to_f64() > 0.0),
                        None => Value::truth(index < len),
                    },
                    "can" => {
                        let method_name = args
                            .first()
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        let supported = !(unpredictive
                            && matches!(method_name.as_str(), "count-only" | "bool-only"))
                            && matches!(
                                method_name.as_str(),
                                "pull-one"
                                    | "count-only"
                                    | "bool-only"
                                    | "push-exactly"
                                    | "push-at-least"
                                    | "push-all"
                                    | "push-until-lazy"
                                    | "sink-all"
                                    | "skip-one"
                                    | "skip-at-least"
                                    | "skip-at-least-pull-one"
                            );
                        if supported {
                            return Ok(Value::array(vec![Value::str(method_name)]));
                        } else {
                            return Ok(Value::array(Vec::new()));
                        }
                    }
                    _ => self.call_method_with_values(target, method, args)?,
                };

                updated.insert("index".to_string(), Value::int(index as i64));
                self.env.insert(
                    target_var.to_string(),
                    Value::write_back_sharing(&attributes, class_name, updated, target_id),
                );
                return Ok(ret);
            }

            // Handle delegation methods: forward the call to the delegate
            if let Some(method_def) = self.resolve_method(&class_name.resolve(), method, &args)
                && method_def.delegation.is_some()
            {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethodmutwithvalues",
                    "delegation",
                );
                // Clear skip_pseudo_method_native so the inner delegate dispatch
                // does not inherit the outer call's bypass flag (which was set
                // for the delegator's own method name).
                let saved_skip_pseudo = self.dispatch.skip_pseudo_method_native.take();
                let (attr_var_name, target_method) = method_def.delegation.as_ref().unwrap();
                let is_method_based = attr_var_name.starts_with('&');
                let attr_key = attr_var_name
                    .trim_start_matches('&')
                    .trim_start_matches('.')
                    .trim_start_matches('!');
                let delegate = if is_method_based {
                    let source_method = attr_var_name.trim_start_matches('&').to_string();
                    let invocant_val =
                        Value::instance_sharing_cell(&attributes, class_name, target_id);
                    self.call_method_with_values(invocant_val, &source_method, Vec::new())?
                } else {
                    attributes
                        .as_map()
                        .get(attr_key)
                        .cloned()
                        .unwrap_or(Value::NIL)
                };
                if delegate == Value::NIL {
                    return Err(RuntimeError::new(format!(
                        "No such method '{}' for invocant of type '{}'",
                        target_method,
                        class_name.resolve()
                    )));
                }
                // Determine sigil for temp var based on delegate type
                let sigil = match delegate.view() {
                    ValueView::Array(..) => "@",
                    ValueView::Hash(_) => "%",
                    _ => "$",
                };
                let temp_var = format!("{}__mutsu_delegation_tmp__", sigil);
                self.env.insert(temp_var.clone(), delegate.clone());
                // A delegated method whose target class declares a `proto method`
                // must run that proto body first (its `{*}` dispatches to the
                // matching multi). The mut dispatch below
                // (`call_method_mut_with_values`) skips the proto, so intercept it
                // here — mirroring the non-mut `forward_resolved_delegation` path,
                // which forwards through `call_method_with_values` (proto-aware).
                let result = if let Some(proto_result) =
                    self.try_proto_method_body(&delegate, target_method, &args)
                {
                    proto_result?
                } else {
                    self.call_method_mut_with_values(&temp_var, delegate, target_method, args)?
                };
                // Read back the potentially-updated delegate
                let updated_delegate = self.env.get(&temp_var).cloned().unwrap_or(Value::NIL);
                self.env.remove(&temp_var);
                if !is_method_based {
                    // Write the updated delegate back into the frontend's live cell.
                    let mut updated = attributes.to_map();
                    updated.insert(attr_key.to_string(), updated_delegate);
                    self.env.insert(
                        target_var.to_string(),
                        Value::write_back_sharing(&attributes, class_name, updated, target_id),
                    );
                }
                // Restore skip_pseudo for the outer caller.
                self.dispatch.skip_pseudo_method_native = saved_skip_pseudo;
                return Ok(result);
            }

            if args.len() == 1 && !self.is_native_method(&class_name.resolve(), method) {
                let class_attrs = self.collect_class_attributes(&class_name.resolve());
                let is_public_rw_accessor = if class_attrs.is_empty() {
                    attributes.contains_key(method)
                } else {
                    class_attrs.iter().any(|a| {
                        a.is_public && a.name == method && (a.is_rw || matches!(a.sigil, '@' | '%'))
                    })
                };
                if is_public_rw_accessor {
                    // User-defined rw method takes priority over simple accessor
                    let has_rw_method = self
                        .resolve_method(&class_name.resolve(), method, &[])
                        .is_some_and(|m| m.is_rw);
                    if !has_rw_method {
                        crate::vm::vm_stats::record_dispatch_entry_outcome(
                            "callmethodmutwithvalues",
                            "accessor",
                        );
                        let sigil = class_attrs
                            .iter()
                            .find(|a| a.is_public && a.name == method)
                            .map(|a| a.sigil)
                            .or_else(|| {
                                attributes.as_map().get(method).and_then(|value| {
                                    match value.view() {
                                        ValueView::Array(..) => Some('@'),
                                        ValueView::Hash(_) => Some('%'),
                                        _ => None,
                                    }
                                })
                            });
                        // A public rw accessor for an aggregate attribute is
                        // still an assignment to that attribute's container.
                        // Coerce list-shaped RHS values before replacing the
                        // existing contents; otherwise assigning a Seq (for
                        // example a `gather` result) leaves a bare Seq in an
                        // `@` attribute and later mutators cannot operate on
                        // it.
                        let assigned = match sigil {
                            Some(sigil @ ('@' | '%')) => {
                                Self::coerce_attr_value_by_sigil(args[0].clone(), sigil)
                            }
                            _ => args[0].clone(),
                        };
                        // An `@`/`%` attribute IS a container: `$obj.attr = (…)`
                        // assigns into the container the attribute already holds
                        // rather than rebinding the slot to a fresh one. Storing
                        // a new container here severed every other share of the
                        // old one — including the `Array`/`Hash` attribute
                        // containers `Mu.clone` deliberately shares between the
                        // original and the clone (raku: "Hash and Array attribute
                        // modifications in clone appear in original as well").
                        if let Some(existing) = attributes.as_map().get(method)
                            && existing.replace_container_contents(&assigned)
                        {
                            return Ok(existing.clone());
                        }
                        let mut updated = attributes.to_map();
                        updated.insert(method.to_string(), assigned.clone());
                        self.env.insert(
                            target_var.to_string(),
                            Value::write_back_sharing(&attributes, class_name, updated, target_id),
                        );
                        return Ok(assigned);
                    }
                    // Signal to assign_method_lvalue to handle via Proxy
                    crate::vm::vm_stats::record_dispatch_entry_intercept(
                        "callmethodmutwithvalues",
                        "rw-proxy-signal",
                    );
                    return Err(super::methods_signature_errors::make_multi_no_match_error(
                        method,
                    ));
                } else {
                    // Check if there's a user-defined method with is_rw
                    let has_rw_method = self
                        .resolve_method(&class_name.resolve(), method, &[])
                        .is_some_and(|m| m.is_rw);
                    if has_rw_method {
                        // Signal to assign_method_lvalue to handle via Proxy
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethodmutwithvalues",
                            "rw-proxy-signal",
                        );
                        return Err(super::methods_signature_errors::make_multi_no_match_error(
                            method,
                        ));
                    }
                    // Public accessor exists but is not rw — reject assignment
                    let is_public_accessor = if class_attrs.is_empty() {
                        false
                    } else {
                        class_attrs.iter().any(|a| a.is_public && a.name == method)
                    };
                    if is_public_accessor {
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethodmutwithvalues",
                            "rw-readonly-reject",
                        );
                        let current = attributes
                            .as_map()
                            .get(method)
                            .cloned()
                            .unwrap_or(Value::NIL);
                        return Err(RuntimeError::assignment_ro_typename(
                            super::utils::value_type_name(&current),
                            &current.to_string_value(),
                        ));
                    }
                }
            }

            if self.is_native_method(&class_name.resolve(), method) {
                crate::vm::vm_stats::record_dispatch_entry_outcome(
                    "callmethodmutwithvalues",
                    "native",
                );
                // Lazy `IO::CatHandle.lines` (no `$limit`/`:close`) / `.handles`
                // return a lazy list backed by the live cat (sharing its cell),
                // so mid-iteration `.chomp`/`.nl-in`/`.encoding` changes apply and
                // `.path`/on-switch track the current handle (Rakudo semantics).
                if class_name == "IO::CatHandle" {
                    let cat = Value::instance_sharing_cell(&attributes, class_name, target_id);
                    if let Some(lazy) = Self::cathandle_lazy_method(&cat, method, &args) {
                        return Ok(lazy);
                    }
                }
                // Try mutable dispatch first; if no mutable handler, fall back to immutable
                match self.call_native_instance_method_mut_in_place(
                    &attributes,
                    &class_name.resolve(),
                    method,
                    args.clone(),
                ) {
                    Ok(result) => {
                        // The delta commit already landed in the shared cell, so
                        // rebinding the name is all that is left (no second write
                        // of a whole map, which is what lost concurrent updates).
                        self.env.insert(
                            target_var.to_string(),
                            Value::instance_sharing_cell(&attributes, class_name, target_id),
                        );
                        return Ok(result);
                    }
                    Err(err) => {
                        if err.message.starts_with("No native mutable method") {
                            let cls = class_name.resolve();
                            if Self::native_method_blocks_on_other_thread(&cls, method) {
                                let snapshot = attributes.to_map();
                                return self
                                    .call_native_instance_method(&cls, &snapshot, method, args);
                            }
                            return self.call_native_instance_method(
                                &cls,
                                &attributes.as_map(),
                                method,
                                args,
                            );
                        }
                        return Err(err);
                    }
                }
            }
            let skip_pseudo = self
                .dispatch
                .skip_pseudo_method_native
                .as_ref()
                .is_some_and(|m| m == method);
            if skip_pseudo {
                self.dispatch.skip_pseudo_method_native = None;
            }
            // WHICH/WHY are excluded here: unlike the other six MOP
            // pseudo-methods, raku treats them as ordinary, overridable
            // methods in every call form (not just a compile-time-literal
            // quoted call), so a user-defined override must win below.
            let is_pseudo_method = matches!(
                method,
                "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHERE" | "VAR"
            );
            // A class-level public accessor shadows a same-named method from a
            // composed role. The VM opcode handles ordinary zero-argument
            // accessor reads before entering this runtime mut-dispatch lane,
            // but delegated `handles` calls arrive here directly. Keep the
            // same per-MRO precedence so `Card.street` reads its delegated
            // address attribute instead of Contact::Address.street's type
            // object method.
            if args.is_empty()
                && matches!(
                    self.resolve_user_method_or_accessor(&class_name.resolve(), method),
                    Some(crate::runtime::UserMethodOrAccessor::Accessor)
                )
            {
                return self.call_method_with_values(target, method, args);
            }
            if self.has_user_method(&class_name.resolve(), method)
                && (!is_pseudo_method || skip_pseudo)
            {
                crate::vm::vm_stats::record_dispatch_entry_outcome(
                    "callmethodmutwithvalues",
                    "user",
                );
                // ADR-0019 F6: VM-level direct-dispatch path first (see
                // `try_dispatch_compiled_method_direct`'s doc comment). `target`
                // and `attributes` share the same underlying cell (ADR-0013),
                // and `dispatch_compiled_method` already commits any reconciled
                // attribute map back through that cell, so a fresh
                // `attributes.to_map()` read after the call reflects the
                // post-mutation state without needing the carrier's own
                // returned snapshot (same reasoning as the mut-lvalue and
                // instance-ops families' own migrations).
                if let Some(result) =
                    self.try_dispatch_compiled_method_direct(&target, method, &args)
                {
                    let result = result?;
                    let updated_clone = attributes.to_map();
                    self.env.insert(
                        target_var.to_string(),
                        Value::instance_sharing_cell(&attributes, class_name, target_id),
                    );
                    if result.is_proxy_value()
                        && self.should_fetch_returned_proxy(&class_name.resolve(), method)
                        && matches!(result.view(), ValueView::Proxy { .. })
                    {
                        return self.proxy_fetch(
                            &result,
                            Some(target_var),
                            &class_name.resolve(),
                            &updated_clone,
                            target_id,
                        );
                    }
                    return Ok(result);
                }
                let (result, updated) = self.run_instance_method_at(
                    "mutdispatch",
                    &class_name.resolve(),
                    attributes.to_map(),
                    method,
                    args,
                    Some(target.clone()),
                )?;
                let updated_clone = updated.clone();
                attributes.commit_attrs(updated);
                self.env.insert(
                    target_var.to_string(),
                    Value::instance_sharing_cell(&attributes, class_name, target_id),
                );
                // Auto-FETCH if the method returned a Proxy
                if result.is_proxy_value()
                    && self.should_fetch_returned_proxy(&class_name.resolve(), method)
                    && matches!(result.view(), ValueView::Proxy { .. })
                {
                    return self.proxy_fetch(
                        &result,
                        Some(target_var),
                        &class_name.resolve(),
                        &updated_clone,
                        target_id,
                    );
                }
                return Ok(result);
            }
        }
        crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmutwithvalues", "user");
        self.call_method_with_values(target, method, args)
    }
}
