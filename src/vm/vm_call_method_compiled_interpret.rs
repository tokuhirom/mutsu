use super::*;

impl Interpreter {
    pub(super) fn try_compiled_method_or_interpret(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let saved_self = self.get_env_with_main_alias("self");
        let result = self.try_compiled_method_or_interpret_inner(target, method, args);
        match saved_self {
            Some(s) => self.set_env_with_main_alias("self", s),
            None => {
                self.env_mut().remove("self");
            }
        }
        result
    }

    fn try_compiled_method_or_interpret_inner(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // Native default construction: `Foo.new(...)` for a simple user-defined
        // class is pure data assembly (named args + attribute defaults), so the
        // Interpreter builds the instance directly instead of routing through the
        // interpreter's generic constructor dispatch (lever A: shrink method-call
        // interpreter fallback).
        if method == "new"
            && let Value::Package(class_name) = &target
            && let Some(result) = loan_env!(self, try_native_default_construct(*class_name, &args))
        {
            // An exception type with a user-defined `message` method needs that
            // method run once at construction and cached into the `message`
            // attribute, exactly as the interpreter's `dispatch_new` caller does
            // (`materialize_exception_message_in_result`). For built-in exceptions
            // and non-exceptions this is a no-op (no user `message` method).
            let cn = class_name.resolve();
            let result = self.materialize_exception_message_in_result(result);
            // Native construction of an attribute-only class is pure data assembly
            // (named args + defaults; writes nothing to the caller env, Slice 6.3).
            // BUT if the class has a `submethod BUILD`/`TWEAK`, the native path runs
            // that phase, whose body can mutate a captured-outer caller lexical
            // (`my $n; submethod TWEAK { $n++ }`) — so that construction is NOT
            // env-pure: leave the dispatch impure so the call site reconciles the
            // caller's slot (Slice F, `reconcile_locals_from_env_at_site`). A
            // user `message` method materialized above can likewise run user code,
            // so treat that as impure too.
            let runs_message =
                (cn == "Exception" || cn.starts_with("X::") || cn.starts_with("CX::"))
                    && self.has_user_method(&cn, "message");
            self.method_dispatch_pure = !self.mro_has_build_or_tweak(&cn) && !runs_message;
            return result;
        }
        // Native built-in construction: `Buf`/`Blob` (byte overlay), `utf8`/
        // `utf16` (code units), `Uni` (codepoints), `Version`/`Duration`/
        // `StrDistance`/`Stash`/empty-instance handles — pure data builds the Interpreter
        // performs directly instead of routing through `dispatch_new`.
        if method == "new"
            && let Value::Package(class_name) = &target
            && let Some(result) =
                crate::runtime::Interpreter::try_native_builtin_construct(*class_name, &args)
        {
            self.method_dispatch_pure = true;
            return result;
        }
        // Native QuantHash construction: `Set`/`SetHash`/`Bag`/`BagHash`/`Mix`/
        // `MixHash`.new(...) — pure element counting + optional parameterized
        // type-check / container-metadata tag, no env / registry / user code.
        // Built directly via the single `try_native_quanthash_construct` impl the
        // interpreter's `dispatch_new` also delegates to.
        if method == "new"
            && let Value::Package(class_name) = &target
            && let Some(result) =
                self.try_native_quanthash_construct_for_package(*class_name, &args)
        {
            self.method_dispatch_pure = true;
            return result;
        }
        // Native aggregate construction: `Array`/`List`/`Positional`/`array`/
        // `Hash`/`Map`.new(...) — shaped-dim parsing + parameterized type check +
        // container-metadata tag, no env / registry / user code. Built directly via
        // the single `try_native_array_construct` / `try_native_hash_construct`
        // impls the interpreter's `dispatch_new` also delegates to.
        if method == "new"
            && let Value::Package(class_name) = &target
            && let Some(result) =
                self.try_native_aggregate_construct_for_package(*class_name, &args)
        {
            self.method_dispatch_pure = true;
            return result;
        }
        // Native IO::Path family construction: `IO::Path`/`IO::Path::Unix`/`::Win32`/
        // `::Cygwin`/`::QNX`.new(...) — pure path-string assembly (positional path
        // or basename/dirname/volume/CWD/SPEC pairs), no FS / cwd / env / user code
        // (registry reads + a one-time SPEC-subclass registration, which the VM
        // owns). Built via the single `build_io_path_instance` impl the
        // interpreter's `dispatch_new` arm also delegates to.
        if method == "new"
            && let Value::Package(class_name) = &target
            && let Some(result) = self.try_native_io_path_construct(*class_name, &args)
        {
            self.method_dispatch_pure = true;
            return result;
        }
        // Native Failure construction: `Failure.new($exception?)` — pure data
        // assembly reading only VM-owned state (`$!` from env, the exception's MRO
        // from the registry), no FS / process / user code. Built via the single
        // `build_native_failure_value` impl the interpreter's
        // `dispatch_new_and_constructors` arm also delegates to.
        if method == "new"
            && let Value::Package(class_name) = &target
            && class_name.resolve() == "Failure"
        {
            self.method_dispatch_pure = true;
            return Ok(self.build_native_failure_value(&args));
        }
        // Native Seq construction: `Seq.new($iterator?)` — registers the
        // iterator into the VM-owned predictive/deferred carrier tables (no
        // eager pull, no FS / process / user code). Built via the single
        // `try_native_seq_construct` impl the interpreter's `dispatch_new` arm
        // also delegates to. (A user subclass `class S is Seq` resolves to its
        // own class name and is left to the interpreter.)
        if method == "new"
            && let Value::Package(class_name) = &target
            && class_name.resolve() == "Seq"
        {
            self.method_dispatch_pure = true;
            return Ok(self.try_native_seq_construct(&args));
        }
        // Native IO::Socket::INET construction: `IO::Socket::INET.new(...)` —
        // the real bind/connect writes only VM-owned `io_handles` state (same
        // shape as the native `IO::Path.open`). Built via the single
        // `dispatch_socket_inet_new` impl the interpreter's `dispatch_new` arm
        // also delegates to. (A user subclass resolves to its own class name and
        // is left to the interpreter.)
        if method == "new"
            && let Value::Package(class_name) = &target
            && class_name.resolve() == "IO::Socket::INET"
        {
            self.method_dispatch_pure = true;
            return self.dispatch_socket_inet_new(&args);
        }
        // Native built-in *class* method (a pure type-object method other than
        // `.new`, e.g. `Instant.from-posix`) — built directly instead of routing
        // through the interpreter's class-method dispatch.
        if let Value::Package(class_name) = &target
            && let Some(result) = crate::runtime::Interpreter::try_native_builtin_class_method(
                *class_name,
                method,
                &args,
            )
        {
            self.method_dispatch_pure = true;
            return result;
        }
        if let Value::Instance { class_name, .. } = &target {
            let class = class_name.resolve();
            // Interpreter-native pure-handle IO dispatch (PLAN.md ③ native IO PR-C/PR-D):
            // resolve `IO::Handle`'s state-only methods (close/tell/eof/seek/
            // opened/t and the Tier-1 setters/getters chomp/nl-out/out-buffer/
            // encoding/native-descriptor) in the Interpreter through its own `io_handles`
            // handle, *before* the generic native-method fallback below would
            // otherwise pre-empt them. Returns `None` (falling through to that
            // fallback unchanged) for any receiver/method/args it cannot handle
            // natively, so this is behavior-invariant.
            if let Some(result) = self.try_native_io_handle_method(&target, method, &args) {
                return result;
            }
            // Interpreter-native text output to a File+UTF8 `IO::Handle` (print/put/say/
            // print-nl): the write touches only handle state (PR-D Tier-2a).
            // Stdout/Stderr (need emit_output) and non-UTF8 File fall through.
            if let Some(result) = self.try_native_io_handle_output(&target, method, &args) {
                return result;
            }
            // Interpreter-native raw byte output to a File `IO::Handle` (write/spurt):
            // raw file write, no buffering/encoding (PR-D Tier-2c). Stdout/Stderr
            // and a non-UTF8 spurt of a Str fall through.
            if let Some(result) = self.try_native_io_handle_byte_output(&target, method, &args) {
                return result;
            }
            // Interpreter-native line read from a File+UTF8 `IO::Handle` (get): reads via
            // the handle's record reader (PR-D read side). ArgFiles/Stdin/non-UTF8
            // (which need @*ARGS / decode) fall through.
            if let Some(result) = self.try_native_io_handle_read(&target, method, &args) {
                return result;
            }
            // Interpreter-native pure-lexical `IO::Path` methods (`.parent`/`.add`/
            // `.basename`/`.sibling`/`.parts`/`.extension`/…): derive a new
            // path/string/bool purely from the receiver's attributes, with no
            // filesystem / cwd / env dependency (ledger §D). Single impl shared with
            // the interpreter's `native_io_path`. Filesystem / cwd-relative forms and
            // `child :secure` return `None` and fall through to the native fork below.
            if Self::is_io_path_lexical_class(&class)
                && let Value::Instance { attributes, .. } = &target
                && let Some(result) =
                    Self::try_io_path_lexical(&class, &attributes.as_map(), method, &args)
            {
                return result;
            }
            // Interpreter-native `.absolute` / `.relative` (path + cwd, lexical — no
            // filesystem; the VM owns env/cwd). Single impl shared with `native_io_path`.
            if Self::is_io_path_lexical_class(&class)
                && let Value::Instance { attributes, .. } = &target
                && let Some(result) =
                    self.try_io_path_cwd_method(&attributes.as_map(), method, &args)
            {
                return result;
            }
            // Interpreter-native filesystem `stat`-only file tests / accessors
            // (`e`/`f`/`d`/…/`s`/`modified`): resolve the path against the VM-owned
            // cwd, then `stat` only — no `io_handles`, no content read (ledger §D).
            // Single impl shared with `native_io_path`.
            if Self::is_io_path_lexical_class(&class)
                && let Value::Instance { attributes, .. } = &target
                && let Some(result) = self.try_io_path_fs_stat(&attributes.as_map(), method)
            {
                return result;
            }
            // Interpreter-native whole-file content reads (`slurp`/`lines`/`words`):
            // read the file + split/decode; no `io_handles` (ledger §D). Single impl
            // shared with `native_io_path`.
            if Self::is_io_path_lexical_class(&class)
                && let Value::Instance { attributes, .. } = &target
                && let Some(result) =
                    self.try_io_path_content_read(&attributes.as_map(), method, &args)
            {
                return result;
            }
            // Interpreter-native single-path filesystem mutations (`spurt`/`mkdir`/
            // `rmdir`/`unlink`/`chmod`): one-shot syscall, no `io_handles` (ledger
            // §D). Single impl shared with `native_io_path`.
            if Self::is_io_path_lexical_class(&class)
                && let Value::Instance { attributes, .. } = &target
                && let Some(result) =
                    self.try_io_path_fs_mutate(&attributes.as_map(), &class, method, &args)
            {
                return result;
            }
            // Interpreter-native `open`: allocate an `io_handles` entry and return
            // the `IO::Handle`. The VM owns `io_handles`, so this is a native
            // dispatch (ledger §D ③). Single impl shared with `native_io_path`.
            if Self::is_io_path_lexical_class(&class)
                && let Value::Instance { attributes, .. } = &target
                && let Some(result) = self.try_io_path_open(&attributes.as_map(), method, &args)
            {
                return result;
            }
            // Interpreter-native two-path FS ops (`copy`/`rename`/`move`/`symlink`/
            // `link`): resolve both paths against the VM-owned cwd, one-shot syscall,
            // no `io_handles` (ledger §D). Single impl shared with `native_io_path`.
            if Self::is_io_path_lexical_class(&class)
                && let Value::Instance { attributes, .. } = &target
                && let Some(result) =
                    self.try_io_path_two_path_op(&attributes.as_map(), method, &args)
            {
                return result;
            }
            // Interpreter-native `comb`: read the file then comb the content (no
            // `io_handles`; ledger §D). Single impl shared with `native_io_path`.
            if Self::is_io_path_lexical_class(&class)
                && let Value::Instance { attributes, .. } = &target
                && let Some(result) = self.try_io_path_comb(&attributes.as_map(), method, &args)
            {
                return result;
            }
            // A user-defined subclass of a builtin type may override an inherited
            // native method (e.g. `class IO::Blob is IO::Handle { method get {…} }`).
            // The user override must win, so do not take the native fork when the
            // class (via its MRO) provides its own method of this name.
            if self.is_native_method(&class, method) && !self.has_user_method(&class, method) {
                // TODO: compile to bytecode — Instance native-method fork (ledger §1).
                crate::vm::vm_stats::record_method_fallback(method);
                return loan_env!(self, call_method_with_values(target, method, args));
            }
        }
        // CARRIER: MOP pseudo-methods (reflection; no bytecode form). See ledger §C.
        // Pseudo-methods must always go through the interpreter which handles
        // them specially — never intercept via the compiled fast path.
        if matches!(
            method,
            "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
        ) {
            crate::vm::vm_stats::record_method_fallback(method);
            return loan_env!(self, call_method_with_values(target, method, args));
        }
        // Private method fast path: resolve private candidate and run compiled code
        // when caller context clearly allows direct dispatch.
        if method.starts_with('!') {
            let class_name = match &target {
                Value::Instance { class_name, .. } => Some(class_name.resolve()),
                Value::Package(name) => Some(name.resolve()),
                _ => None,
            };
            if let Some(cn) = class_name {
                let resolved = loan_env!(self, resolve_private_method_for_vm(&cn, method, &args));
                if let Some((owner_class, method_def)) = resolved {
                    let caller_allowed = self.can_fast_dispatch_private_method_vm(&owner_class);
                    if caller_allowed && let Some(ref cc) = method_def.compiled_code {
                        let cc = cc.clone();
                        let target_id = match &target {
                            Value::Instance { id, .. } => Some(*id),
                            _ => None,
                        };
                        let attrs_cell = match &target {
                            Value::Instance { attributes, .. } => Some(attributes.clone()),
                            _ => None,
                        };
                        let attributes = match &target {
                            Value::Instance { attributes, .. } => attributes.to_map(),
                            _ => std::collections::HashMap::new(),
                        };
                        let invocant_for_dispatch = if attributes.is_empty() {
                            Value::Package(crate::symbol::Symbol::intern(&cn))
                        } else {
                            target.clone()
                        };
                        let pushed_dispatch = loan_env!(
                            self,
                            push_method_dispatch_frame(&cn, method, &args, invocant_for_dispatch,)
                        );
                        let invocant = Some(target);
                        let empty_fns = HashMap::new();
                        let method_result = self.call_compiled_method(
                            &cn,
                            &owner_class,
                            method,
                            &method_def,
                            &cc,
                            attributes,
                            args,
                            invocant,
                            &empty_fns,
                        );
                        if pushed_dispatch {
                            self.pop_method_dispatch();
                        }
                        self.pop_method_samewith_context();
                        let (result, new_attrs, attrs_adjusted) = method_result?;
                        if let Some(id) = target_id {
                            // Commit only a `:=`-adjusted snapshot: an unadjusted
                            // one equals the cell and the whole-map write would
                            // race with concurrent cell-CAS (lost updates).
                            if attrs_adjusted && let Some(cell) = &attrs_cell {
                                cell.commit_attrs(new_attrs.clone());
                            }
                            if !self.in_lvalue_assignment
                                && let Value::Proxy { ref fetcher, .. } = result
                            {
                                // Without a `:=` adjustment the triple's map is
                                // the stale entry snapshot (the lazy reconcile
                                // skips the cell clone) — re-snapshot the live
                                // cell for the proxy fetcher.
                                let proxy_attrs = if !attrs_adjusted && let Some(cell) = &attrs_cell
                                {
                                    cell.to_map()
                                } else {
                                    new_attrs
                                };
                                return loan_env!(
                                    self,
                                    proxy_fetch(fetcher, None, &cn, &proxy_attrs, id)
                                );
                            }
                        }
                        return Ok(result);
                    }
                }
            }
        }
        // User-defined ^method (metamethod) dispatch:
        // Foo.^bar passes Foo as the first positional argument.
        if method.starts_with('^')
            && method.len() > 1
            && !crate::runtime::Interpreter::is_classhow_method(&method[1..])
        {
            let class_name = match &target {
                Value::Instance { class_name, .. } => Some(class_name.resolve()),
                Value::Package(name) => Some(name.resolve()),
                _ => None,
            };
            if let Some(cn) = class_name
                && self.has_user_method(&cn, method)
            {
                let mut how_args = vec![target.clone()];
                how_args.extend(args);
                // CARRIER: user-defined ^metamethod dispatch (MOP). See ledger §C.
                crate::vm::vm_stats::record_method_fallback(method);
                return loan_env!(self, call_method_with_values(target, method, how_args));
            }
        }
        // Only attempt compiled path for Instance or Package targets
        let class_name = match &target {
            Value::Instance { class_name, .. } => Some(class_name.resolve()),
            Value::Package(name) => Some(name.resolve()),
            _ => None,
        };
        if let Some(ref cn) = class_name {
            let class_sym = crate::symbol::Symbol::intern(cn);
            let method_sym = crate::symbol::Symbol::intern(method);
            let cache_key = (class_sym, method_sym);

            // Fast method dispatch cache: skip wrap chain check, compiled_code
            // extraction, and param_def eligibility scans for known-fast methods.
            if !self.has_any_wrap_chains()
                && let Some(entry) = self.fast_method_cache.get(&cache_key)
                && args.len() <= entry.positional_count
            {
                let needs_default_eval =
                    entry.has_defaults && args.len() < entry.positional_count && {
                        let mut pos = 0;
                        entry.method_def.param_defs.iter().any(|pd| {
                            if pd.is_invocant || pd.traits.iter().any(|t| t == "invocant") {
                                return false;
                            }
                            let result = pos >= args.len() && pd.default.is_some();
                            pos += 1;
                            result
                        })
                    };
                let has_attr_aliases = match &target {
                    Value::Instance { attributes, .. } => attributes
                        .as_map()
                        .keys()
                        .any(|k| k.starts_with(super::vm_method_dispatch::ATTR_ALIAS_META_PREFIX)),
                    _ => false,
                };
                // Slice 2d (method follow-up): a container variable passed into a
                // plain scalar `$` param must share the caller's container, which
                // only the slow `bind_function_args_values` path realizes. Skip
                // the cached fast path so it falls through to the full resolve
                // (which dispatches with `None` -> `call_compiled_method`).
                let shares_scalar_container =
                    self.method_shares_container_into_scalar_param(&entry.method_def, &args);
                if !needs_default_eval && !has_attr_aliases && !shares_scalar_container {
                    let owner_class = entry.owner_class.resolve();
                    let method_def = entry.method_def.clone();
                    let cc = entry.compiled_code.clone();
                    let can_skip_merge = entry.can_skip_merge;
                    return self.dispatch_compiled_method(
                        cn,
                        &owner_class,
                        method,
                        &method_def,
                        &cc,
                        target,
                        args,
                        Some(can_skip_merge),
                    );
                }
            }

            if let Some((owner_class, method_def)) = {
                // Monomorphic inline cache: single-entry check before HashMap.
                if let Some((cc, cm, ref co, ref cd)) = self.last_method_resolve
                    && cc == class_sym
                    && cm == method_sym
                    && !cd.is_multi
                {
                    Some((co.clone(), cd.clone()))
                } else {
                    let cached = self.method_resolve_cache.get(&cache_key).cloned();
                    let result: Option<(String, std::sync::Arc<crate::runtime::MethodDef>)> =
                        if let Some(ref hit) = cached
                            && let Some((_, def)) = hit
                            && !def.is_multi
                        {
                            hit.clone()
                        } else if let Some(arg_keys) = Self::multi_arg_type_keys(&args)
                            && self.multi_dispatch_type_cacheable(class_sym, method_sym, cn, method)
                        {
                            // Sound multi-method resolution cache (§B): a type+arity-
                            // deterministic multi resolves as a function of (class,
                            // method, positional arg types), so key it on those types
                            // rather than re-running the MRO/specificity walk per call.
                            let mkey = (class_sym, method_sym, arg_keys);
                            if let Some(hit) = self.multi_resolve_cache.get(&mkey) {
                                hit.clone()
                            } else {
                                let resolved = loan_env!(
                                    self,
                                    resolve_method_with_owner_invocant(cn, method, &args, &target)
                                );
                                let resolved_arc =
                                    resolved.map(|(owner, def)| (owner, std::sync::Arc::new(def)));
                                // Never cache an ambiguous multi resolution — it must
                                // re-raise X::Multi::Ambiguous on every call.
                                if !self.dispatch_ambiguous {
                                    self.multi_resolve_cache.insert(mkey, resolved_arc.clone());
                                }
                                resolved_arc
                            }
                        } else {
                            let resolved = loan_env!(
                                self,
                                resolve_method_with_owner_invocant(cn, method, &args, &target)
                            );
                            let resolved_arc =
                                resolved.map(|(owner, def)| (owner, std::sync::Arc::new(def)));
                            if resolved_arc.as_ref().is_none_or(|(_, def)| !def.is_multi) {
                                self.method_resolve_cache
                                    .insert(cache_key, resolved_arc.clone());
                            }
                            resolved_arc
                        };
                    if let Some((ref owner, ref def)) = result
                        && !def.is_multi
                    {
                        self.last_method_resolve =
                            Some((class_sym, method_sym, owner.clone(), def.clone()));
                    }
                    result
                }
            } {
                if let Some(result) = self.check_method_wrap_chain(
                    cn,
                    &owner_class,
                    method,
                    &method_def,
                    &target,
                    &args,
                ) {
                    return result;
                }
                // Resolve to a method def that has compiled bytecode. Normally
                // `compiled_code` is already populated at class registration. A
                // few sites add methods after that compile pass (e.g.
                // `.^add_method`, ledger §1) and leave `compiled_code = None`;
                // for those, compile on demand here so the method runs as
                // bytecode instead of tree-walking through the interpreter.
                let compiled_def: Option<(String, std::sync::Arc<crate::runtime::MethodDef>)> =
                    if method_def.compiled_code.is_some() {
                        Some((owner_class.clone(), method_def.clone()))
                    } else if !method_def.body.is_empty()
                        && let Some((owner, def)) = self.populate_uncompiled_method(
                            cn,
                            &owner_class,
                            method,
                            &args,
                            &target,
                        )
                    {
                        // Refresh the resolve caches so future calls take the
                        // already-compiled fast path without re-resolving.
                        self.method_resolve_cache
                            .insert(cache_key, Some((owner.clone(), def.clone())));
                        if !def.is_multi {
                            self.last_method_resolve =
                                Some((class_sym, method_sym, owner.clone(), def.clone()));
                        }
                        Some((owner, def))
                    } else {
                        None
                    };
                if let Some((owner_class, method_def)) = compiled_def {
                    let cc = method_def.compiled_code.clone().expect("compiled_code set");

                    // Try to populate fast_method_cache for future calls
                    if !method_def.is_multi {
                        self.try_populate_fast_cache(cache_key, cn, &owner_class, &method_def, &cc);
                    }

                    return self.dispatch_compiled_method(
                        cn,
                        &owner_class,
                        method,
                        &method_def,
                        &cc,
                        target,
                        args,
                        None,
                    );
                }
            }
        }
        // Native `.map` / `.grep` over a concrete array with a simple block: run
        // the iteration loop in the Interpreter instead of the interpreter (lever A). No
        // `target_name` here (the receiver is a value, not a mutable variable),
        // so `$_`-mutating blocks fall back to the interpreter.
        if let Some(result) = self.try_native_array_map(None, &target, method, &args) {
            return result;
        }
        // Native `.subst` over a Str with a simple pattern/replacement (lever A).
        if let Some(result) = self.try_native_subst(&target, method, &args) {
            return result;
        }
        // Native `.sort` over a plain array with no/simple comparator (lever A).
        if let Some(result) = self.try_native_sort(&target, method, &args) {
            return result;
        }
        // Native `.min` / `.max` over a plain list, including `:by` blocks.
        if let Some(result) = self.try_native_extrema(&target, method, &args) {
            return result;
        }
        // Native `.minmax` over a plain list, including `:by` blocks.
        if let Some(result) = self.try_native_minmax(&target, method, &args) {
            return result;
        }
        // Native `.first` over a plain list (no-adverb forms), including blocks.
        if let Some(result) = self.try_native_first(&target, method, &args) {
            return result;
        }
        // Native QuantHash coercion `.Set`/`.Bag`/`.Mix`/`.SetHash`/`.BagHash`/
        // `.MixHash` over a list-like aggregate or a plain Cool scalar (ledger §D:
        // native receiver dispatch -> Interpreter-native). Pure element-folding
        // shared with the interpreter via `builtins::quanthash_coerce`. `.MixHash`
        // embeds its type metadata directly in the `Value::Mix` Arc (not an
        // interpreter-owned side table — container metadata has travelled in the
        // value since #2952), so it is a pure value op like the others. A scalar
        // (`42.Set`) becomes a single-element collection. Instance/Package/Junction
        // receivers fall through.
        if args.is_empty()
            && let Some(result) = Self::try_native_quanthash_coerce(&target, method)
        {
            return result;
        }
        // Native `.Map` / `.Hash` coercion over a list-like aggregate or a plain
        // Cool scalar (pure value op; the `Map` declared-type is embedded in the
        // Hash Arc). A scalar (`42.Hash`) raises the same odd-number error as the
        // interpreter. Instance/Package/Junction fall through.
        if args.is_empty()
            && let Some(result) = Self::try_native_map_hash_coerce(&target, method)
        {
            return result;
        }
        // Native `.Seq` coercion over a structural receiver (Seq/Array/Slip/
        // Range/bare scalar). Supply/LazyList/Instance need state and fall
        // through to the interpreter.
        if args.is_empty()
            && method == "Seq"
            && let Some(result) = crate::builtins::seq_coerce::to_seq_structural(&target)
        {
            return Ok(result);
        }
        // Native `.iterator` construction over a plain receiver (Range/Set/Bag/
        // Mix/List/Array/...): builds the `Iterator` instance via the single
        // `builtins::iterator_construct` impl the interpreter also uses. `Seq`
        // (consumed-state + `squish` env mutation) and an already-built Iterator
        // fall through to the interpreter.
        if args.is_empty()
            && let Some(result) = Self::try_native_iterator_construct(&target, method)
        {
            return Ok(result);
        }
        // Native `.IO` coercion over a Cool scalar (`"path".IO`, `42.IO`) — builds
        // an IO::Path via the shared `make_io_path_instance` (ledger §D). Instance /
        // non-IO Package / aggregate receivers fall through.
        if let Some(result) = self.try_native_io_coercion(&target, method, &args) {
            return result;
        }
        // Native `.encode` (Cool scalar -> Buf) / `.decode` (Buf/Blob -> Str) —
        // pure transformation via the VM-owned encoding registry, no `io_handles`
        // (ledger §D). Single impl shared with the interpreter catch-all.
        if let Some(result) = self.try_native_encode_decode(&target, method, &args) {
            return result;
        }
        // TODO: compile to bytecode — native/Buf/Failure method fork (ledger §1).
        // User-defined Instance methods now always run as bytecode (compiled at
        // registration, or on demand above via `populate_uncompiled_method`), so
        // what remains here is native receiver dispatch (Buf/Blob/Failure/IO and
        // the `dispatch_method_by_name_*` machinery) that depends on interpreter-
        // owned state (③ state ownership / first-class container Phase 2).
        crate::vm::vm_stats::record_method_fallback(method);
        self.vm_call_method_with_values(target, method, args)
    }
}
