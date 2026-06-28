use super::*;

impl Interpreter {
    pub(super) fn try_compiled_method_mut_or_interpret(
        &mut self,
        target_name: &str,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // Native default construction (see `try_compiled_method_or_interpret`).
        if method == "new"
            && let Value::Package(class_name) = &target
            && let Some(result) = loan_env!(self, try_native_default_construct(*class_name, &args))
        {
            // Pure construction: fresh instance, no caller-env write (Slice 6.3) —
            // UNLESS the class has a `submethod BUILD`/`TWEAK` whose body can mutate
            // a captured-outer caller lexical, in which case the dispatch is impure
            // and the call site must reconcile the caller slot (Slice F twin of the
            // non-mut path; `reconcile_locals_from_env_at_site`).
            self.method_dispatch_pure = !self.mro_has_build_or_tweak(&class_name.resolve());
            return result;
        }
        // Native built-in construction (mut path twin of the above).
        if method == "new"
            && let Value::Package(class_name) = &target
            && let Some(result) =
                crate::runtime::Interpreter::try_native_builtin_construct(*class_name, &args)
        {
            self.method_dispatch_pure = true;
            return result;
        }
        // Native QuantHash construction (mut path twin of the above).
        if method == "new"
            && let Value::Package(class_name) = &target
            && let Some(result) =
                self.try_native_quanthash_construct_for_package(*class_name, &args)
        {
            self.method_dispatch_pure = true;
            return result;
        }
        // Native aggregate construction (mut path twin of the above).
        if method == "new"
            && let Value::Package(class_name) = &target
            && let Some(result) =
                self.try_native_aggregate_construct_for_package(*class_name, &args)
        {
            self.method_dispatch_pure = true;
            return result;
        }
        // Native IO::Path family construction (mut path twin of the above).
        if method == "new"
            && let Value::Package(class_name) = &target
            && let Some(result) = self.try_native_io_path_construct(*class_name, &args)
        {
            self.method_dispatch_pure = true;
            return result;
        }
        // Native Failure construction (mut path twin of the above).
        if method == "new"
            && let Value::Package(class_name) = &target
            && class_name.resolve() == "Failure"
        {
            self.method_dispatch_pure = true;
            return Ok(self.build_native_failure_value(&args));
        }
        // Native Seq construction (mut path twin of the above).
        if method == "new"
            && let Value::Package(class_name) = &target
            && class_name.resolve() == "Seq"
        {
            self.method_dispatch_pure = true;
            return Ok(self.try_native_seq_construct(&args));
        }
        // Native IO::Socket::INET construction (mut path twin of the above).
        if method == "new"
            && let Value::Package(class_name) = &target
            && class_name.resolve() == "IO::Socket::INET"
        {
            self.method_dispatch_pure = true;
            return self.dispatch_socket_inet_new(&args);
        }
        // Native built-in class method (mut path twin of the above).
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
            // Interpreter-native pure-handle IO dispatch (PLAN.md ③ native IO PR-C/PR-D),
            // mut path: `$fh.method` on a variable receiver routes here, so the
            // same state-only `IO::Handle` methods must be intercepted before the
            // generic native-method fallback below. These methods mutate only the
            // shared handle-table state (not the receiver binding), so the native
            // path returns the result directly; `None` falls through unchanged.
            if let Some(result) = self.try_native_io_handle_method(&target, method, &args) {
                return result;
            }
            // Interpreter-native text output to a File+UTF8 `IO::Handle` (print/put/say/
            // print-nl), mut path (PR-D Tier-2a). See the non-mut twin above.
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
            // Interpreter-native pure-lexical `IO::Path` methods for variable
            // receivers (`$p.parent`, `$p.add(...)`): same pure value op as the
            // non-mut path — produces a *new* IO::Path/string/bool and never mutates
            // the receiver, so no writeback. Filesystem / cwd-relative forms and
            // `child :secure` fall through to the native fork below.
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
                // TODO: compile to bytecode — Instance native-method fork, mut (ledger §1).
                crate::vm::vm_stats::record_method_fallback(method);
                return self.vm_call_method_mut_with_values(target_name, target, method, args);
            }
        }
        if matches!(
            method,
            "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
        ) {
            // CARRIER: MOP pseudo-methods, mut (reflection). See ledger §C.
            crate::vm::vm_stats::record_method_fallback(method);
            return loan_env!(
                self,
                call_method_mut_with_values(target_name, target, method, args)
            );
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
                // CARRIER: user-defined ^metamethod dispatch, mut (MOP). See ledger §C.
                crate::vm::vm_stats::record_method_fallback(method);
                return loan_env!(self, call_method_with_values(target, method, how_args));
            }
        }
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
        // Reuse the receiver's already-interned class Symbol instead of
        // resolving it to a String and re-interning that String back to a
        // Symbol (`Symbol -> resolve() -> intern()` round-trip) on every call.
        let class_sym_opt = match &target {
            Value::Instance { class_name, .. } => Some(*class_name),
            Value::Package(name) => Some(*name),
            _ => None,
        };
        let class_name = class_sym_opt.map(|s| s.resolve());
        if let Some(cn) = class_name
            && let Some(class_sym) = class_sym_opt
            && let Some((owner_class, method_def)) = {
                let method_sym = crate::symbol::Symbol::intern(method);
                self.resolve_method_cached(&cn, method, class_sym, method_sym, &args, &target)
            }
        {
            // Ambiguous multi dispatch: two or more candidates matched equally
            // well. Raise X::Multi::Ambiguous instead of silently picking one.
            if self.dispatch_ambiguous {
                self.dispatch_ambiguous = false;
                let sigs = self.format_method_candidate_signatures(&cn, method);
                return Err(
                    crate::runtime::methods_signature_errors::make_multi_ambiguous_error(
                        method, &cn, &sigs,
                    ),
                );
            }
            if let Some(result) =
                self.check_method_wrap_chain(&cn, &owner_class, method, &method_def, &target, &args)
            {
                return result;
            }
            // Resolve to a def carrying compiled bytecode, compiling on demand
            // for methods added after the registration compile pass (e.g.
            // `.^add_method`, ledger §1) so they run as bytecode rather than
            // tree-walking. None → native receiver, keep interpreter fallback.
            let resolved: Option<(String, std::sync::Arc<crate::runtime::MethodDef>)> =
                if method_def.compiled_code.is_some() {
                    Some((owner_class, method_def))
                } else if !method_def.body.is_empty() {
                    self.populate_uncompiled_method(&cn, &owner_class, method, &args, &target)
                } else {
                    None
                };
            if let Some((owner_class, method_def)) = resolved {
                let cc = method_def.compiled_code.clone().expect("compiled_code set");
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
                    Value::Package(class_sym)
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
                    // Commit only a `:=`-adjusted snapshot (cell-CAS race
                    // avoidance — see the primary dispatch site).
                    if attrs_adjusted && let Some(cell) = &attrs_cell {
                        cell.commit_attrs(new_attrs.clone());
                    }
                    if !self.in_lvalue_assignment
                        && let Value::Proxy { ref fetcher, .. } = result
                    {
                        // Without a `:=` adjustment the triple's map is the stale
                        // entry snapshot (lazy reconcile) — re-snapshot the live
                        // cell for the proxy fetcher.
                        let proxy_attrs = if !attrs_adjusted && let Some(cell) = &attrs_cell {
                            cell.to_map()
                        } else {
                            new_attrs
                        };
                        return loan_env!(self, proxy_fetch(fetcher, None, &cn, &proxy_attrs, id));
                    }
                }
                return Ok(result);
            }
        }
        // Native `.map` / `.grep` over a concrete array with a simple block (lever A).
        // `target_name` is the receiver variable, enabling rw-binding writeback
        // for `$_`-mutating blocks (`@a.map({ $_++ })` mutates `@a`).
        if let Some(result) = self.try_native_array_map(Some(target_name), &target, method, &args) {
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
        // `.MixHash` over a list-like aggregate or plain Cool scalar. Variable
        // receivers (`@a.Set`, `$s.Bag`) compile to CallMethodMut and so land on this
        // mut path; the
        // coercion produces a *new* Set/Bag/Mix value and never mutates the
        // receiver variable, so there is no writeback — identical to the non-mut
        // path's native dispatch. Instance/Package receivers fall through.
        if args.is_empty()
            && let Some(result) = Self::try_native_quanthash_coerce(&target, method)
        {
            return result;
        }
        // Native `.Map` / `.Hash` coercion for variable receivers (`%h.Map`,
        // `@a.Hash`) — same pure value op as the non-mut path, no writeback.
        if args.is_empty()
            && let Some(result) = Self::try_native_map_hash_coerce(&target, method)
        {
            return result;
        }
        // Native `.Seq` coercion for variable receivers (`@a.Seq`) — structural
        // receivers only, same pure value op as the non-mut path.
        if args.is_empty()
            && method == "Seq"
            && let Some(result) = crate::builtins::seq_coerce::to_seq_structural(&target)
        {
            return Ok(result);
        }
        // Native `.IO` coercion over a Cool scalar for variable receivers
        // (`$s.IO`) — builds a *new* IO::Path and never mutates the receiver, so no
        // writeback. Instance / non-IO Package / aggregate receivers fall through.
        if let Some(result) = self.try_native_io_coercion(&target, method, &args) {
            return result;
        }
        // Native `.encode` (Cool scalar -> Buf) / `.decode` (Buf/Blob -> Str) for
        // variable receivers (`$s.encode("utf-16")`) — same pure transformation as
        // the non-mut path; returns a *new* Buf/Str, no writeback.
        if let Some(result) = self.try_native_encode_decode(&target, method, &args) {
            return result;
        }
        // TODO: compile to bytecode — native/Buf/Failure method fork, mut (ledger §1).
        // User-defined methods run as bytecode (compiled at registration or on
        // demand above); what remains is native receiver dispatch blocked on
        // ③ state ownership / first-class container Phase 2.
        crate::vm::vm_stats::record_method_fallback(method);
        self.vm_call_method_mut_with_values(target_name, target, method, args)
    }
}
