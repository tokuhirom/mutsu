use super::*;

impl Interpreter {
    /// Symbol-keyed entry (see `try_compiled_method_or_interpret_sym`).
    pub(super) fn try_compiled_method_mut_or_interpret_sym(
        &mut self,
        target_name: &str,
        target: Value,
        method_sym: crate::symbol::Symbol,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // #8880: the caller has already established that this receiver class and
        // method name walk the whole probe prefix below without a single probe
        // claiming the call, so go straight to the dispatch tail. Consumed here
        // (rather than read) so the flag can never outlive one dispatch.
        if std::mem::take(&mut self.plain_method_lane_active) {
            return self.compiled_mut_resolved_dispatch(target_name, target, method_sym, args);
        }
        let method: &str = method_sym.as_str();
        // Decode the receiver's type-object symbol once. The type-object guards
        // below (construction, built-in class methods) are all gated on a
        // `Package` receiver, and eight of them on `.new` as well. Asking
        // `target.view()` per guard re-decoded the same NaN-box tag up to ten
        // times per call, and re-compared the method name to "new" at each of
        // those eight, for receivers none of them can claim (issue #8888).
        // `ValueView::Package` is produced by `Kind::Package` alone, so this one
        // decode answers every one of those guards.
        let package_sym = match target.view() {
            ValueView::Package(sym) => Some(sym),
            _ => None,
        };
        let new_on_package = if method == "new" { package_sym } else { None };
        // Calling a method on a role TYPE OBJECT puns the role, and punning is
        // a composition: the role's body runs. This fast path dispatched the
        // role's method straight off the role, so the body never ran at all —
        // `role R { my $x = 7; method gx { $x } }` answered `Nil` from `R.gx`,
        // and a body `my class KV` reported `Undeclared name: KV` because
        // nothing had ever declared it.
        //
        // Only the BODIES are run here, not the full
        // `ensure_role_punned_to_class`: registering the pun class would put
        // the role into `registry.classes`, which `.isa`, `.^methods` and
        // candidate-list introspection read, so a role would start reporting
        // itself as a class as soon as anything called a method on it. The
        // full pun still happens on the paths that genuinely need the class.
        //
        // Ordered cheapest-first: an ordinary class receiver fails the
        // `classes` probe outright, and `run_pun_role_bodies` is memoized so
        // every later call on the same role is one `HashSet` hit.
        if let Some(name) = package_sym {
            let pkg = name.as_str();
            if !self.registry().classes.contains_key(pkg) && self.is_role(pkg) {
                self.run_pun_role_bodies(pkg)?;
            }
        }
        // Native default construction (see `try_compiled_method_or_interpret`).
        if let Some(class_name) = new_on_package
            && let Some(result) = loan_env!(self, try_native_default_construct(class_name, &args))
        {
            // Pure construction: fresh instance, no caller-env write (Slice 6.3) —
            // UNLESS the class has a `submethod BUILD`/`TWEAK` whose body can mutate
            // a captured-outer caller lexical, in which case the dispatch is impure
            // and the call site must reconcile the caller slot (Slice F twin of the
            // non-mut path; `reconcile_locals_from_env_at_site`).
            self.method_dispatch_pure = !self.mro_has_build_or_tweak(class_name);
            return result;
        }
        // Native built-in construction (mut path twin of the above).
        if let Some(class_name) = new_on_package
            // Augmented `multi method new` candidates must reach dispatch_new
            // (see the non-mut twin).
            && !self.has_user_method(&class_name.resolve(), "new")
            && let Some(result) =
                self.try_native_builtin_construct(class_name, &args)
        {
            self.method_dispatch_pure = true;
            return result;
        }
        // Native QuantHash construction (mut path twin of the above).
        if let Some(class_name) = new_on_package
            && !self.user_declared_classes.contains(&class_name.resolve())
            && let Some(result) = self.try_native_quanthash_construct_for_package(class_name, &args)
        {
            self.method_dispatch_pure = true;
            return result;
        }
        // Native aggregate construction (mut path twin of the above).
        if let Some(class_name) = new_on_package
            && !self.user_declared_classes.contains(&class_name.resolve())
            && let Some(result) = self.try_native_aggregate_construct_for_package(class_name, &args)
        {
            self.method_dispatch_pure = true;
            return result;
        }
        // Native IO::Path family construction (mut path twin of the above).
        if let Some(class_name) = new_on_package
            && let Some(result) = self.try_native_io_path_construct(class_name, &args)
        {
            self.method_dispatch_pure = true;
            return result;
        }
        // Native Failure construction (mut path twin of the above).
        if let Some(class_name) = new_on_package
            && class_name == "Failure"
        {
            self.method_dispatch_pure = true;
            return Ok(self.build_native_failure_value(&args));
        }
        // Native Seq construction (mut path twin of the above).
        if let Some(class_name) = new_on_package
            && class_name == "Seq"
        {
            self.method_dispatch_pure = true;
            return Ok(self.try_native_seq_construct(&args));
        }
        // Native IO::Socket::INET construction (mut path twin of the above).
        if let Some(class_name) = new_on_package
            && class_name == "IO::Socket::INET"
        {
            self.method_dispatch_pure = true;
            return self.dispatch_socket_inet_new(&args);
        }
        // Native built-in class method (mut path twin of the above).
        if let Some(class_name) = package_sym
            && let Some(result) = crate::runtime::Interpreter::try_native_builtin_class_method(
                class_name, method, &args,
            )
        {
            self.method_dispatch_pure = true;
            return result;
        }
        // Native `bless` (mut path twin — `self.bless(...)` has a variable
        // receiver, so it lands here): route straight to the interpreter's
        // single `dispatch_bless` impl, skipping the generic method-dispatch
        // scan (lever A). Gated inside `try_native_bless`: registered class, no
        // user `bless` override. bless builds a *new* instance and never
        // mutates the receiver, so there is no writeback; the dispatch is
        // env-pure exactly when the plan has no BUILD/TWEAK phase.
        if method == "bless"
            && let Some(result) = loan_env!(self, try_native_bless(&target, &args))
        {
            let class_sym = match target.view() {
                ValueView::Package(name) => name,
                ValueView::Instance { class_name, .. } => class_name,
                _ => unreachable!("try_native_bless only fires on Package/Instance"),
            };
            let plan = self.native_ctor_plan(class_sym);
            self.method_dispatch_pure = !(plan.has_build || plan.has_tweak);
            return result;
        }
        if let ValueView::Instance { class_name, .. } = target.view() {
            let class = class_name.as_str();
            // Interpreter-native pure-handle IO dispatch (PLAN.md ③ native IO PR-C/PR-D),
            // mut path: `$fh.method` on a variable receiver routes here, so the
            // same state-only `IO::Handle` methods must be intercepted before the
            // generic native-method fallback below. These methods mutate only the
            // shared handle-table state (not the receiver binding), so the native
            // path returns the result directly; `None` falls through unchanged.
            // User-subclassed IO::Handle (overrides WRITE/READ/EOF): route the
            // high-level methods through the user methods before the native
            // file-handle path (which would fail — no OS handle).
            if let Some(result) = self.try_user_io_handle_method(&target, method, &args) {
                return result;
            }
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
            if Self::is_io_path_lexical_class(class)
                && let ValueView::Instance { attributes, .. } = target.view()
                && let Some(result) =
                    Self::try_io_path_lexical(class, &attributes.as_map(), method, &args)
            {
                return result;
            }
            // Interpreter-native `.absolute` / `.relative` (path + cwd, lexical — no
            // filesystem; the VM owns env/cwd). Single impl shared with `native_io_path`.
            if Self::is_io_path_lexical_class(class)
                && let ValueView::Instance { attributes, .. } = target.view()
                && let Some(result) =
                    self.try_io_path_cwd_method(&attributes.as_map(), method, &args)
            {
                return result;
            }
            // Interpreter-native filesystem `stat`-only file tests / accessors
            // (`e`/`f`/`d`/…/`s`/`modified`): resolve the path against the VM-owned
            // cwd, then `stat` only — no `io_handles`, no content read (ledger §D).
            // Single impl shared with `native_io_path`.
            if Self::is_io_path_lexical_class(class)
                && let ValueView::Instance { attributes, .. } = target.view()
                && let Some(result) = self.try_io_path_fs_stat(&attributes.as_map(), method)
            {
                return result;
            }
            // Interpreter-native whole-file content reads (`slurp`/`lines`/`words`):
            // read the file + split/decode; no `io_handles` (ledger §D). Single impl
            // shared with `native_io_path`.
            if Self::is_io_path_lexical_class(class)
                && let ValueView::Instance { attributes, .. } = target.view()
                && let Some(result) =
                    self.try_io_path_content_read(&attributes.as_map(), method, &args)
            {
                return result;
            }
            // Interpreter-native single-path filesystem mutations (`spurt`/`mkdir`/
            // `rmdir`/`unlink`/`chmod`): one-shot syscall, no `io_handles` (ledger
            // §D). Single impl shared with `native_io_path`.
            if Self::is_io_path_lexical_class(class)
                && let ValueView::Instance { attributes, .. } = target.view()
                && let Some(result) =
                    self.try_io_path_fs_mutate(&attributes.as_map(), class, method, &args)
            {
                return result;
            }
            // Interpreter-native `open`: allocate an `io_handles` entry and return
            // the `IO::Handle`. The VM owns `io_handles`, so this is a native
            // dispatch (ledger §D ③). Single impl shared with `native_io_path`.
            if Self::is_io_path_lexical_class(class)
                && let ValueView::Instance { attributes, .. } = target.view()
                && let Some(result) = self.try_io_path_open(&attributes.as_map(), method, &args)
            {
                return result;
            }
            // Interpreter-native two-path FS ops (`copy`/`rename`/`move`/`symlink`/
            // `link`): resolve both paths against the VM-owned cwd, one-shot syscall,
            // no `io_handles` (ledger §D). Single impl shared with `native_io_path`.
            if Self::is_io_path_lexical_class(class)
                && let ValueView::Instance { attributes, .. } = target.view()
                && let Some(result) =
                    self.try_io_path_two_path_op(&attributes.as_map(), method, &args)
            {
                return result;
            }
            // Interpreter-native `comb`: read the file then comb the content (no
            // `io_handles`; ledger §D). Single impl shared with `native_io_path`.
            if Self::is_io_path_lexical_class(class)
                && let ValueView::Instance { attributes, .. } = target.view()
                && let Some(result) = self.try_io_path_comb(&attributes.as_map(), method, &args)
            {
                return result;
            }
            // A user-defined subclass of a builtin type may override an inherited
            // native method (e.g. `class IO::Blob is IO::Handle { method get {…} }`).
            // The user override must win, so do not take the native fork when the
            // class (via its MRO) provides its own method of this name.
            if self.is_native_method(class, method) && !self.has_user_method(class, method) {
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
        // A user-declared `method ^bar` remains on the type's own method
        // table. Preserve its established calling convention before trying
        // the receiver's HOW, which is where a custom metaclass's `bar`
        // method lives.
        if method.starts_with('^') && method.len() > 1 {
            let class_name = match target.view() {
                ValueView::Instance { class_name, .. } => Some(class_name.as_str()),
                ValueView::Package(name) => Some(name.as_str()),
                _ => None,
            };
            if let Some(cn) = class_name
                && self.has_user_method(cn, method)
            {
                let mut user_args = Vec::with_capacity(args.len() + 1);
                user_args.push(target.clone());
                user_args.extend(args);
                crate::vm::vm_stats::record_method_fallback(method);
                return loan_env!(self, call_method_with_values(target, method, user_args));
            }
        }
        // `Foo.^bar` is otherwise dispatched as `Foo.HOW.bar(Foo, ...)`.
        // Falling through to the ordinary compiled lookup binds `self` to
        // `Foo`, which breaks custom metaclasses such as Red's
        // `.^add-relationship` during class declaration.
        if let Some(meta_method) = method.strip_prefix('^')
            && !meta_method.is_empty()
            && meta_method != "name"
        {
            let how = loan_env!(self, call_method_with_values(target.clone(), "HOW", vec![]))?;
            let mut how_args = Vec::with_capacity(args.len() + 1);
            let type_target = if matches!(
                meta_method,
                "mixin" | "set_name" | "language-revision" | "can"
            ) {
                target.clone()
            } else {
                match target.view() {
                    ValueView::Instance { class_name, .. } => Value::package(class_name),
                    _ => target.clone(),
                }
            };
            how_args.push(type_target);
            how_args.extend(args);
            // CARRIER: MOP dispatch through the receiver's HOW, mut (MOP). See ledger §C.
            crate::vm::vm_stats::record_method_fallback(method);
            return loan_env!(self, call_method_with_values(how, meta_method, how_args));
        }
        if method.starts_with('!') {
            let class_sym = match target.view() {
                ValueView::Instance { class_name, .. } => Some(class_name),
                ValueView::Package(name) => Some(name),
                _ => None,
            };
            if let Some(class_sym) = class_sym {
                let cn = class_sym.as_str();
                let resolved = loan_env!(self, resolve_private_method_for_vm(cn, method, &args));
                if let Some((owner_class, method_def)) = resolved {
                    let caller_allowed = self.can_fast_dispatch_private_method_vm(&owner_class);
                    if caller_allowed && let Some(ref cc) = method_def.compiled_code {
                        let cc = cc.clone();
                        let target_id = match target.view() {
                            ValueView::Instance { id, .. } => Some(id),
                            _ => None,
                        };
                        let attrs_cell = match target.view() {
                            ValueView::Instance { attributes, .. } => Some(attributes.clone()),
                            _ => None,
                        };
                        let attributes = match target.view() {
                            ValueView::Instance { attributes, .. } => attributes.to_map(),
                            _ => AttrMap::new(),
                        };
                        let invocant_for_dispatch = if attributes.is_empty() {
                            Value::package(class_sym)
                        } else {
                            target.clone()
                        };
                        let pushed_dispatch = loan_env!(
                            self,
                            push_method_dispatch_frame(cn, method, &args, invocant_for_dispatch,)
                        );
                        let invocant = Some(target);
                        let empty_fns = CompiledFns::default();
                        let fns_ref = method_def.compiled_fns.as_deref().unwrap_or(&empty_fns);
                        let method_result = self.call_compiled_method(
                            cn,
                            &owner_class,
                            method,
                            &method_def,
                            &cc,
                            &attributes,
                            args,
                            invocant,
                            fns_ref,
                        );
                        if pushed_dispatch {
                            self.pop_method_dispatch();
                        }
                        self.pop_method_samewith_context();
                        let (result, reconciled) = method_result?;
                        if let Some(id) = target_id {
                            // Commit only a `:=`-adjusted snapshot: an unadjusted
                            // one equals the cell and the whole-map write would
                            // race with concurrent cell-CAS (lost updates).
                            if let (Some(m), Some(cell)) = (&reconciled, &attrs_cell) {
                                cell.commit_attrs(m.clone());
                            }
                            if result.is_proxy_value()
                                && !self.in_lvalue_assignment
                                && !Self::method_is_rw_capable(&method_def)
                                && let ValueView::Proxy { fetcher, .. } = result.view()
                            {
                                // Without a `:=` adjustment the returned map is
                                // absent — re-snapshot the live cell for the
                                // proxy fetcher.
                                let proxy_attrs = match (&reconciled, &attrs_cell) {
                                    (Some(m), _) => m.clone(),
                                    (None, Some(cell)) => cell.to_map(),
                                    (None, None) => AttrMap::new(),
                                };
                                return loan_env!(
                                    self,
                                    proxy_fetch(fetcher, None, cn, &proxy_attrs, id)
                                );
                            }
                        }
                        return Ok(result);
                    }
                }
            }
        }
        self.compiled_mut_resolved_dispatch(target_name, target, method_sym, args)
    }
}
