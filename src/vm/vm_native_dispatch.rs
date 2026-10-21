use super::*;

impl Interpreter {
    /// Dispatch a pure native method, then settle any resumable warning it
    /// raised *at the raise site*.
    ///
    /// A pure native method has no `&mut Interpreter`, so the only way it can
    /// signal `Use of uninitialized value ...` / `Use of Nil in ... context` is
    /// to return `RuntimeError::warn_signal_with_resume`. Letting that unwind is
    /// wrong twice over: the resume value rides in `return_value`, which the
    /// enclosing routine boundary applies as an explicit `return` (abandoning
    /// the rest of the body), and a `CONTROL { when CX::Warn { … .resume } }`
    /// handler further up has no raise site left to resume into. `warns-like
    /// { Int.Numeric }, …` — roast's `Test::Util` idiom — lost every statement
    /// after the call that way.
    ///
    /// [`raise_resumable_warning`](Self::raise_resumable_warning) is the shared
    /// settle path: print-and-resume when no CONTROL handler is active, run a
    /// resume-safe handler inline, and otherwise fall back to the same
    /// unwinding signal as before.
    pub(crate) fn try_native_method(
        &mut self,
        target: &Value,
        method_sym: crate::symbol::Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let _region = crate::profile::enter(crate::profile::Region::NativeBuiltin);
        // The receiver step (ADR-11276 §2.4, `method_table::receiver_step`):
        // a container is transparent, a hash-like receiver and a `Match`
        // answer `Any`'s `self.list.METHOD` methods on their list, an enum
        // member and a user `IO::Path` subclass delegate to the value standing
        // in for them, and a `Channel` subclass coerces its channel. The
        // container's own `.raku` is the renderer's, below.
        match crate::builtins::method_table::receiver_step::step_in(
            self,
            target,
            method_sym.as_str(),
            args.len(),
        ) {
            Some(crate::builtins::method_table::receiver_step::Step::Redirect(receiver, _)) => {
                return self.try_native_method(&receiver, method_sym, args);
            }
            Some(crate::builtins::method_table::receiver_step::Step::Answer(answer)) => {
                return Some(answer);
            }
            Some(crate::builtins::method_table::receiver_step::Step::Itself) | None => {}
        }
        // ADR-0058: a native method reads its ARGUMENTS' elements through pure
        // Rust (the receiver is already handled by
        // `reify_or_consume_seq_target`), so a still-deferred `.map` argument —
        // `600.polymod((1..3).map(* * 3))` — must run its callback first.
        if let Err(e) = self.reify_map_grep_seq_args(args) {
            return Some(Err(e));
        }
        // A native method takes its arguments by value, so the reference an
        // element of a native `CArray` answers (`$c[0]` is an `IntPosRef`) is
        // read before the number in it reaches pure Rust:
        // `$blob.subbuf(0, $c[0])`. Only these references: reading one has no
        // effect, so a method this call does not handle natively -- which then
        // binds the original argument and reads it again -- cannot tell. Any
        // other `Proxy` is read once by the parameter that binds it (a bound
        // `Mu $test` of `grep`), and a bind keeps the container.
        // A shared scalar cell (`ContainerRef`) is read the same way: a spread
        // capture keeps the containers of its positionals (`|\($p, 2)`, or the
        // `|c` a `method subbuf(|c) { $.ords.subbuf(|c) }` forwards), and a
        // native row refuses a container argument (#12907).
        // Cost: O(n) tag probes, n = arguments; no allocation unless one is a reference.
        if args
            .iter()
            .any(|a| a.is_proxy_value() || a.is_container_ref())
            && !method_sym.as_str().starts_with("BIND")
            && !method_sym.as_str().starts_with("bind")
        {
            let mut fetched = Vec::with_capacity(args.len());
            for arg in args {
                if arg.is_container_ref() {
                    fetched.push(arg.deref_container());
                } else if arg.is_proxy_value() {
                    match self.auto_fetch_proxy(arg) {
                        Ok(value) => fetched.push(value),
                        Err(e) => return Some(Err(e)),
                    }
                } else {
                    fetched.push(arg.clone());
                }
            }
            return self.try_native_method(target, method_sym, &fetched);
        }
        // A receiver state only the general entry reads (a `Proxy`, an element
        // source behind a user iterator, a lazy source, an enum type object's
        // `pick`/`roll`, a typed `Hash.raku`): neither the table nor a cascade
        // answers it (`receiver_step::general_entry_owns`).
        if crate::builtins::method_table::receiver_step::general_entry_owns(
            self,
            target,
            method_sym.as_str(),
            args.len(),
        ) {
            return None;
        }
        // The built-in method table (ADR-11276, #8888). For a plain receiver
        // of a shape it covers, a row is the method's implementation, and no
        // probe between here and the family cascade can claim the call: each
        // is gated on a method name with no row, or on a receiver kind
        // `dispatch_shape` refuses -- the two `Cool` gates just below only
        // ever claim a type object. So the table goes first and the walk is
        // skipped outright. `builtins::method_table` cross-checks itself
        // against the full path in debug builds.
        //
        // The one thing the table cannot decide is whether user code has
        // `augment`ed the receiver's builtin type with a method of this name,
        // which is a property of the registry rather than of the call: that
        // stays the same memoized gate the general path applies.
        if let Some(result) = crate::builtins::method_table::try_dispatch_in(
            self,
            |vm| !vm.native_lever_a_user_override_sym(target, method_sym),
            target,
            method_sym,
            args,
        ) {
            return Some(self.settle_native_warning(result));
        }
        // A user RakuAST subclass keeps its native parent node in the instance.
        // Its opaque Instance shape cannot select the parent's owner-only rows,
        // so resolve those rows through the declared MRO after user overrides.
        if let ValueView::Instance { attributes, .. } = target.view()
            && attributes.rakuast_base().is_some()
            && !self.native_lever_a_user_override_sym(target, method_sym)
        {
            let ancestors = self.dispatch_owner_chain(target);
            let owners: Vec<&str> = ancestors
                .iter()
                .map(|owner| owner.as_str())
                .filter(|owner| owner.starts_with("RakuAST::"))
                .collect();
            if let Some(result) = crate::builtins::method_table::invoke_owner(
                self,
                &owners,
                method_sym.as_str(),
                args,
                || target.clone(),
            ) {
                return Some(self.settle_native_warning(result));
            }
        }
        // `Any` is not a `Cool` (#7773): the cascades below recognize a `Cool`
        // method by NAME and answer out of the stringified receiver, which for
        // a type object is its gist -- so `$undefined.uc` answered `"(ANY)"`.
        // The interpreter entry (`call_method_with_values`) carries the twin of
        // this gate; the OPCODE path reaches the cascades without passing
        // through it.
        if let Some(err) = crate::runtime::cool_method_not_found_on_any(target, method_sym.as_str())
        {
            return Some(Err(err));
        }
        if let Some(result) = self.cool_instance_text_via_str(target, method_sym.as_str(), args) {
            return Some(result);
        }
        // The `Cool`-subtype sibling (#9772): `Str.comb` / `Int.uc` on the type
        // object refuse it instead of answering out of its gist.
        if let Some(result) = self.cool_type_object_default_fmt(target, method_sym.as_str(), args) {
            return Some(result);
        }
        if let Some(result) = self.cool_type_object_string_method(target, method_sym.as_str()) {
            return Some(result);
        }
        // ADR-0068 §20: the cascades read a shared Array/Hash receiver with
        // nothing held, as the table's own entry (`dispatch::call`) would not:
        // a leaf read holds the node's stripe, any other read runs on a
        // snapshot.
        let method_name = method_sym.as_str();
        let _read = if crate::value::container_lock::multi_mutator_threads_live()
            && crate::value::container_read::is_leaf_structure_read(method_name)
        {
            crate::value::container_lock::ContainerStructGuard::acquire_for(None, target)
        } else {
            None
        };
        let snapshot = crate::value::container_read::snapshot_read_receiver(method_name, target);
        let target = snapshot.as_ref().unwrap_or(target);
        let result = self.try_native_method_raw(target, method_sym, args)?;
        Some(self.settle_native_warning(result))
    }

    /// Settle a resumable warning a pure native method returned, at its raise
    /// site (see [`Self::try_native_method`]). Every native result passes
    /// through here, so the common case -- no warning -- is one inlined test.
    #[inline(always)]
    pub(super) fn settle_native_warning(
        &mut self,
        result: Result<Value, RuntimeError>,
    ) -> Result<Value, RuntimeError> {
        match &result {
            Err(e) if e.is_warn() && e.return_value.is_some() => {
                self.raise_native_warning(e.message.to_string(), e.return_value.clone())
            }
            _ => result,
        }
    }

    #[cold]
    fn raise_native_warning(
        &mut self,
        message: String,
        resume: Option<Value>,
    ) -> Result<Value, RuntimeError> {
        self.raise_resumable_warning(&message, resume.unwrap_or(Value::NIL))
    }

    fn try_native_method_raw(
        &mut self,
        target: &Value,
        method_sym: crate::symbol::Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        // `Any.kv`/`.pairs`/... are `self.list.<method>`: an object that
        // overrides `list` answers from that list, not as one item.
        if let Some(view) = self
            .try_any_list_view_method(target, method_sym.as_str(), args)
            .transpose()
        {
            return Some(view);
        }
        // `as_str`, not `resolve`: see `native_method_0arg`. An owned copy of
        // an already-`&'static str` per native method call.
        let method_name: &str = method_sym.as_str();
        // A mixed-in role's rows (a wrapper role's `gist`, ADR-11276, #11276)
        // ahead of the native arms below that would answer the bare inner
        // value; as in `call_method_with_values`.
        if let Some(owners) =
            crate::builtins::method_table::mixin_role_owners_of(target, method_name)
            && let Some(result) = crate::builtins::method_table::invoke_owner(
                self,
                &owners.iter().map(String::as_str).collect::<Vec<_>>(),
                method_name,
                args,
                || target.clone(),
            )
        {
            return Some(result);
        }
        // `.REPR` / `.WHERE` on a NativeCall handle. Telling a `nativecast`ed
        // CStruct/CUnion/CArray from an ordinary instance needs the class
        // registry, so the pure native path cannot answer these — and it must
        // not be allowed to: its `.WHERE` is an identity hash, and a module that
        // trusted an honest `.REPR` would go on to *dereference* that hash.
        // (Measured: doing this the other way round segfaults.)
        if args.is_empty()
            && matches!(method_name, "REPR" | "WHERE")
            && let Some(v) = self.try_native_handle_repr_where(target, method_name)
        {
            return Some(Ok(v));
        }
        // `.Capture` that must call user methods / drain a live source (Channel/
        // Supply, or a non-Str Pair key needing `.Str`) is interpreter-aware; other
        // targets fall through to the pure native `value_to_capture` below.
        if method_name == "Capture"
            && args.is_empty()
            && let Some(r) = self.try_interpreter_capture(target)
        {
            return Some(r);
        }
        // Eager list operations cannot run on a lazy/infinite source: throw
        // X::Cannot::Lazy instead of hanging while the native impl materializes
        // it (matches raku).
        if let Some(err) = crate::runtime::Interpreter::lazy_guard_error(method_name, target) {
            return Some(Err(err));
        }
        // A declaration of the program takes the call from the built-in row:
        // a user method or accessor (`.WHICH`/`.WHY` included: they are
        // ordinary, overridable methods), a role mixed in, an `augment` or
        // `.wrap` of a built-in type. The one shadow query (ADR-11276 §2.3).
        if self.user_shadows_builtin(target, method_sym, args, false) {
            return None;
        }
        if let Some(value) = self.inherited_adhoc_method(target, method_name, args) {
            return Some(Ok(value));
        }
        // The receiver guard the interpreter's entry runs too: an
        // interpreter-side handler, a computed exception message, a `Cool`
        // name the receiver lacks.
        if self.native_receiver_guard(target, method_name) {
            return None;
        }
        // `Range.int-bounds($from is rw, $to is rw --> Bool)`: writes the bounds
        // into the caller's containers, so it needs `&mut self` and the call
        // site's arg-source names (see `vm/vm_range_int_bounds.rs`). The
        // zero-argument candidate stays in the pure arity cascade below.
        if Self::is_range_int_bounds_rw(target, method_name, args) {
            return Some(self.range_int_bounds_rw(target));
        }
        // A named argument the method does not accept must not be counted as a
        // positional: the cascade below picks its arm by `args.len()` and then
        // indexes `args[0]`/`args[1]`, so an undeclared `Pair` would be numified
        // or consumed as data (`"abc".chop(:zzz)` -> a 0 character count). Every
        // native that genuinely reads an adverb is either lifted out as an
        // interceptor above (`contains` / `starts-with` / `ends-with` /
        // `substr-eq`) or declared on the method's rows; a name no row binds is
        // dropped (`method_table::implicit_named`).
        let stripped = crate::builtins::strip_undeclared_nameds(method_name, args);
        let args: &[Value] = stripped.as_deref().unwrap_or(args);
        if args.len() == 2 {
            // No cascade takes two arguments: a row with no shape may.
            crate::builtins::method_table::answer_shapeless(Some(self), target, method_sym, args)
        } else if args.len() == 1 {
            crate::builtins::native_method_1arg_without_table(target, method_sym, &args[0]).or_else(
                || {
                    crate::builtins::method_table::answer_shapeless(
                        Some(self),
                        target,
                        method_sym,
                        &args[..1],
                    )
                },
            )
        } else if args.is_empty() {
            // The cascade without the method table: `try_native_method` asked
            // the table for this very receiver and method before calling here,
            // and a row it would answer from but the lever-A gate above refused
            // must not answer here either.
            crate::builtins::methods_0arg::native_method_0arg_cascade(target, method_sym).or_else(
                || {
                    crate::builtins::method_table::answer_shapeless(
                        Some(self),
                        target,
                        method_sym,
                        &[],
                    )
                },
            )
        } else {
            None
        }
    }

    pub(super) fn try_native_function(
        &mut self,
        name_sym: crate::symbol::Symbol,
        args: &mut Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        // A plain gather argument is read the way Rakudo's routine reads it:
        // streamed, consumed or cached (ADR-11678 §8). The read is written
        // back, so a caller that falls through to another dispatch path after
        // a `None` passes on the stolen or pulled argument rather than the
        // consumed Seq.
        if Self::has_builtin_seq_read(args) {
            match self.builtin_gather_args(name_sym.as_str(), args) {
                Ok(read) => *args = read.into_owned(),
                Err(e) => return Some(Err(e)),
            }
        }
        if let Some(leaves) = Self::shaped_routine_args(name_sym.as_str(), args) {
            *args = leaves;
        }
        let args: &[Value] = args;
        // Atomic var/element RMW markers (`__mutsu_atomic_*` / `__mutsu_cas_*`) for the
        // `⚛`-operators. Dispatch them here directly so they don't fall through to the
        // generic `call_function` name-match (§D state ownership). Checked before the
        // Instance-arg bail below because a cas/store value may legitimately be an
        // Instance (e.g. `cas($x, $old, $obj)`).
        {
            let name: &str = name_sym.as_str();
            if (name.starts_with("__mutsu_atomic_") || name.starts_with("__mutsu_cas_"))
                && let Some(result) = self.try_native_atomic_function(name, args)
            {
                return Some(result);
            }
            // The FETCH/STORE of a native positional reference (`IntPosRef`,
            // #11209): its first argument is the reference itself.
            if crate::runtime::native_pos_ref::is_native_pos_ref_routine(name) {
                return self.try_native_pos_ref_routine(name, args);
            }
            // The same for a native lexical reference (`IntLexRef`, #11498).
            if let Some(result) = self.try_native_lex_ref_routine(name, args) {
                return Some(result);
            }
        }
        // #9012: `floor`/`ceiling` as FREE FUNCTIONS return `Int` for a boxed
        // `Num` argument (a literal, a computed value, or a `Num`-typed
        // variable) but must stay `Num` for a genuinely native
        // `num`/`num32`/`num64` argument -- rakudo distinguishes the two by
        // the argument's declared/static nativity, which mutsu has no
        // `Value`-level tag for (both are the same `ValueView::Num(f64)`).
        // `dispatch_1arg.rs`'s pure implementation cannot see that distinction
        // at all, so it always keeps `Num` -- matching only the native case.
        // See `try_native_floor_ceiling` for the compile-time signal
        // (`literal_native_args`'s call-specific meaning for these three
        // names) that makes the distinction here.
        // `zip` over an unbounded column streams its rows instead of letting
        // the pure native `zip` materialize the column (#9159).
        if name_sym.as_str() == "zip" {
            match self.builtin_zip_unbounded(args) {
                Ok(Some(result)) => return Some(Ok(result)),
                Ok(None) => {}
                Err(e) => return Some(Err(e)),
            }
        }
        if let Some(result) = self.try_native_floor_ceiling(name_sym, args) {
            return Some(result);
        }
        // The sub forms of `Cool`'s string methods are the method on their
        // subject argument in raku (`substr($s, 0, 2)` is `$s.substr(0, 2)`),
        // so an undefined subject gets the method form's answer (#9772).
        if let Some(result) = self.type_object_string_sub_gate(name_sym.as_str(), args) {
            return Some(result);
        }
        // ADR-0058: `crate::builtins::native_function` is pure Rust that reads
        // its arguments' elements directly, so a still-deferred `.map` Seq has
        // to be pulled first (otherwise `join`/`say`/... read its empty seed).
        // No-op for every other value and every other `SeqSource`.
        //
        // Exception: rakudo's `list` LISTOP on a single `Seq` is a pure no-op —
        // `(list $s) =:= $s`, and `builtin_list` hands the very same value back
        // without ever reading an element. Reifying it here buys nothing and
        // costs correctness: `reify` is the one thing that marks a body
        // `retained`, and a retained body is exempt from every later CONSUMING
        // touch. So a single `list s` permanently disarmed the Seq's one-shot
        // gate — `my \s = (list 1..5).grep(* > 0); list s; s>>.abs; s>>.abs`
        // stopped throwing `X::Seq::Consumed`, which raku does throw (roast
        // S03-operators/context-forcers.t, "list listop doesn't cache"). With
        // more than one argument `list` genuinely flattens and does need the
        // elements, so the exemption is exactly the one-Seq-argument shape.
        let list_is_noop =
            args.len() == 1 && args[0].is_seq_value() && name_sym.with_str(|s| s == "list");
        if !list_is_noop && let Err(e) = self.reify_map_grep_seq_args(args) {
            return Some(Err(e));
        }
        if let Some(result) = self.try_native_attribute_trait(name_sym.resolve().as_str(), args) {
            return Some(result);
        }
        if args
            .iter()
            .any(|arg| matches!(arg.view(), ValueView::Instance { .. }))
        {
            // Junction constructors wrap their arguments verbatim regardless of
            // type, so an Instance argument is safe to handle natively; every
            // other native function bails out on Instance args (they may need
            // method dispatch) and falls through to the interpreter.
            let name: &str = name_sym.as_str();
            // `unpack(Blob, Str)` takes a Blob (Instance) argument and is
            // handled natively; everything else bails out on Instance args.
            if !matches!(name, "any" | "all" | "one" | "none" | "unpack") {
                return None;
            }
        }
        // Map(...) is the callable form of the immutable map constructor.
        // Keep it on the VM-native path beside Map.new(...) so lazy map
        // results are materialized once and callers need not spell the
        // constructor differently.
        if name_sym.with_str(|name| name == "Map")
            && !self
                .types
                .user_declared_classes
                .contains(&name_sym.resolve())
        {
            return Some(self.builtin_map_coerce(args));
        }
        // For functions that need to iterate their arguments (e.g. flat),
        // force any LazyList arguments first so the pure builtin can process them.
        if args
            .iter()
            .any(|a| matches!(a.view(), ValueView::LazyList(_)))
        {
            let name = name_sym.resolve();
            if name == "reverse"
                && args.len() == 1
                && let ValueView::LazyList(ll) = args[0].view()
                && !ll.is_genuinely_lazy()
            {
                return Some(self.force_lazy_list_vm(&ll).map(|mut items| {
                    items.reverse();
                    Value::seq(items)
                }));
            }
            if matches!(name.as_str(), "any" | "all" | "one" | "none") {
                // Junction constructors need concrete eigenstates. In
                // particular, a finite gather/map/grep pipeline must be
                // reified while the RHS is still being evaluated: otherwise
                // an assignment such as `$j = any (gather $j».take).grep(...)`
                // stores the pipeline itself as an eigenstate, and forcing it
                // later observes the newly assigned (self-referential)
                // junction and recurses forever. Leave genuinely infinite
                // lazy sources untouched rather than attempting to exhaust
                // them here.
                let mut forced_args = Vec::with_capacity(args.len());
                for arg in args {
                    if let ValueView::LazyList(ll) = arg.view()
                        && ll.is_from_gather()
                        && !ll.is_lazy_infinite()
                    {
                        match self.force_lazy_list_vm(&ll) {
                            Ok(items) => forced_args.push(Value::seq(items)),
                            Err(e) => return Some(Err(e)),
                        }
                    } else {
                        forced_args.push(arg.clone());
                    }
                }
                return crate::builtins::native_function(name_sym, &forced_args);
            }
            if matches!(name.as_str(), "flat" | "eager") {
                // For `flat`, a single lazy argument stays lazy (`flat 42 xx *`
                // / `flat 10,11 ... *` propagate `.is-lazy`). The one exception
                // is a `gather` coroutine: `flat` must descend into its nested
                // lazy elements (`take internals($child)` in a binary-tree
                // walk), which a raw return would leave un-descended
                // (`('A', Seq(...), ...)`).
                // A plain gather streamed as a pipe over its Seq (`flat(gather
                // {...})`, the §8.2 Stream class) is flattened as it is pulled
                // instead, so
                // an infinite one works; the result is a Seq, as in Rakudo.
                if name.as_str() == "flat"
                    && let [arg] = args
                    && let ValueView::LazyList(ll) = arg.view()
                    && let Some(seq) = ll.seq_stream_source()
                {
                    return Some(Ok(Self::lazy_flat_gather(seq)));
                }
                let is_finite_gather = |v: &Value| {
                    matches!(v.view(), ValueView::LazyList(ll)
                        if ll.coroutine.is_some() && ll.cache.lock().unwrap().is_none())
                };
                if name.as_str() == "flat"
                    && args.len() == 1
                    && let ValueView::LazyList(ll) = args[0].view()
                    && !is_finite_gather(&args[0])
                {
                    // A genuinely-lazy list may hold nested arrays
                    // (`flat [2,3,4], 10, 11 ... *`), so wrap it in a lazy
                    // flattening pipe that pulls source elements on demand and
                    // spills each through `flat_val` — flattened AND still lazy.
                    if ll.is_genuinely_lazy() {
                        return Some(Ok(Value::lazy_list(crate::gc::Gc::new(
                            crate::value::LazyList::new_index_pipe(
                                args[0].clone(),
                                crate::value::IndexTransform::Flat,
                            ),
                        ))));
                    }
                    return Some(Ok(args[0].clone()));
                }
                let mut forced_args = Vec::with_capacity(args.len());
                for arg in args {
                    if let ValueView::LazyList(ll) = arg.view() {
                        match self.force_lazy_list_vm(&ll) {
                            Ok(items) => {
                                forced_args.push(Value::seq(items));
                            }
                            Err(e) => return Some(Err(e)),
                        }
                    } else {
                        forced_args.push(arg.clone());
                    }
                }
                return crate::builtins::native_function(name_sym, &forced_args);
            }
        }
        crate::builtins::native_function(name_sym, args)
    }

    /// The `floor`/`ceiling` free-function special case documented at its
    /// call site in [`Self::try_native_function`]. `None` for every other
    /// name/shape, so the caller falls through to the ordinary native table
    /// (which still owns the NaN/Infinite/Int/Rat/Complex/Instance arms, and
    /// the native `Num` case: a declared-native argument, `bit 0` set,
    /// returns `None` here and reaches that table's `Value::num(...)` arm
    /// unchanged).
    ///
    /// `self.literal_native_args` is normally a per-position "was this
    /// argument written as a literal" mask for multi-dispatch ranking
    /// (`unwrap_varref_for_dispatch_at`); the compiler's
    /// `literal_native_args_mask` special-cases exactly these three
    /// (single-argument) callee names to carry a different bit instead --
    /// whether the sole argument's call-site SHAPE is a genuinely declared
    /// native scalar (see `Compiler::is_declared_native_arg`). A plain
    /// variable argument is unwrapped to its bare value well before this
    /// point (`normalize_call_args_for_target` strips the `VarRef` tag for
    /// any name with no user-declared candidate, builtins included), so the
    /// mask -- published for the whole call by `exec_call_func_op` -- is the
    /// only surviving signal.
    fn try_native_floor_ceiling(
        &self,
        name_sym: crate::symbol::Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let name = name_sym.as_str();
        if !matches!(name, "floor" | "ceiling" | "ceil") || args.len() != 1 {
            return None;
        }
        let ValueView::Num(f) = args[0].unwrap_varref().view() else {
            return None;
        };
        if f.is_nan() || f.is_infinite() {
            return None;
        }
        let is_declared_native = self.literal_native_args & 1 != 0;
        if is_declared_native {
            return None;
        }
        let rounded = if name == "floor" { f.floor() } else { f.ceil() };
        Some(Ok(Value::int(rounded as i64)))
    }
}
