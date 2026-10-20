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
            |vm| {
                !vm.native_lever_a_user_override_sym(target, method_sym)
                    && !crate::builtins::method_table::early_entry_defers(
                        vm,
                        target,
                        method_sym.as_str(),
                    )
            },
            target,
            method_sym,
            args,
        ) {
            return Some(self.settle_native_warning(result));
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
        use crate::runtime::utils::collection_contains_instance;
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
        // A wrapper role's `gist` row (ADR-11276, #11276), found by the role
        // the exception carries, ahead of the native exception-gist arm below
        // that would answer the bare cause; as in `call_method_with_values`.
        if let Some(owners) = crate::builtins::method_table::wrapper_role_owners_of(target)
            && let Some(result) =
                crate::builtins::method_table::invoke_owner(self, owners, method_name, args, || {
                    target.clone()
                })
        {
            return Some(result);
        }
        // A user instance with its own `iterator` method routes the Any
        // iteration methods through that iterator (the arm in
        // `call_method_with_values`), whether or not it also composes
        // `Iterable` (#8547); a native impl (e.g. `flat`) would treat the
        // instance as one opaque item instead, so decline it here.
        if matches!(
            method_name,
            "grep" | "map" | "first" | "sort" | "head" | "tail" | "flat"
        ) {
            // An `IterationBuffer` likewise answers these from its elements
            // (`call_method_with_values`); the native impl would see one item.
            if crate::runtime::nqp_ops_list::is_iteration_buffer(target) {
                return None;
            }
            let cn = match target.view() {
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if !attributes.contains_key("__mutsu_array_storage")
                    && !attributes.contains_key("__mutsu_hash_storage") =>
                {
                    Some(class_name.as_str().to_string())
                }
                _ => None,
            };
            if let Some(cn) = cn
                && self.has_user_method(&cn, "iterator")
                && !self.has_user_method(&cn, method_name)
            {
                return None;
            }
            // The same for a role mixed into a native container (`my @a is R`):
            // its elements live behind the role's `iterator`, which the native
            // impl cannot see.
            if matches!(target.view(), ValueView::Mixin(..))
                && self.mixin_composes_method(target, "iterator")
                && !self.mixin_composes_method(target, method_name)
            {
                return None;
            }
        }
        // A `Seq.new($iterator)`/`IO::Handle.lines` body whose source has not
        // been pulled yet (ADR-0034 §2.3) reads as an empty `Vec` through the
        // native impls below (`.List`/`.elems`/`.Array`/...). Defer such a
        // Seq to the interpreter, whose `call_method_with_values` reifies or
        // consumes it (via `reify_seq_body`/`take_seq_body`) before
        // dispatching. `cache`/`raku`/`perl`/`sink`, and every method that
        // answers from type/identity alone (`seq_method_never_touches`),
        // intentionally must NOT pull, so they proceed with the native path.
        // (The Seq probe below is tag-gated: `view()` on a lazy Match target
        // would materialize it per dispatched method call.)
        if target.is_seq_value()
            && let ValueView::Seq(body) = target.view()
            && body.needs_touch()
            && !matches!(method_name, "cache" | "raku" | "sink")
            && !crate::value::seq_method_never_touches(method_name)
        {
            return None;
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
        // A `.map`/`.grep` on a lazy source that the interpreter would turn into
        // a lazy pipeline stage (a chained map/grep pipe, an infinite sequence, or
        // a genuinely-lazy `gather { … }.lazy`) must be deferred to the interpreter
        // (`dispatch_map_method`/`dispatch_grep` via `is_lazy_pipe_source`). Running
        // the native impl here would materialize the source eagerly — forcing the
        // whole gather body (and its trailing side effects) instead of pulling on
        // demand. Defer.
        if target.is_lazy_list_value()
            && Self::is_lazy_pipe_source(target)
            && matches!(method_name, "map" | "grep")
        {
            return None;
        }
        // A laziness-preserving coercion on a lazy map/grep pipeline returns the
        // pipeline unchanged (it stays pullable). Intercept before the native
        // impl, which would otherwise wrap/empty the pipeline's (empty) cache.
        if target.is_lazy_list_value()
            && let ValueView::LazyList(ll) = target.view()
            && ll.lazy_pipe.is_some()
            && !ll.coercion_keeps_pipe_lazy()
            && method_name == "cache"
        {
            // `.cache` is a strict Seq-to-List coercion. Keeping a finite pipe
            // as a Seq makes Test.rakumod's `is-deeply(Seq, ...)` overload call
            // itself forever through `$got.cache`.
            return Some(self.force_lazy_list_vm(&ll).map(Value::array));
        }
        if target.is_lazy_list_value()
            && let ValueView::LazyList(ll) = target.view()
            && ll.lazy_pipe.is_some()
            && Self::lazy_pipe_preserving_coercion(method_name)
            // A pipe with a finite source must reify for strict coercions such
            // as `.cache`. In particular, Test.rakumod's Seq `is-deeply`
            // candidate recursively calls `.cache`; returning the same finite
            // Seq here reselects that candidate forever. A `lazy`-marked pipe
            // stays lazy even when finite, as in Rakudo.
            && ll.coercion_keeps_pipe_lazy()
        {
            // The pipeline stays pullable, but `.List`/`.Array`/`.cache` change
            // the reported type immediately (Rakudo: type changes, laziness
            // does not) — tag the context marker the same way `.^name`/`.WHAT`
            // read it. `.Seq`/`.lazy` leave the value unchanged.
            let retagged = match method_name {
                "Array" => Value::lazy_list(crate::gc::Gc::new(ll.with_array_context())),
                "List" | "list" | "values" => {
                    Value::lazy_list(crate::gc::Gc::new(ll.with_list_context()))
                }
                "cache" => ll.cache_lazy_view().unwrap_or_else(|| target.clone()),
                _ => target.clone(),
            };
            return Some(Ok(retagged));
        }
        // Eager list operations cannot run on a lazy/infinite source: throw
        // X::Cannot::Lazy instead of hanging while the native impl materializes
        // it (matches raku). Shared with the interpreter dispatch path.
        if let Some(err) = crate::runtime::Interpreter::lazy_guard_error(method_name, target) {
            return Some(Err(err));
        }
        // Early exit for Proxy containers
        if target.is_proxy_value()
            && !matches!(
                method_name,
                "VAR" | "WHAT" | "WHICH" | "WHERE" | "HOW" | "WHY" | "REPR" | "DEFINITE"
            )
        {
            return None;
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
        // A `Supply` (instance or type object) answers its methods through
        // its own rows and dispatcher, which need the interpreter (a supply
        // materializes, taps or schedules); the pure cascades would read it as
        // one item (`Supply.batch` on the type object, ADR-11276 §9.168).
        if target
            .receiver_shape()
            .or_else(|| target.type_object_shape())
            == Some(crate::value::DispatchShape::Supply)
        {
            return None;
        }
        // Lazy Match: class is statically "Match" — mirror the Instance
        // bypasses below without materializing. (A Match never matches
        // Real/Numeric, and the Supply/Supplier arms cannot apply.)
        if target.is_lazy_match_value() {
            // A grammar cursor's dispatch owner is the grammar's own class, not
            // `Match` — as `native_receiver_defers` reads it.
            let owner = target.match_dispatch_class();
            let is_pure_render = matches!(method_name, "gist" | "Str" | "Stringy" | "raku");
            let render_overridden = is_pure_render && self.has_user_method(owner, method_name);
            if (!is_pure_render || render_overridden) && self.has_user_method(owner, "Bridge") {
                return None;
            }
        }
        // Instance-specific bypasses (avoid for non-Instance targets)
        else if let ValueView::Instance { class_name, .. } = target.view() {
            let cn = class_name.resolve();
            // Numeric bridge — Real/Numeric instances route through the
            // interpreter's Numeric bridge for arithmetic and coercion.
            // Exception: pure-render methods (gist/Str/raku/...) don't need
            // the bridge, so let `native_method_0arg` render the known
            // builtin numeric instances (Instant/Duration) directly. A
            // user numeric class that overrides one of these keeps the
            // bypass so its method runs; unknown numeric instances return
            // `None` from native and still fall through to the interpreter.
            let is_pure_render = matches!(method_name, "gist" | "Str" | "Stringy" | "raku");
            let render_overridden = is_pure_render && self.has_user_method(&cn, method_name);
            // The probe (`~~ Real`, `~~ Numeric`, own `Bridge` method)
            // is a property of the receiver's CLASS, not of the call, so
            // it is memoized per class symbol. Asked inline it made two
            // full `type_matches_value` walks -- each over the class MRO
            // *and* the transitive closure of its composed roles -- 52% of
            // a `Buf.push` loop. The memo is keyed on the registry write
            // generation, bumped by the registry's sole write path, so no
            // enumeration of mutation sites can go stale (#7712).
            if (!is_pure_render || render_overridden)
                && self.instance_needs_numeric_bridge(class_name, target)
            {
                return None;
            }
        }
        if let Some(result) = self.try_core_type_object_method(target, method_name, args) {
            return Some(result);
        }
        // Collection gist bypass
        if method_sym == "gist" && args.is_empty() && collection_contains_instance(target) {
            return None;
        }
        // Collection .raku/.perl bypass: an element whose `.raku` is only
        // reachable through method dispatch (a user instance, a built-in object
        // type) must be rendered by the interpreter — the pure renderer would
        // fall back to `Foo()` (see `runtime::methods_raku_dispatch`).
        if matches!(method_name, "raku")
            && args.is_empty()
            && crate::runtime::container_needs_raku_dispatch(target)
        {
            return None;
        }
        // `pick`/`roll` on an enum type object (or its name as a `Str`) sample
        // its values, which only the interpreter's enum registry knows
        // (`dispatch_enum_pick_roll`); any other type object reaches the
        // `Any.pick`/`Any.roll` rows and samples itself.
        if (method_sym == "pick" || method_sym == "roll")
            && args.len() <= 1
            && match target.view() {
                ValueView::Package(name) => self.registry().enum_types.contains_key(name.as_str()),
                ValueView::Str(name) => self.registry().enum_types.contains_key(name.as_str()),
                _ => false,
            }
        {
            return None;
        }
        // Hash-specific bypasses (lazy-Match gate: `view()` would materialize)
        if !target.is_lazy_match_value()
            && matches!(target.view(), ValueView::Hash(_))
            && args.is_empty()
        {
            let mn = method_name;
            if (mn == "raku") && self.container_type_metadata(target).is_some() {
                return None;
            }
        }
        // Typed array .raku/.perl bypass (method-name gate first: the `view()`
        // probe would materialize a lazy Match on every other method call)
        if matches!(method_name, "raku")
            && args.is_empty()
            && matches!(target.view(), ValueView::Array(..))
            && self
                .container_type_metadata(target)
                .is_some_and(|info| info.value_type != "Any" && info.value_type != "Mu")
        {
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
        // The pure `unique`/`repeated`/`squish` compare elements by `.WHICH`
        // and cannot run a user `WHICH`, so deposit it first
        // (see `runtime::which_identity`).
        if args.is_empty() && matches!(method_name, "unique" | "repeated" | "squish") {
            self.warm_which_identity(target);
        }
        let mut result = if args.len() == 2 {
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
            return None;
        };

        // For Hash values with declared_type "Map", override gist/raku/perl
        // to use Map.new((...)) format instead of {...} format. (Method-name
        // gate first: the Hash `view()` probe would materialize a lazy Match
        // on every other method call.)
        if matches!(method_name, "gist" | "raku")
            && let ValueView::Hash(map) = target.view()
        {
            let is_map = self
                .container_type_metadata(target)
                .and_then(|info| info.declared_type)
                .is_some_and(|dt| dt == "Map");
            if is_map {
                if map.is_empty() {
                    // `.gist` keeps the empty argument list (`Map.new(())`);
                    // `.raku` drops it (`Map.new`).
                    let text = if method_name == "gist" {
                        "Map.new(())"
                    } else {
                        "Map.new"
                    };
                    return Some(Ok(Value::str(text.to_string())));
                }
                let mut sorted_keys: Vec<&String> = map.keys().collect();
                sorted_keys.sort();
                if method_name == "gist" {
                    let total = sorted_keys.len();
                    let truncated = total > 100;
                    let display_keys = if truncated {
                        &sorted_keys[..100]
                    } else {
                        &sorted_keys[..]
                    };
                    let mut parts: Vec<String> = display_keys
                        .iter()
                        .map(|k| format!("{} => {}", k, crate::runtime::gist_value(&map[*k])))
                        .collect();
                    if truncated {
                        parts.push("...".to_string());
                    }
                    return Some(Ok(Value::str(format!("Map.new(({}))", parts.join(", ")))));
                } else {
                    let parts: Vec<String> = sorted_keys
                        .iter()
                        .map(|k| {
                            let v = &map[*k];
                            if let ValueView::Bool(true) = v.view() {
                                format!(":{}", k)
                            } else if let ValueView::Bool(false) = v.view() {
                                format!(":!{}", k)
                            } else {
                                let repr = if matches!(v.view(), ValueView::Nil) {
                                    "Any".to_string()
                                } else {
                                    crate::builtins::methods_0arg::raku_value(v)
                                };
                                format!(":{}({})", k, repr)
                            }
                        })
                        .collect();
                    return Some(Ok(Value::str(
                        crate::builtins::methods_0arg::raku_repr::raku_map_new(&parts),
                    )));
                }
            }
        }

        // When cloning a container (Hash/Array/Set/Bag/Mix) that has type
        // metadata, copy the metadata to the cloned value so the clone
        // preserves the type constraint (e.g. %h.clone ~~ Hash[Int,Int]).
        if method_name == "clone"
            && matches!(&result, Some(Ok(_)))
            && let Some(info) = self.container_type_metadata(target)
            && let Some(Ok(v)) = result.take()
        {
            // Hashes embed metadata in `HashData`; re-tag the cloned value
            // (no-op Arc for array/instance side-table containers).
            result = Some(Ok(self.tag_container_metadata(v, info)));
        }

        result
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
