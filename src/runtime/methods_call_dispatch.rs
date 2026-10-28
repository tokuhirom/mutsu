use super::methods_signature_errors::make_multi_no_match_error;
use super::*;
use crate::builtins::method_table::receiver_step::{Step, Why};
use crate::meta_ns::MetaNs;
use crate::symbol::Symbol;
use crate::value::ValueView;

impl Interpreter {
    /// Build a `HyperConfiguration` instance carrying `.batch`/`.degree`. A
    /// `None` reports the core default (batch 64; degree = available CPU cores).
    pub(crate) fn make_hyper_configuration(batch: Option<i64>, degree: Option<i64>) -> Value {
        let default_degree = std::thread::available_parallelism()
            .map(|n| n.get() as i64)
            .unwrap_or(4);
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("batch".to_string(), Value::int(batch.unwrap_or(64)));
        attrs.insert(
            "degree".to_string(),
            Value::int(degree.unwrap_or(default_degree)),
        );
        Value::make_instance(crate::symbol::Symbol::intern("HyperConfiguration"), attrs)
    }

    /// Whether `err` is the plain (non-private) `X::Method::NotFound` raised for
    /// `method` itself — i.e. the whole dispatch chain found no candidate at all,
    /// as opposed to a method that *was* found and threw something of its own.
    fn is_method_not_found_for(err: &RuntimeError, method: &str) -> bool {
        let Some(exc) = err.exception.as_deref() else {
            return false;
        };
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = exc.view()
        else {
            return false;
        };
        if class_name.resolve() != "X::Method::NotFound" {
            return false;
        }
        let map = attributes.as_map();
        if map.get("private").is_some_and(|v| v.truthy()) {
            return false;
        }
        map.get("method")
            .is_some_and(|v| v.to_string_value() == method)
    }

    /// Method dispatch entry point, wrapping [`Self::call_method_with_values_inner`]
    /// with Raku's implicit `*%_`.
    ///
    /// Every Raku *method* (never a sub) carries an implicit `*%_` slurpy named
    /// parameter, so a named argument the method does not declare is simply
    /// swallowed: `4.log(:base(2))` is `4.log` (`$base` is *positional*, so the
    /// colonpair binds nothing) and `"abc".uc(:foo)` is `"abc".uc`. mutsu's native
    /// methods are dispatched by *arity* instead of by signature, and a named
    /// argument occupies a positional slot in that count, so the lookup missed
    /// entirely and the call died with `X::Method::NotFound` (plus a bogus
    /// "Did you mean ...?").
    ///
    /// The nameds cannot simply be stripped up front: a native method that *does*
    /// read an adverb (`.split(:skip-empty)`, `.substr-eq(:i)`, `.comb(:match)`,
    /// `.subst(:g)`, ...) reads it straight out of that same argument list, and
    /// which names each one consumes is implicit in its Rust body rather than
    /// declared anywhere. So the full argument list is offered first — every call
    /// that works today keeps working, byte for byte — and only when the dispatch
    /// chain reports "no such method" (meaning nothing understood this argument
    /// list) is it retried with the named arguments removed. That retry cannot
    /// turn a wrong-arity call into a silent success: `"abc".uc("x")` and
    /// `4.log(1,2,3)` carry no nameds, so they never take this path.
    ///
    /// Named-ness is a call-site property (ADR-0021): only the `Pair` flavour is a
    /// named argument, so a positional `Pair` (`%h.push((a => 1))`) is untouched.
    pub(crate) fn call_method_with_values(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // A routine receiver answers with the roles composed into the routine
        // so far, whichever alias of it this is (ADR-11827).
        let target = Self::routine_current_view(&target).unwrap_or(target);
        // A lazy-tail Slip answers as the lazy list it reifies from (#13117).
        let target = Self::lazy_slip_receiver(target, method);
        let target = Self::shaped_iteration_receiver(target, method);
        // `Mu.perl` is `self.raku` (ADR-11276 §9.152).
        let method = if method == "perl" {
            self.perl_as_raku(&target, crate::symbol::wk::perl())
                .as_str()
        } else {
            method
        };
        self.call_method_with_values_unviewed(target, method, args)
    }

    /// [`Self::call_method_with_values`] on `target` as given: what a
    /// Mixin's dispatch uses to reach its own inner value, which must not be
    /// re-wrapped in the routine's composition (ADR-11827).
    pub(crate) fn call_method_with_values_unviewed(
        &mut self,
        target: Value,
        method: &str,
        mut args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        self.coerce_assoc_key_arg(&target, method, &mut args)?;
        // Tagging the whole call, body included, is deliberate and costs
        // nothing in accuracy: a tick that fires while the method's *bytecode*
        // runs is consumed by that bytecode's own poll long before this guard
        // drops, so only the dispatch work itself can still be holding one
        // (`crate::profile::region`).
        let _region = crate::profile::enter(crate::profile::Region::MethodDispatch);
        // A `create_type` type object with an installed method cache answers
        // from it (`Metamodel::Primitives.install_method_cache`).
        if let Some(cached) = self.custom_type_cached_method(&target, method) {
            let code = cached?;
            let mut call_args = Vec::with_capacity(args.len() + 1);
            call_args.push(target.clone());
            call_args.extend(args);
            return self.call_sub_value(code, call_args);
        }
        // A user `method ^find_method` answers every call on its type, the
        // internal ones too (`say` asking for `.gist`, `~` for `.Str`):
        // `find_method_intercept`.
        if let Some(result) = self.try_user_find_method_dispatch(&target, method, &args) {
            return result;
        }
        // A `does Sequence` class with its own `iterator` (`vm_sequence_role_delegate.rs`):
        // the interpreter-entry twin of the VM call-site hooks, for `~`, `eq`,
        // interpolation and every other internal `.Str`/`.list` request.
        if let Some(result) = self.try_sequence_role_delegate(
            &target,
            crate::symbol::Symbol::intern(method),
            &args,
        ) {
            return result;
        }

        // A `VarRef` (the value `Variable.var` hands back, e.g. inside a
        // `trait_mod:<is>(Variable:D \v, ...)` body calling `v.var.keyof`) is a
        // transient wrapper around the variable's real value, not a type of
        // its own -- a container is transparent for ordinary method dispatch
        // (the same principle ADR-0064 states for the `.VAR` descriptor).
        // Every OTHER call site that constructs a `VarRef` consumes it
        // directly (argument binding, assignment writeback) without routing
        // back through this dispatcher, so the only method name that must see
        // the wrapper itself here is `VAR`, handled below via `varref_parts`.
        if let ValueView::VarRef { name, value, .. } = target.view()
            && method != "VAR"
        {
            if method == "name" && args.is_empty() {
                return Ok(Value::str(name.resolve().to_string()));
            }
            let inner = value.clone();
            return self.call_method_with_values_unviewed(inner, method, args);
        }
        // `Any` is not a `Cool`, so it does not answer `Cool`'s methods. Gated
        // here, ahead of every native/instance handler, because the by-name
        // native cascades below recognize the NAME and then stringify the
        // receiver -- a type object stringifies to its gist, so
        // `$undefined.comb(/\w+/)` answered `("Any",)` instead of throwing
        // (#7773). See `any_cool_method_gate` for the derivation of the set.
        if let Some(err) = super::any_cool_method_gate::cool_method_not_found(&target, method) {
            return Err(err);
        }
        if super::any_cool_method_gate::fmt_has_no_candidate(&target, method) {
            return Err(make_multi_no_match_error(method));
        }
        // A buffer is not a `Cool` either (#13313): the by-name text
        // dispatchers would stringify it (`Buf.new(1).contains(1)`).
        if let Some(type_name) =
            super::any_cool_method_gate::buffer_refuses_cool_method(&target, method)
            && !self.buffer_class_answers(&target, method)
        {
            return Err(super::methods_signature_errors::make_method_not_found_error(
                method, &type_name, false,
            ));
        }
        if let Some(result) = self.cool_instance_text_via_str(&target, method, &args) {
            return result;
        }
        // The `Cool`-subtype sibling: `Str.comb` / `Int.uc` refuse the
        // undefined invocant, `Str.uc` answers `""` with a warning (#9772).
        if let Some(result) = self.cool_type_object_default_fmt(&target, method, &args) {
            return result;
        }
        if let Some(result) = self.cool_type_object_string_method(&target, method) {
            return result;
        }
        // The NQP cursor protocol (#7883, #7931): building a cursor
        // (`Match.!cursor_init($s, :0c)`, reached through the callable
        // `Match.^lookup("!cursor_init")` hands back), advancing one
        // (`CURSOR_MORE`), and *calling a regex on* one (`$needle($cursor)`,
        // which arrives here as `Regex.CALL-ME`). Gated on the protocol's own
        // name list so there is a single source of truth; `&str` equality
        // compares lengths first, so the common dispatch pays two length
        // checks and nothing else.
        if super::regex::regex_cursor::is_cursor_protocol_method(method)
            && let Some(result) = self.try_cursor_protocol_method(&target, method, &args)
        {
            return result;
        }
        if method == "CALL-ME"
            && let Some(result) = self.try_call_regex_on_cursor(&target, &args)
        {
            return result;
        }
        // A `Mixin` reaches the rows of the roles mixed into it ahead of its
        // inner value's (ADR-11276, #11276): `X::Promise::Broken`,
        // `X::React::Died` and `X::Await::Died`, composed into the exception
        // `Promise.result`, a dying `react` or `await` rethrows, answer
        // `gist` before the native `.gist` arms downstream that would answer
        // the bare exception.
        if let Some(owners) = crate::builtins::method_table::mixin_role_owners_of(&target, method)
            && let Some(result) = crate::builtins::method_table::invoke_owner(
                self,
                &owners.iter().map(String::as_str).collect::<Vec<_>>(),
                method,
                &args,
                || target.clone(),
            )
        {
            return result;
        }
        // `.WHICH` of an aggregate (`Pair`, `Set`, `Bag`, `Mix`) is composed
        // from its elements' identities by the interpreter-free
        // `value_which_key`, so resolve any user-defined element `WHICH` first.
        // This is also the refresh point for an instance whose attributes
        // changed since its identity was last computed
        // (see `runtime::which_identity`).
        if method == "WHICH" && args.is_empty() {
            self.warm_which_identity(&target);
        }
        // ADR-0058: a slurpy parameter FLATTENS a `Seq` argument, and the
        // binder reads its elements through pure code -- so a still-deferred
        // `.map`/`.grep` argument has to run its callback before binding
        // (zef's `self!depends2specs($deps)`, where `$deps` is a
        // `.grep(...).map(...)` chain and the callee takes `*@depends`).
        // The RECEIVER is handled separately, by
        // `reify_or_consume_seq_target`.
        self.reify_map_grep_seq_args(&args)?;
        // A plain gather argument of a built-in type's method is read the way
        // Rakudo's method reads it (ADR-11678 §8.3).
        let args = self.method_gather_args(&target, method, args)?;
        // A user `IO::Handle` subclass that overrides `WRITE`/`READ`/`EOF` gets
        // the high-level text methods for free by routing them through those
        // overrides (`try_user_io_handle_method`). That hook was wired into the
        // two OPCODE dispatch paths only, so an INTERNAL dispatch reached the
        // native `IO::Handle` arm instead and failed for want of a real file
        // descriptor. The visible symptom was the documented `$*OUT = $store`
        // idiom (`Type/IO/Handle.rakudoc`, "Creating Custom Handles") printing
        // to the real stdout and capturing nothing:
        // `write_to_named_handle` calls `.print` on the handle through here,
        // that call errored, and the error was swallowed by its own fallback to
        // `emit_output`. `$store.print(...)` written directly in Raku worked all
        // along, which is what made the two paths' disagreement invisible.
        if let Some(result) = self.try_user_io_handle_method(&target, method, &args) {
            return result;
        }
        // ADR-0068 §4 step 3: a MUTATING METHOD on a container that two threads
        // can reach -- `$obj.attr.push($v)` is the measured shape, 95/96 runs
        // corrupt -- structurally mutates the backing node with nothing held.
        // The element-store routes are excluded at their own funnels; this is
        // the funnel for the method form, which arrives here as an ordinary
        // value dispatch (verified by breakpoint: `exec_call_method_op` ->
        // `call_method_with_values`, reaching none of the store paths).
        // Keyed on the receiver's own node, which is what two threads holding
        // the same attribute container share. A no-op (one relaxed load) until
        // a VM mutator thread is spawned.
        let _mutating_guard = if crate::value::container_lock::multi_mutator_threads_live()
            && (crate::value::container_read::is_container_mutator(method)
                || crate::value::container_read::is_leaf_structure_read(method))
        {
            crate::value::container_lock::ContainerStructGuard::acquire_for(None, &target)
        } else {
            None
        };
        // #12456: a method that runs user code over the elements cannot hold the
        // stripe, so it runs on a snapshot taken under it (ADR-0068 §16).
        let target =
            crate::value::container_read::snapshot_read_receiver(method, &target).unwrap_or(target);
        if !args.iter().any(|a| a.is_string_pair_value()) {
            return self.call_method_with_values_inner(target, method, args, true);
        }
        let positional: Vec<Value> = args
            .iter()
            .filter(|a| !a.is_string_pair_value())
            .cloned()
            .collect();
        // A `Seq` body is single-use, and the retry below re-enters the whole
        // dispatch chain — so consume/reify it exactly ONCE, here, and tell both
        // attempts not to touch it again. Otherwise the second attempt would
        // steal an already-`Taken` body and throw `X::Seq::Consumed` on what is,
        // to user code, the first call. (Tag probe first: `view()` on a lazy
        // Match would materialize it.)
        let pre_reify = !target.is_lazy_match_value()
            && matches!(target.view(), ValueView::Seq(_))
            && (method != "sink" || args.is_empty());
        if pre_reify {
            self.pull_pos_method_gather_prefix(&target, method, &args)?;
        }
        let (target, reify_seq) = if pre_reify {
            (
                self.reify_or_consume_seq_target_authoritative(target, method)?,
                false,
            )
        } else {
            (target, true)
        };
        // A deferred `Seq.new($iterator).iterator` hands its original user
        // iterator directly to the caller. It is already the result of the
        // requested method; dispatching `iterator` on it again would treat the
        // user iterator as an ordinary one-element value. Materialized Seq
        // iterators also arrive here as an `Iterator` instance, so the same
        // short-circuit preserves both forms.
        if pre_reify && method == "iterator" && matches!(target.view(), ValueView::Instance { .. })
        {
            return Ok(target);
        }
        match self.call_method_with_values_inner(target.clone(), method, args, reify_seq) {
            Err(err) if Self::is_method_not_found_for(&err, method) => {
                self.call_method_with_values_inner(target, method, positional, reify_seq)
            }
            other => other,
        }
    }

    /// Whether `arg` is an object an `IO::Path` segment method (`add`,
    /// `child`, `sibling`) must first stringify through its `Str` (an
    /// `IO::Path` argument stringifies natively).
    // Cost: O(1).
    fn io_path_segment_needs_str(arg: &Value) -> bool {
        let inner = arg.descalarize();
        matches!(
            inner.view(),
            ValueView::Instance { .. } | ValueView::Mixin(..)
        ) && !Self::is_io_path_value(inner)
    }

    /// `.raku` of a `Scalar` container: the `$` sigil of an itemized aggregate
    /// shows, an element whose repr needs method dispatch (`$[Foo.new]`) is
    /// rendered through it, and an itemized lazy sequence renders the prefix
    /// its own `raku` pulls inside `$( )`.
    // Cost: O(n), n = the rendered text.
    fn render_scalar_container_raku(
        &mut self,
        target: &Value,
        inner: &Value,
    ) -> Result<Value, RuntimeError> {
        if let ValueView::LazyList(_) = inner.view() {
            let rendered = self.call_method_with_values(inner.clone(), "raku", Vec::new())?;
            return Ok(Value::str(format!("$({})", rendered.to_string_value())));
        }
        if let Some(rendered) = self.raku_repr_with_dispatch(target) {
            return Ok(Value::str(rendered));
        }
        Ok(Value::str(
            crate::builtins::methods_0arg::raku_repr::raku_value(target),
        ))
    }

    fn call_method_with_values_inner(
        &mut self,
        target: Value,
        method: &str,
        mut args: Vec<Value>,
        reify_seq: bool,
    ) -> Result<Value, RuntimeError> {
        let target_was_seq = target.is_seq_value();
        // ADR-0064: a `.VAR` reflection descriptor is a CONTAINER, and a
        // container is transparent for ordinary method dispatch -- everything
        // that is not a property of the container itself is answered by the
        // value it holds. Intercepted here, ahead of every native/instance
        // handler, because the descriptor is an attribute-only `Instance` that
        // would otherwise answer `.raku`/`.gist`/`.elems`/... out of an empty
        // attribute map (`Scalar.new`, `1`, ...).
        if let Some(result) = self.try_var_meta_delegate(&target, method, &args) {
            return result;
        }
        // `class CM is Str {}` inherits its `.COERCE` from `Str` (or whatever
        // built-in scalar parent it declares): `CM.COERCE(value)` boxes the
        // coerced payload into an instance of `CM`, exactly as the `CM(value)`
        // call-syntax coercion does (`builtins_operators_fallback.rs`). A
        // class of no COERCE of its own has nothing else to answer this
        // with -- built-in parents are not searched by ordinary method
        // resolution -- so this is checked ahead of it, before every other
        // dispatch tier gets a chance to raise X::Method::NotFound. Skipped
        // entirely when the class declares its own `COERCE`, which must win.
        if method == "COERCE"
            && args.len() == 1
            && matches!(
                target.view(),
                ValueView::Package(_) | ValueView::Instance { .. }
            )
        {
            let class_name = self.mop_receiver_owner(&target);
            if !self.class_has_user_method(&class_name, "COERCE")
                && let Some(result) =
                    self.coerce_into_builtin_inheriting_class(&class_name, &args[0])
            {
                return result;
            }
        }
        // A public attribute accessor is a user-visible method and must win
        // over same-named native by-name routines. This matters for role
        // attributes such as `%.index`: the native String `index` dispatcher
        // otherwise sees the role instance first and interprets the accessor
        // read as a zero-argument string search.
        if args.is_empty()
            && let ValueView::Instance { class_name, .. } = target.view()
            && {
                let resolution =
                    self.resolve_user_method_or_accessor(&class_name.resolve(), method);
                let role_accessor = self
                    .registry()
                    .roles
                    .get(&class_name.resolve())
                    .is_some_and(|role| {
                        role.attributes
                            .iter()
                            .any(|attr| attr.is_public && attr.name == method)
                    });
                matches!(
                    resolution,
                    Some(crate::runtime::UserMethodOrAccessor::Accessor)
                ) || role_accessor
            }
        {
            return self.dispatch_instance_and_fallback(target, method, args);
        }
        // ADR-0040 §9.2: a renderer resolves its receiver's `Proxy` elements
        // first. Rakudo renders a container by calling `.gist`/`.Str`/`.raku`
        // ON EACH ELEMENT, and a method call deconts its invocant, so a `Proxy`
        // element renders as its FETCHed value. mutsu's renderers are pure
        // `Value` walkers that format elements inline, with no per-element
        // dispatch to do the deconting — so the FETCH happens here, at the
        // dispatch boundary where the interpreter is still in hand, and the
        // resolved value goes on to the same pure renderer unchanged.
        //
        // This is the mechanism `resolve_list_element_stringifiers` already
        // established for an `Instance` element with a user-defined `.Str`
        // ("resolve the elements in place, then hand the list to the same pure
        // renderer"); a `Proxy` element resolves by FETCH rather than by
        // dispatching `.Str`. Placed with the other "decide the receiver first"
        // guards for the same reason they are: several by-name interceptors
        // below answer for the receiver as it stands.
        //
        // Cost: `value_has_proxy` is an allocation-free scan that stops at the
        // first Proxy, and the render path already runs a scan of exactly this
        // shape (`needs_method_dispatch`) on every `say`.
        let target = Self::gist_receiver(method, target);
        if Self::renders_receiver_elements(method) && Self::holds_nested_proxy(&target) {
            let resolved = self.resolve_proxies_in_value(&target)?;
            return self.call_method_with_values_inner(resolved, method, args, reify_seq);
        }
        // An `IO::Path` joins a segment object by its `Str`, which is pure in
        // the rows: resolve an instance argument to the string it answers
        // first (a `Str` answering an object dies, as in rakudo).
        if matches!(method, "add" | "child" | "sibling")
            && Self::is_io_path_value(&target)
            && args.iter().any(Self::io_path_segment_needs_str)
        {
            let mut resolved = Vec::with_capacity(args.len());
            for arg in &args {
                let inner = arg.descalarize();
                if Self::io_path_segment_needs_str(arg) {
                    let s = self.call_method_with_values_unviewed(inner.clone(), "Str", vec![])?;
                    resolved.push(Value::str(crate::runtime::unbox_str_answer(&s)?));
                } else {
                    resolved.push(arg.clone());
                }
            }
            return self.call_method_with_values_inner(target, method, resolved, reify_seq);
        }
        // A quant hash renders each element through the element's own
        // `Str`/`gist` (`quanthash_element_stringify.rs`).
        if matches!(method, "Str" | "Stringy" | "gist")
            && args.is_empty()
            && Self::quanthash_needs_element_dispatch(&target)
        {
            let resolved = self.resolve_quanthash_element_stringifiers(&target, method)?;
            return self.call_method_with_values_inner(resolved, method, args, reify_seq);
        }

        // Array-subclass instance delegation, the interpreter-entry twin of the
        // one in `vm_call_method_ops.rs`. An `is Array`/`is List` subclass keeps
        // its elements in a backing `__mutsu_array_storage` attribute; without
        // this, every caller that reaches the dispatcher HERE rather than
        // through the `CallMethod` opcode — a runtime method name
        // (`$v."$m"()`), and the `CallMethodMut` path for any name its own
        // allowlist does not carry — asked the by-name builtin dispatchers about
        // the Instance instead of about its elements.
        //
        // That was not merely a wrong answer. `builtin_elems` is defined AS
        // `$x.elems`, and `dispatch_elems_method` answers `.elems` by calling
        // `builtin_elems`, so an Instance that neither of them can serve looped
        // between them until the stack overflowed
        // (`class R is Array {}; my $m = "elems"; R.new(1,2)."$m"()`). The
        // `!args.is_empty()` guard in `dispatch_elems_method` was the same cycle,
        // caught for the MOP-shaped spelling only.
        //
        // Same exclusions as the opcode twin, both inside
        // `delegates_to_array_storage`: a user method of that name wins, and
        // `is_type_identity_method` names the receiver itself, not its elements.
        //
        // Placed with the other "decide the receiver first" guards, ahead of
        // every by-name interceptor: several of them answer for a Cool
        // receiver by stringifying it, which is how `$v."$m"()` with `$m` =
        // `join` answered `R()` instead of `12` even though the call reached
        // this function's own tail.
        if method == "raku"
            && crate::builtins::methods_0arg::raku_repr::raku_scalar_itemized(&target)
            && let Some(rendered) = self.raku_repr_with_dispatch(&target)
        {
            return Ok(Value::str(rendered));
        }
        if self.delegates_to_array_storage(&target, method)
            && let ValueView::Instance { attributes, .. } = target.view()
        {
            // A subclass that overrides `iterator` decides every method raku
            // defines through the Iterable protocol, so those delegate to what
            // the override yields rather than to the raw storage.
            if let Some(source) = self.positional_subclass_iteration_source(&target, method) {
                return self.call_method_with_values(source?, method, args);
            }
            let storage = attributes
                .as_map()
                .get(crate::symbol::wk::array_storage())
                .cloned()
                .unwrap_or_else(|| Value::real_array(Vec::new()));
            return self.call_method_with_values(storage, method, args);
        }
        // Scalar-native-backed instance delegation, the interpreter-entry
        // twin of the one in `vm_call_method_ops.rs` -- see
        // `delegates_to_array_storage`'s own doc comment just above for why
        // both are needed. `class CM is Str {}; CM('x')` boxes the coerced
        // payload rather than keeping elements, so this is the scalar analog
        // rather than a reuse of the array-storage path above.
        if let Some(result) = self.try_native_backing_delegate(&target, method, &args) {
            return result;
        }
        // Augmented native-type dispatch: a plain Array/List/Hash/Str/Range/
        // Set/Bag/Mix/... receiver is not `Instance`/`Package`, so none of this
        // function's by-name native dispatch below (`dispatch_method_by_name_*`,
        // the Tier-1/lever-A callers upstream) ever checks it for a
        // user-declared/augmented override — route straight to the ordinary
        // instance-method machinery when one exists, threading the receiver
        // itself as `self` (not a synthesized type object). See
        // `native_lever_a_user_override`'s doc comment.
        if !matches!(
            target.view(),
            ValueView::Instance { .. } | ValueView::Package(_)
        ) && self.native_lever_a_user_override(&target, method)
            && {
                // An `augment`ed `multi` is ranked with the built-in
                // candidates of the method (#12751).
                let owner = self.dispatch_owner_name(&target);
                !self.builtin_candidate_outranks_user(&target, owner, method, &args)
            }
        {
            // ADR-0019 F6: VM-level direct-dispatch path first (see
            // `try_dispatch_compiled_method_direct_as`'s doc comment). `target`
            // is a native value here (no attribute cell), so there is nothing
            // for the original code's discarded `updated` map to propagate —
            // matches the plain `Value` this returns.
            let class_sym =
                crate::symbol::Symbol::intern(crate::runtime::utils::value_type_name(&target));
            if let Some(result) =
                self.try_dispatch_compiled_method_direct_as(class_sym, &target, method, &args)
            {
                return result;
            }
            let invocant = self
                .pending_raw_invocant
                .as_ref()
                .filter(|pending| pending.implicit_self && pending.method == method)
                .map(|pending| pending.cell.clone())
                .unwrap_or_else(|| target.clone());
            let (result, _) = self.run_instance_method_at(
                "generalcalldispatch",
                crate::runtime::utils::value_type_name(&target),
                AttrMap::new(),
                method,
                args,
                Some(invocant),
            )?;
            return Ok(result);
        }
        // An instance of an opaque shape (a lock, a socket, a scheduler, a
        // compilation unit, the runtime's own result objects) is answered by its
        // class's rows through the table from every entry; a call the guard
        // step declined (an argument it does not admit, a named argument no row
        // declares) finds the row by the shape's owners here, which accepts any
        // argument as the class's per-site hooks did.
        if let Some(owners) = crate::builtins::method_table::opaque_owners_of(&target)
            && !self.native_lever_a_user_override_sym(&target, Symbol::intern(method))
            && !self.enum_composes_role_method(&target, method)
            && let Some(answer) = crate::builtins::method_table::invoke_owner(
                self,
                owners,
                method,
                &args,
                || target.clone(),
            )
        {
            return answer;
        }

        // `.REPR` / `.WHERE` on a NativeCall handle. Both need the class
        // registry to tell a `nativecast`ed CStruct/CUnion/CArray from an
        // ordinary instance, so they cannot be answered on the pure native fast
        // path — and they must not reach it, since its `.WHERE` is an identity
        // hash that a module would go on to dereference.
        if args.is_empty()
            && matches!(method, "REPR" | "WHERE")
            && let Some(v) = self.try_native_handle_repr_where(&target, method)
        {
            return Ok(v);
        }
        // The receiver step (ADR-11276 §2.4): a container is transparent, a
        // hash-like receiver and a `Match` answer `Any`'s `self.list.METHOD`
        // methods on their list, an enum member its base's, a `Channel`
        // subclass its channel's coercions (`method_table::receiver_step`).
        match crate::builtins::method_table::receiver_step::step_in(
            self,
            &target,
            method,
            args.len(),
        ) {
            Some(Step::Redirect(receiver, Why::Decont)) => {
                return self.call_method_with_values_unviewed(receiver, method, args);
            }
            Some(Step::Redirect(receiver, _)) => {
                return self.call_method_with_values(receiver, method, args);
            }
            Some(Step::Answer(answer)) => return answer,
            Some(Step::Itself) => {
                if let ValueView::Scalar(inner) = target.view() {
                    return self.render_scalar_container_raku(&target, inner);
                }
                // `$(...)` and `$(%h)` are `Scalar` containers for
                // introspection, sharing the aggregate's storage.
                if method == "VAR" {
                    return Ok(Value::package(Symbol::intern("Scalar")));
                }
            }
            None => {}
        }
        // A receiver with its own `iterator` or `list` (a user class, a
        // `Backtrace`, an `IterationBuffer`, a buffer-backed `CArray`, a role
        // mixed into a native container) answers `Any`'s list methods from
        // its elements (ADR-11276 §9.148).
        if let Some(result) = self.own_iteration_view(&target, method, &args) {
            return result;
        }
        if method == "raku"
            && crate::builtins::methods_0arg::raku_repr::raku_scalar_itemized(&target)
            && let Some(rendered) = self.raku_repr_with_dispatch(&target)
        {
            return Ok(Value::str(rendered));
        }
        // `Rakudo::Internals::JSON.from-json` / `.to-json`: a core Rakudo class
        // routed to the native JSON implementation (used by OpenSSL, JSON::JWT).
        if let Some(result) = self.try_rakudo_internals_json_method(&target, method, &args) {
            return result;
        }
        // `proto method` body dispatch (see try_proto_method_body).
        if let Some(result) = self.try_proto_method_body(&target, method, &args) {
            return result;
        }
        // Operations that need the full (finite) list cannot run on a lazy or
        // infinite source: throw X::Cannot::Lazy instead of hanging while
        // materializing it, matching raku.
        if let Some(err) = Self::lazy_guard_error(method, &target) {
            return Err(err);
        }
        // Seq reification/consumption (ADR-0034 §2.3): reify (in place) or
        // consume a deferred/already-taken Seq body as `method` requires.
        // `take` (inside `reify_or_consume_seq_target`, for every
        // `seq_method_consumes` entry including `.iterator`/`.skip`) steals
        // the body's single read even when it was already `Reified` at birth
        // (`Value::seq(vec)`) — rakudo's `Seq` wraps a single-use iterator by
        // default regardless of how eagerly its data was known, and only an
        // earlier NON-consuming touch or an explicit `.cache` earns it
        // durable behavior (see `SeqBody::take`'s doc comment for the
        // measured raku evidence). `sink` with args is left alone (not a
        // real Seq `.sink` call).
        let is_seq_sink =
            method == "sink" && args.is_empty() && matches!(target.view(), ValueView::Seq(_));
        let target = if reify_seq && (method != "sink" || args.is_empty()) {
            // `_authoritative`, not the plain guard: this function is the
            // one call site every dispatch chain funnels through exactly
            // once (see `reify_or_consume_seq_target_authoritative`'s doc
            // comment) — the only place `.iterator` may actually be
            // consumed. `reify_seq` is false only when the implicit-`*%_`
            // wrapper above already performed that single touch on our
            // behalf (it must, because it may call us twice).
            self.consume_seq_for_call(target, method, &mut args, true)?
        } else {
            target
        };
        if target_was_seq
            && method == "iterator"
            && matches!(target.view(), ValueView::Instance { .. })
        {
            return Ok(target);
        }
        if is_seq_sink {
            return Ok(Value::NIL);
        }
        // User-defined ^method (metamethod) dispatch.
        // `method ^foo(Mu) { ... }` in a class body defines a metamethod.
        // The caller (VM) already prepends the type object to args.
        // Route through run_instance_method to execute the method body.
        // A user-declared metamethod wins even when it shadows a native
        // ClassHOW metamethod name (`method ^parameterize`, `method ^compose`,
        // ...): Rakudo adds it to the type's own metaclass, so it overrides the
        // stock implementation for that type. The `has_user_method` check below
        // is what keeps this narrow — every other type still reaches the native
        // handler.
        if method.starts_with('^') && method.len() > 1 {
            let class_sym = match target.view() {
                ValueView::Instance { class_name, .. } => Some(class_name),
                ValueView::Package(name) => Some(name),
                _ => None,
            };
            if let Some(class_sym) = class_sym
                && self.has_user_method(class_sym.as_str(), method)
            {
                // ADR-0019 F6: VM-level direct-dispatch path first (see
                // `try_dispatch_compiled_method_direct_as`'s doc comment).
                // A metamethod's `self` is never the receiver: `method ^foo`
                // is added to the type's METACLASS, so Rakudo binds `self` to
                // the HOW (`Perl6::Metamodel::ClassHOW+{<anon>}`) and passes
                // the type object separately as `args[0]` (prepended by the VM
                // caller). Binding the HOW here is what makes the documented
                // `method ^parameterize` shape work — its body calls
                // `self.name($this)` / `self.mixin(...)` on the metaobject
                // (`Language/mop.rakudoc`). Resolution still happens against
                // the declaring class (`class_sym`), which is where the
                // metamethod body is registered.
                let dispatch_target = self
                    .call_method_with_values(target.clone(), "HOW", vec![])
                    .unwrap_or_else(|_| Value::package(class_sym));
                if let Some(result) = self.try_dispatch_compiled_method_direct_as(
                    class_sym,
                    &dispatch_target,
                    method,
                    &args,
                ) {
                    return result;
                }
                let attrs = AttrMap::new();
                let (result, _) = self.run_instance_method_at(
                    "generalcalldispatch",
                    class_sym.as_str(),
                    attrs,
                    method,
                    args,
                    Some(dispatch_target),
                )?;
                return Ok(result);
            }
        }
        // `Exception`'s and `Failure`'s control methods (`throw`, `rethrow`,
        // `die`, `fail`, `resume`, `Exception.Failure`) are rows, reached by the
        // receiver's MRO (ADR-11276 §9.163).
        if let Some(result) = self.exception_control_by_mro(&target, method, &args) {
            return result;
        }
        // A built-in exception's `message`, `gist`, `Str`, `backtrace` and
        // `X::AdHoc.payload` are rows found by its owner chain (§9.39).
        if let Some(result) = self.exception_rows_by_owner(&target, method, &args) {
            return result;
        }
        // Unhandled Failure explosion
        if let ValueView::Instance { class_name, .. } = target.view()
            && class_name.resolve() == "Failure"
            && !target.is_failure_handled()
            && !matches!(
                method,
                "exception"
                    | "handled"
                    | "self"
                    | "defined"
                    | "Bool"
                    | "so"
                    | "not"
                    | "gist"
                    | "Str"
                    | "raku"
                    | "WHICH"
                    | "backtrace"
                    | "is-handling"
                    | "WHAT"
                    | "^name"
                    | "isa"
                    | "does"
                    | "ACCEPTS"
                    | "Failure"
                    | "sink"
            )
            && let Some(err) = self.failure_to_runtime_error_if_unhandled(&target)
        {
            return Err(err);
        }
        // A junction's own methods are `Junction`'s rows (ADR-11276 §9.151).
        if let Some(result) = self.junction_own_method(&target, method, &args) {
            return result;
        }
        // Junction auto-threading
        if Self::junction_invocant_autothreads(method)
            && let ValueView::Junction { kind, values } = target.view()
        {
            let mut results = Vec::with_capacity(values.len());
            for value in values.iter() {
                results.push(self.call_method_with_values(value.clone(), method, args.clone())?);
            }
            return Ok(Value::junction(kind, results));
        }
        if Self::should_autothread_method(method)
            && let Some((idx, kind, values)) = args.iter().enumerate().find_map(|(idx, arg)| {
                // Tag probe first: a `view()` on a lazy Match would
                // materialize it just to see it is not a Junction.
                if !arg.is_junction_value() {
                    return None;
                }
                match arg.view() {
                    ValueView::Junction { kind, values } => Some((idx, kind, values.clone())),
                    _ => None,
                }
            })
        {
            let mut results = Vec::with_capacity(values.len());
            for value in values.iter() {
                let mut threaded_args = args.clone();
                threaded_args[idx] = value.clone();
                results.push(self.call_method_with_values(
                    target.clone(),
                    method,
                    threaded_args,
                )?);
            }
            return Ok(Value::junction(kind, results));
        }
        // A type object's key/value views are `Any`'s `Any(Enumeration)` and
        // `Any:U` rows (ADR-11276 §9.145).
        if matches!(target.view(), ValueView::Package(_))
            && let Some(result) =
                crate::builtins::method_table::any_enum_views::invoke(self, &target, method, &args)
        {
            return result;
        }

        // IterationBuffer dispatch
        if crate::runtime::nqp_ops_list::is_iteration_buffer(&target)
            && matches!(
                method,
                "elems"
                    | "AT-POS"
                    | "BIND-POS"
                    | "push"
                    | "unshift"
                    | "List"
                    | "Slip"
                    | "Seq"
                    | "iterator"
                    | "append"
                    | "prepend"
                    | "clear"
            )
        {
            return self.dispatch_instance_and_fallback(target, method, args);
        }
        // DateTime/Date formatter rendering. The formatter runs against this
        // invocant on every stringification (nothing is cached), so a value
        // derived by `.utc`, `.later`, `.clone`, `+` ... renders its own
        // fields. `Stringy` is included because `~$dt` / interpolation try it
        // first.
        if args.is_empty()
            && matches!(method, "Str" | "gist" | "Stringy")
            && let ValueView::Instance {
                class_name,
                attributes,
                ..
            } = target.view()
            && self
                .class_mro(&class_name.resolve())
                .iter()
                .any(|name| name == "DateTime" || name == "Date")
            && let Some(formatter) = attributes.as_map().get("formatter")
        {
            let saved_env = self.env().clone();
            let saved_readonly = self.enter_readonly_frame();
            let rendered = self.eval_call_on_value(formatter.clone(), vec![target.clone()])?;
            *self.env_mut() = saved_env;
            self.exit_readonly_frame(saved_readonly);
            return Ok(Value::str(rendered.to_string_value()));
        }
        // A receiver-mutating built-in method answered from its row
        // (ADR-11276 §9.23), on a receiver the call holds by value
        // (`$obj.bag.grab`, `f().unset('x')`): the row writes through the
        // receiver's shared node, which every holder of the container sees.
        // Probed by receiver kind first, so a call on any other receiver
        // pays one tag probe and no interning.
        // `splice` is also a row of `Any`, so any receiver may have one.
        if method == "splice"
            || crate::builtins::method_table::mut_owners_of(&target).is_some()
            || crate::builtins::method_table::mut_type_object_owners(&target).is_some()
        {
            let mut detached = target.clone();
            let mut place = crate::builtins::method_table::ReceiverPlace::detached(&mut detached);
            if let Some(result) = crate::builtins::method_table::invoke_mut(
                self,
                &mut place,
                Symbol::intern(method),
                &args,
            ) {
                return result;
            }
        }
        // A row that binds its own arguments (`Num.Rat($epsilon)` with an
        // allomorph or a `Duration` as the epsilon): the table's own entry
        // declined the argument, and no cascade arm answered it.
        if let Some(result) =
            crate::builtins::method_table::invoke_unadmitted(self, &target, method, &args)
        {
            return result;
        }
        // `IO::Spec::*` class methods are rows of the method table. The table's
        // own entry declined this call (a named argument no row declares, an
        // argument it does not admit, a call with many arguments), so the row
        // is found by its owner here, which accepts any argument as the
        // method did before it was a row.
        if let Some(kind) = crate::runtime::SpecKind::of(&target)
            && let Some(result) = crate::builtins::method_table::invoke_owner(
                self,
                kind.owners(),
                method,
                &args,
                || target.clone(),
            )
        {
            return result;
        }
        // An instance of a user subclass of `Attribute` (`class CosOfAttr is
        // Attribute does R {}`, PDF) is a metaobject too: it carries the facts
        // its constructor was given (`seed_native_subclass_payloads`), so the
        // `Attribute` rows answer it. The subclass has no dispatch shape, so
        // the rows are found by their owner, after its own methods declined.
        if let ValueView::Instance { class_name, .. } = target.view()
            && class_name != "Attribute"
            && crate::builtins::method_table::owner_declares("Attribute", Symbol::intern(method))
            && self
                .classhow_mro_unhidden_names(&target)
                .iter()
                .any(|name| name == "Attribute")
            && let Some(result) = crate::builtins::method_table::invoke_owner(
                self,
                &["Attribute"],
                method,
                &args,
                || target.clone(),
            )
        {
            return result;
        }
        // A private call whose receiver sits in a container cell (`self` of a
        // method reached through a `handles` delegate that returns a type
        // object) is the call on the value the cell holds.
        if method.starts_with('!')
            && matches!(target.view(), ValueView::ContainerRef(_))
            && matches!(
                target.deref_container().view(),
                ValueView::Instance { .. } | ValueView::Package(_) | ValueView::Mixin(..)
            )
        {
            return self.call_method_with_values_unviewed(target.deref_container(), method, args);
        }
        // Private method call on non-Instance
        if let Some(result) = self.dispatch_private_method_on_proxy_subclass(&target, method, &args) {
            return result;
        }
        if let Some(result) = self.dispatch_private_method_on_non_instance(&target, method) {
            return result;
        }

        // Qualified coercion to a universal base (self.Mu::Str, self.Any::gist):
        // route to the built-in default, bypassing the receiver's own override.
        // Must precede the instance/non-instance qualified dispatchers, which would
        // otherwise recurse (type object) or reject it as "not inherited" (instance).
        if let Some(result) = self.dispatch_universal_base_coercion(&target, method, &args) {
            return result;
        }

        // A qualified call defers along the QUALIFIER's own chain, never the
        // receiver's (#11592): `self.Q::m` with a `callsame` reaches Q's parent,
        // and a role's method (`self.R::m`) has nothing to defer to.
        if !method.starts_with('!')
            && let Some((qualifier, actual)) =
                super::methods_qualified::split_method_qualifier_last(method)
        {
            let qualifier = self.lexical_env_remap_name(qualifier);
            let qualifier_is_role = self.is_role(&qualifier);
            let pushed = if qualifier_is_role {
                self.push_qualified_method_dispatch_frame(&qualifier, &args, target.clone())
            } else {
                self.push_method_dispatch_frame(&qualifier, actual, &args, target.clone(), None)
            };
            let result = self.dispatch_qualified_method(&target, method, &args);
            if pushed {
                self.pop_method_dispatch();
            }
            // `push_method_dispatch_frame` always pushes the samewith context
            // (the frame itself is only built when a deferral builtin is named
            // anywhere, `pushed`), so it is taken back whether or not a frame
            // went on. A role's qualified frame pushes none. Left behind, a
            // later `samewith` re-dispatched the qualified method (`Eq.new`
            // after `self.Mu::new`) instead of its own routine.
            if !qualifier_is_role {
                self.pop_method_samewith_context();
            }
            if let Some(result) = result {
                return result;
            }
        }

        // Proxy subclass method dispatch
        if let Some(result) = self.dispatch_proxy_subclass_method(&target, method, &args) {
            return result;
        }

        // Auto-FETCH Proxy values
        if let Some(result) = self.dispatch_proxy_auto_fetch(&target, method, args.clone()) {
            return result;
        }

        // A `Date`/`DateTime` instance the shape lookup did not take (a
        // subclass instance): the rows, found by owner (ADR-11276 §9.127).
        if let Some(result) = self.dispatch_temporal_method(&target, method, &args) {
            return result;
        }

        // `Format`'s and `Formatter`'s own methods are rows (ADR-11276 §9.153),
        // found by owner from this entry as well as by their shapes.
        if let Some(owner) = match target.view() {
            ValueView::Instance { .. } if Self::format_string_of(&target).is_some() => Some("Format"),
            ValueView::Package(name) if name == "Formatter" && !self.has_class("Formatter") => {
                Some("Formatter")
            }
            _ => None,
        } && let Some(result) = crate::builtins::method_table::invoke_owner(
            self,
            &[owner],
            method,
            &args,
            || target.clone(),
        ) {
            return result;
        }
        // `Match.make` is a row (ADR-11276 §9.164).
        if let Some(result) =
            crate::builtins::method_table::regex_match::make_answer(self, &target, method, &args)
        {
            return result;
        }

        // The positional protocol of a list or array: `Any`'s late rows for
        // multi-dimensional indices, a shaped array's bounds and a native
        // array's refusals (ADR-11276 §9.138).
        if let ValueView::Array(..) = target.view()
            && matches!(
                method,
                "AT-POS" | "EXISTS-POS" | "ASSIGN-POS" | "BIND-POS" | "DELETE-POS"
            )
        {
            // ADR-0070: an undeclared named argument is swallowed by the
            // method's implicit `*%_`; counted here it became one more
            // dimension (`(1,2,3).AT-POS("a", :zzz)` answered Nil where Rakudo
            // dies on the "a" index). Only declared methods are stripped.
            if matches!(method, "AT-POS" | "EXISTS-POS")
                && let Some(stripped) = crate::builtins::strip_undeclared_nameds(method, &args)
            {
                args = stripped;
            }
            let mro = self.classhow_mro_unhidden_names(&target);
            let owners: Vec<&str> = mro.iter().map(String::as_str).collect();
            if let Some(answer) = crate::builtins::method_table::invoke_mro_late(
                self, &owners, method, &args, &target,
            ) {
                return answer;
            }
        }

        // The reflective `Variable` a variable trait handler receives keeps
        // answering `var` / `name` / `block` after the handler mixes a role into
        // it (`$v does R`): the mixin only adds to the object.
        if matches!(method, "var" | "name" | "block")
            && args.is_empty()
            && let ValueView::Mixin(inner, _) = target.view()
            && matches!(
                inner.view(),
                ValueView::Instance { class_name, .. } if class_name == "Variable"
            )
            && !self.mixin_composes_method(&target, method)
        {
            return self.call_method_with_values((**inner).clone(), method, args);
        }

        // Mixin dispatch
        if let ValueView::Mixin(..) = target.view()
            && let Some(result) = self.dispatch_mixin_method_call(&target, method, args.clone())
        {
            return result;
            // If mixin dispatch didn't handle it, check for mixin __mutsu_attr__ and
            // delegate to inner value at the end (handled later in this function)
        }

        // Role type-object method punning
        if method != "new" {
            if let ValueView::Package(role_name) = target.view() {
                // A class-level (`my $.x`) role attribute is readable on the role
                // type object directly (`R.x`), returning its recorded default —
                // no instance punning (there is no per-instance storage for it).
                let class_level_default = if args.is_empty() {
                    self.registry()
                        .role_class_level_attrs
                        .get(&(role_name.resolve(), method.to_string()))
                        .cloned()
                } else {
                    None
                };
                if let Some(default) = class_level_default {
                    return match default {
                        Some(arg) => self.eval_decl_trait_arg(&arg),
                        None => Ok(Value::NIL),
                    };
                }
                // An INDIVIDUAL parametric role (`my $r = role R { ... }`, or a
                // role used as a type-parameter default) puns through the group
                // it belongs to — the pun is registered under the group's name.
                // Only the *punning* half is redirected: introspection
                // (`.HOW`, `.^name`) must still see the candidate, which is the
                // whole point of it having its own type object. See
                // `types/role_candidate.rs`.
                let raw_role_name = role_name.resolve();
                // A bracketed role spelling can arrive here as a package name
                // when it crossed a type-object lookup boundary. Normalize it
                // to the canonical parametric-role representation before the
                // punning retry: retrying `R[T]` as the same package name has
                // no class to fall through to and recurses forever.
                if let Some((base_name, type_names)) =
                    Self::parse_parametric_type_name(&raw_role_name)
                    && self.is_role_type_name(&base_name)
                    && !crate::runtime::types::is_builtin_role_name(&base_name)
                    && !self.registry().classes.contains_key(&raw_role_name)
                {
                    let type_args = type_names
                        .iter()
                        .map(|name| self.type_arg_value_from_name(name))
                        .collect();
                    return self.call_method_with_values(
                        Value::parametric_role(Symbol::intern(&base_name), type_args),
                        method,
                        args,
                    );
                }
                let role_name_resolved = self.role_group_name(&raw_role_name);
                let has_role_method = self.role_or_parent_has_method(&role_name_resolved, method);
                let has_public_accessor = args.is_empty()
                    && self
                        .registry()
                        .roles
                        .get(&role_name_resolved)
                        .is_some_and(|role| {
                            role.attributes
                                .iter()
                                .any(|a| a.is_public && a.name == method)
                        });
                // Punning registers a class under the same name, so the retry
                // falls through to ordinary class dispatch rather than
                // re-entering here.
                if has_role_method || has_public_accessor {
                    // Already punned: fall through to ordinary class dispatch,
                    // which is what makes the retry below terminate.
                    let needs_pun = !self.registry().classes.contains_key(&role_name_resolved);
                    if needs_pun {
                        self.ensure_role_punned_to_class(&role_name_resolved)?;
                    }
                    if role_name_resolved != raw_role_name {
                        // The pun lives under the group's name, so retry there
                        // rather than on the candidate's own site key.
                        return self.call_method_with_values(
                            Value::package(Symbol::intern(&role_name_resolved)),
                            method,
                            args,
                        );
                    }
                    if needs_pun {
                        return self.call_method_with_values(target.clone(), method, args);
                    }
                }
            } else if let ValueView::ParametricRole {
                base_name,
                type_args,
            } = target.view()
                && let Some((role, _)) =
                    self.resolve_parametric_role_runtime(&base_name.resolve(), type_args)
            {
                let base = base_name.resolve();
                let has_public_accessor = args.is_empty()
                    && role
                        .attributes
                        .iter()
                        .any(|a| a.is_public && a.name == method);
                if (role.methods.contains_key(method) || has_public_accessor)
                    && let Some(punned) = self.ensure_parametric_role_pun_class(&base, type_args)?
                {
                    return self.call_method_with_values(
                        Value::package(Symbol::intern(&punned)),
                        method,
                        args,
                    );
                }
                if role.methods.contains_key(method) || has_public_accessor {
                    let instance = self.dispatch_new(target.clone(), Vec::new())?;
                    return self.call_method_with_values(instance, method, args);
                }
            }
        }

        // Enum type-object dispatch for pick/roll
        if let Some(result) = self.dispatch_enum_pick_roll(&target, method, &args) {
            return result;
        }

        // String pick/roll (character-wise)
        if let Some(result) = self.dispatch_string_pick_roll(&target, method, &args) {
            return result;
        }

        // Mu.clone has a separate type-object candidate that accepts no
        // attribute overrides. The named-argument retry below deliberately
        // strips undeclared nameds for native methods, so checking this after
        // that retry would turn `Num.clone(:yes)` into the no-argument clone
        // and incorrectly return the type object. Preserve a user-defined
        // `clone` on a class, but reject named overrides for the inherited native
        // type-object candidate as Rakudo does.
        if method == "clone"
            && args.iter().any(Value::is_string_pair_value)
            && matches!(
                target.view(),
                ValueView::Nil | ValueView::Package(_) | ValueView::ParametricRole { .. }
            )
            && !matches!(target.view(), ValueView::Package(name) if self
                .class_has_user_method(&name.resolve(), method))
        {
            return Err(RuntimeError::new(
                "Cannot set attribute values when cloning a type object",
            ));
        }

        // Native fast path bypass and dispatch
        let skip_pseudo = self
            .dispatch
            .skip_pseudo_method_native
            .as_ref()
            .is_some_and(|m| m == method);
        // NOTE: do NOT clear the flag here. A quoted `."WHAT"()` etc. is
        // intercepted by the MOP-macro arms in `dispatch_method_by_name_1`
        // *before* user-method resolution; that dispatch consults the flag to
        // fall through to the user method and clears it there (see the
        // `skip_pseudo_method_native` read at the top of that function).
        let method_sym = crate::symbol::Symbol::intern(method);
        // The built-in rows answer unless the receiver's state needs the
        // interpreter or a declaration of the program takes the call (the one
        // shadow query, ADR-11276 §2.3; a MOP pseudo-method only through the
        // quoted call `skip_pseudo` marks).
        let bypass_native_fastpath = skip_pseudo
            || self.native_receiver_defers(&target, method, &args)
            || self.user_shadows_builtin(&target, method_sym, &args, false);
        // Named-blind arity cascade -- the interpreter-side twin of the guard in
        // `vm_native_dispatch::try_native_method_raw`. The arity cascade selects
        // its arm by argument *count*, so a named argument the method does not
        // bind must not be in the list; the rows of the method state which
        // names it binds (`method_table::implicit_named`). The stripped list is used ONLY
        // for this cascade: if it declines, the original `args` (nameds intact)
        // go on to the by-name dispatchers, the constructors and the user
        // method, none of which may lose a named.
        let cascade_stripped = crate::builtins::strip_undeclared_nameds(method, &args);
        let cascade_args: &[Value] = cascade_stripped.as_deref().unwrap_or(&args);
        let inherited_adhoc = (!bypass_native_fastpath)
            .then(|| self.inherited_adhoc_method(&target, method, cascade_args))
            .flatten();
        let native_result = if let Some(value) = inherited_adhoc {
            Some(Ok(value))
        } else if bypass_native_fastpath {
            None
        } else {
            // The interpreter-side twin of `try_native_method_raw`, and the
            // reason it is tagged separately from the dispatch around it: a
            // line whose time is a native builtin is the user's own code being
            // slow, while one whose time is `method-dispatch` is mutsu's.
            let _region = crate::profile::enter(crate::profile::Region::NativeBuiltin);
            // The built-in method table first (ADR-11276): a row that needs
            // the interpreter (`Handler::Interp`) is reachable only from an
            // entry that has one, and the pure rows the cascades ask for
            // themselves are found here without walking them. `args`, not
            // `cascade_args`: a row binds the named arguments it declares,
            // and a call with any other takes the cascades with the
            // stripped list as before. `bypass_native_fastpath` already
            // covers an `augment` of the receiver's type.
            if let Some(result) = crate::builtins::method_table::try_dispatch_in(
                self,
                |_| true,
                &target,
                method_sym,
                &args,
            ) {
                Some(result)
            } else {
                match cascade_args {
                    [] => crate::builtins::native_method_0arg(&target, method_sym).or_else(|| {
                        crate::builtins::method_table::answer_shapeless(
                            Some(self),
                            &target,
                            method_sym,
                            &[],
                        )
                    }),
                    [a] => crate::builtins::native_method_1arg(&target, method_sym, a).or_else(
                        || {
                            crate::builtins::method_table::answer_shapeless(
                                Some(self),
                                &target,
                                method_sym,
                                std::slice::from_ref(a),
                            )
                        },
                    ),
                    // No cascade takes two arguments or more; a row with no
                    // shape (`Mu.WALK` with its adverbs) may.
                    _ => crate::builtins::method_table::answer_shapeless(
                        Some(self),
                        &target,
                        method_sym,
                        cascade_args,
                    ),
                }
            }
        };
        if let Some(result) = native_result {
            return result;
        }

        // A quant hash's `STORE` is its row, which the mutating entry above
        // answers (`mutating::quanthash`).
        // STORE on a native Hash/Array: re-initialize its node in place.
        if method == "STORE"
            && let Some(stored) = self.native_container_store(&target, &args)
        {
            return stored;
        }

        // `Positional.of`, `Associative.of`/`keyof` and `Iterable`'s
        // `hyper`/`race`/`lazy` of a class composing the role are rows
        // (ADR-11276 §9.165), and so are the `Real` and `Numeric` roles'
        // methods (§9.179).
        if let Some(result) =
                crate::builtins::method_table::composed_roles::answer(self, &target, method, &args)
        {
            return result;
        }

        // HyperSeq/RaceSeq: the methods they declare are their rows; `Mu`'s
        // introspection reads the sequence itself; every other method reads it
        // as its `List` (§9.137, §9.173).
        if let Some(owners) = crate::builtins::method_table::hyper_seq_owners_of(&target)
            && !crate::builtins::method_table::hyper_seq_answers_as_itself(method)
        {
            if let Some(result) = crate::builtins::method_table::invoke_owner(
                self,
                owners,
                method,
                &args,
                || target.clone(),
            ) {
                return result;
            }
            let list = Self::hyper_seq_as_list(&target).unwrap_or(Value::NIL);
            return self.call_method_with_values(list, method, args);
        }

        // A streaming method (`skip`, `rotor`, `pairs`, ...) on a receiver
        // that may not end: a lazy stage over it (ADR-13891 §4).
        if let Some(stage) = self.lazy_stream(&target, method, &args) {
            return Ok(stage);
        }

        // A lazy receiver: a method's pull answers from what it reads, a method
        // that needs every element reifies it (the lazy step, ADR-13891).
        if let ValueView::LazyList(ll) = target.view()
            && let Some(result) = self.lazy_receiver_step(&target, &ll, method, &args)
        {
            return result;
        }

        // Callable introspection
        if matches!(
            target.view(),
            ValueView::Routine { .. } | ValueView::Sub(_) | ValueView::WeakSub(_)
        ) && let Some(result) = self.dispatch_callable_method(&target, method, &args)
        {
            return result;
        }

        // .VAR on VarRef
        if method == "VAR"
            && args.is_empty()
            && let Some((source_name, inner)) = Self::varref_parts(&target)
        {
            return self.call_method_mut_with_values(&source_name, inner, "VAR", vec![]);
        }

        // ^meta_method dispatch (except ^name)
        if let Some(meta_method) = method.strip_prefix('^')
            && meta_method != "name"
        {
            let how = self.call_method_with_values(target.clone(), "HOW", vec![])?;
            let mut how_args = Vec::with_capacity(args.len() + 1);
            let type_target =
                crate::runtime::Interpreter::meta_call_type_target(&how, meta_method, &target);
            how_args.push(type_target);
            how_args.extend(args.clone());
            return self.call_method_with_values(how, meta_method, how_args);
        }

        // A metamethod the type declared itself (`method ^kind`) is a method
        // of its metaclass, so another metamethod reaches it through `self`
        // (`method ^parameterize(|args) { self.kind: |args }`); the type
        // object is part of the caller's arguments, as for any HOW method.
        // Cost: O(1) plus one short `format!`, for a HOW receiver only.
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
            && Self::is_metamodel_how(&class_name)
            && !method.starts_with('^')
            && let Some(type_name) = attributes
                .as_map()
                .get("__mutsu_how_target")
                .and_then(|v| v.as_str().map(str::to_string))
            && let type_sym = crate::symbol::Symbol::intern(&type_name)
            && let meta_name = format!("^{method}")
            && self.has_user_method(&type_name, &meta_name)
            && let Some(result) =
                self.try_dispatch_compiled_method_direct_as(type_sym, &target, &meta_name, &args)
        {
            return result;
        }

        // ClassHOW/SubsetHOW/EnumHOW method dispatch
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
            && Self::is_metamodel_how(&class_name)
            && Self::is_classhow_method(method)
        {
            let how_args = self.how_dispatch_args(&attributes, method, &args);
            return self.dispatch_classhow_method(method, how_args);
        }

        // `Metamodel::Archetypes.new: :nominal, ...`: an archetypes object with
        // exactly the flags the named arguments set (the others read False).
        if method == "new"
            && let ValueView::Package(name) = target.view()
            && matches!(
                name.as_str(),
                "Metamodel::Archetypes" | "Perl6::Metamodel::Archetypes"
            )
        {
            let mut attrs = std::collections::HashMap::new();
            for arg in &args {
                if let ValueView::Pair(key, value) = arg.view() {
                    attrs.insert(key.to_string(), Value::truth(value.truthy()));
                }
            }
            return Ok(Value::make_instance(
                Symbol::intern("Perl6::Metamodel::Archetypes"),
                attrs,
            ));
        }

        // Archetypes flag accessors (composable/nominal/nominalizable/coercive/...)
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
            && class_name == "Perl6::Metamodel::Archetypes"
            && matches!(
                method,
                "composable"
                    | "nominal"
                    | "nominalizable"
                    | "coercive"
                    | "generic"
                    | "parametric"
                    | "inheritable"
                    | "inheritalizable"
                    | "definite"
            )
            && args.is_empty()
        {
            return Ok(attributes
                .as_map()
                .get(method)
                .cloned()
                .unwrap_or(Value::FALSE));
        }

        // `.CREATE` on an `is repr<Uninstantiable>` type object dies like
        // `.new` and `.bless` do.
        if method == "CREATE"
            && let ValueView::Package(name) = target.view()
            && let Some(err) = self.uninstantiable_error(name.as_str())
        {
            return Err(err);
        }

        // CREATE method
        if method == "CREATE"
            && args.is_empty()
            && let Some(result) = self.dispatch_create(&target)
        {
            // A bare REPR allocation: an `is repr('CStruct')` object gets its
            // zeroed native body, as `nqp::create` gives it (ADR-11209).
            if let Ok(instance) = &result {
                self.install_cstruct_storage(instance);
            }
            return result;
        }

        // Custom type method dispatch
        if !matches!(
            method,
            "HOW"
                | "WHAT"
                | "WHO"
                | "WHY"
                | "WHICH"
                | "WHERE"
                | "DEFINITE"
                | "VAR"
                | "REPR"
                | "Str"
                | "Stringy"
                | "gist"
                | "raku"
                | "say"
                | "print"
                | "put"
                | "note"
                | "new"
        ) && let Some(how) = target.custom_how()
        {
            let how_clone = how.clone();
            let found = self.call_method_with_values(
                how_clone,
                "find_method",
                vec![target.clone(), Value::str(method.to_string())],
            );
            if let Ok(callable) = found
                && !callable.is_nil()
            {
                return self.eval_call_on_value(callable, vec![target.clone()]);
            }
        }

        if method == "leave" {
            return self.builtin_leave_method(target, &args);
        }

        // Enum.new error check
        if method == "new"
            && let Some(result) = self.dispatch_enum_new_check(&target, &args)
        {
            return result;
        }

        // Trig methods on Instance values
        if let Some(result) =
            self.dispatch_trig_instance_method(target.clone(), method, args.clone())
        {
            return result;
        }

        // A method the user explicitly added to a builtin type via
        // `augment class Str does Role { }` (or a method body) must win over the
        // native string/collection/coercion dispatchers below — in Rakudo an
        // augmented method overrides the built-in. This only diverts when the
        // value's type actually carries a user-declared method of this name
        // (builtin types have no `.methods` entries unless augmented), so normal
        // native dispatch is unaffected. Without it, a name that collides with a
        // native dispatcher that hard-errors on this type (e.g. `.rotate` on a
        // Str is method-not-found natively) never reaches the augment fallback.
        if !matches!(
            target.view(),
            ValueView::Instance { .. } | ValueView::Package(_)
        ) {
            // ADR-0019 E1b: authoritative TypeId classifier owner (was
            // `value_type_name`) — see `Interpreter::dispatch_owner_name` and
            // `todo/deep/adr0019-e1-typeid-receiver-owner.md`. This fixes the
            // Enum ("Int" -> the real enum type) and ParametricRole/Instance-in-
            // Mixin ("Package"/"Any" -> the real name) collapses the old
            // `value_type_name`-based owner had.
            let type_name = self.dispatch_owner_name(&target);
            if self.has_user_method(type_name, method)
                && !self.native_base_bypass_hit(&target, Symbol::intern(method))
                && !self.user_override_outranked_by_builtin(type_name, method)
                && !self.builtin_candidate_outranks_user(&target, type_name, method, &args)
            {
                return self.dispatch_instance_and_fallback(target, method, args);
            }
        }

        // A role-mixed native-backed value (`$sock does Connection`,
        // `$path does R`): a native method of the inner class must delegate to the
        // inner value BEFORE the by-name native dispatchers below. Those key on the
        // target's runtime shape and hard-error on the Mixin wrapper (e.g.
        // `.basename` on `IO::Path+{R}`, `.print` on `IO::Socket::INET+{Connection}`)
        // instead of falling through to the Mixin-inner delegation at the tail of
        // this function. Role methods and `__mutsu_attr__` markers were already
        // handled by `dispatch_mixin_method_call` above; a user-declared (possibly
        // inherited) method is left for the Mixin fallback so it runs with `self`
        // bound to the wrapper.
        if let ValueView::Mixin(inner, _) = target.view() {
            let inner_owned = inner.as_ref().clone();
            if let ValueView::Instance { class_name, .. } = inner_owned.view() {
                let cls = class_name.resolve();
                if !self.class_has_user_method(&cls, method) && self.is_native_method(&cls, method)
                {
                    return self.call_method_with_values_unviewed(inner_owned, method, args);
                }
            }
        }

        // A method the *user declared* on this class outranks the by-name builtin
        // dispatchers below, which key on the method name alone and would answer
        // first. `Grammar.parse(:actions(A))` invokes the action named after each
        // rule, so a grammar with a rule called `map` — YAMLish's block-mapping
        // rule is exactly that — needs `A.map($/)` to reach the action method and
        // not the collection builtin, which rejected the Match as "not callable".
        // The one shadow query (ADR-11276 §2.3), the same answer the native
        // entries read; `.WHAT` and the other MOP macros are not overridable
        // here (only the quoted `$o."WHAT"()` reaches a user method).
        let shadows_builtin = self.user_shadows_builtin(&target, method_sym, &args, false);

        // Named-blind by-name dispatch, the slow-path twin of the arity-cascade
        // guard above. These three handlers read `args[0]`/`args[1]` positionally
        // just as the cascade does (`10.polymod(3, :zzz)` read the Pair as a
        // second modulus; `(1,2,3).rotor(2, :zzz)` as a second cycle spec), so a
        // named argument the method does not accept is dropped here too. The
        // filtered list reaches ONLY these three: `dispatch_new_and_constructors`
        // and everything after it below still sees the call's own `args`.
        let by_name_stripped = crate::builtins::strip_undeclared_nameds(method, &args);
        let by_name_args: &[Value] = by_name_stripped.as_deref().unwrap_or(&args);

        // Primary method dispatch by name (group 1: string, IO, coercion, misc)
        if !shadows_builtin
            && let Some(result) =
                self.dispatch_method_by_name_1(target.clone(), method, by_name_args.to_vec())
        {
            return result;
        }

        // Primary method dispatch by name (group 2: collection/iteration)
        if !shadows_builtin
            && let Some(result) =
                self.dispatch_method_by_name_2(target.clone(), method, by_name_args.to_vec())
        {
            return result;
        }

        // Primary method dispatch by name (group 3: Supply, network, temporal, misc)
        if !shadows_builtin
            && let Some(result) =
                self.dispatch_method_by_name_3(target.clone(), method, by_name_args.to_vec())
        {
            return result;
        }

        // The constructors (`new`, `bless`, `new-from-pairs`) and
        // `Failure.handled` are late rows of the receiver's MRO (§9.141).
        // `self.Mu::new(...)` is the qualified call of Mu's constructor.
        if method == "Mu::new" {
            let result = self.call_method_with_values(target.clone(), "bless", args.clone());
            return self.materialize_exception_message_in_result(result);
        }
        if matches!(method, "new" | "bless" | "handled" | "new-from-pairs") {
            let mut mro = self.classhow_mro_unhidden_names(&target);
            // Every type is a `Mu`.
            if mro.last().map(String::as_str) != Some("Mu") {
                mro.push("Mu".to_string());
            }
            let owners: Vec<&str> = mro.iter().map(String::as_str).collect();
            if let Some(result) = crate::builtins::method_table::invoke_mro_late(
                self, &owners, method, &args, &target,
            ) {
                return self.materialize_exception_message_in_result(result);
            }
        }

        // `enum X does Role`: a composed role's method wins over the built-in
        // enum method of the same name (`multi method ACCEPTS` overriding the
        // enum's identity ACCEPTS is the documented use), exactly as a role
        // composed onto a class overrides the class's inherited method.
        if let Some(result) = self.dispatch_enum_role_method(&target, method, &args) {
            return result;
        }

        // Enum dispatch
        if let Some(result) = self.dispatch_enum_method(&target, method, &args) {
            return result;
        }

        // `Promise`'s and `Channel`'s methods are rows of the method table,
        // found by their owner (ADR-11276 §9.118).
        // An instance of a user subclass is answered on the native promise or
        // channel it carries (#13634).
        if let Some(receiver) = crate::builtins::method_table::promise_channel_receiver_of(&target)
        {
            if let Some(owners) =
                crate::builtins::method_table::opaque_owners_of(&receiver)
                && let Some(result) = crate::builtins::method_table::invoke_owner(
                    self,
                    owners,
                    method,
                    &args,
                    || receiver.clone(),
                )
            {
                return result;
            }
            // `Any`'s list coercions a `Channel` inherits read `self.list`, the
            // lazy list that receives one value per element (`Channel.list`).
            if let ValueView::Channel(ch) = receiver.view()
                && crate::builtins::method_table::CHANNEL_LIST_COERCIONS.contains(&method)
            {
                return Self::channel_list_coercion(&ch, method);
            }
        }

        // A `Promise::Vow`'s `keep`/`break` and the internal names a user
        // scheduler's cue calls.
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
            && class_name.resolve() == "Promise::Vow"
            && let Some(result) =
                self.promise_vow_method(&(attributes).as_map(), method, args.clone())
        {
            return result;
        }

        // Mixin fallback: check __mutsu_attr__ and delegate to inner
        if let ValueView::Mixin(inner, mixins) = target.view() {
            // A private role attribute (`has $!name`) has no accessor: on a
            // routine with such a role mixed in, `.name` is the routine's own
            // name, not the attribute (upstream NativeCall's `Native` role
            // keeps `has str $!name`).
            if args.is_empty() && !self.mixin_role_attr_is_private_only(mixins, method) {
                let attr_key = MetaNs::Attr.owned_key_for_str(method);
                if let Some(value) = mixins
                    .role_attribute_by_name(method)
                    .or_else(|| mixins.get(&attr_key).cloned())
                {
                    return Ok(value);
                }
            }
            // A user-declared method not overridden by the mixin roles is an
            // inherited class method. Run it with `self` bound to the MIXIN
            // wrapper (not the bare inner instance) so a nested `self.foo`
            // re-dispatches through the mixin roles and finds their overrides —
            // e.g. `C.new but role { method who {...} }` where the class's
            // inherited `call-who` does `self.who` must reach the role's
            // override, not the base `who`. Auto-generated attribute accessors
            // and native/builtin methods are NOT declared class methods, so
            // they keep the plain inner-delegation path below (which resolves
            // them) — and they never do nested self-dispatch, so the invocant
            // binding does not matter for them.
            if let ValueView::Instance {
                class_name,
                attributes,
                ..
            } = inner.as_ref().view()
            {
                let cls = class_name.resolve();
                // A private method (`self!inner`) on the mixin: resolve it on the
                // inner class MRO and run it with `self` bound to the MIXIN wrapper
                // (not the bare inner instance) so a nested `self.foo` inside the
                // private method re-dispatches through the mixin roles and finds
                // their overrides — mirroring the public-method case below.
                if let Some(private_rest) = method.strip_prefix('!') {
                    let resolved = if let Some((owner_class, pm_name)) =
                        crate::qualified::split_first(crate::qualified::known_symbol(private_rest))
                    {
                        self.resolve_private_method_with_owner(&cls, owner_class, pm_name, &args)
                            .map(|r| (r, pm_name))
                    } else {
                        self.resolve_private_method_any_owner(&cls, private_rest, &args)
                            .map(|r| (r, private_rest))
                    };
                    if let Some(((resolved_owner, method_def), pm_name)) = resolved {
                        let attrs = attributes.to_map();
                        let (result, updated) = self.run_resolved_method_compiled_or_treewalk(
                            &cls,
                            &resolved_owner,
                            pm_name,
                            method_def,
                            attrs,
                            args,
                            Some(target.clone()),
                        )?;
                        if let ValueView::Instance { attributes, .. } = inner.as_ref().view() {
                            attributes.commit_attrs(updated);
                        }
                        return Ok(result);
                    }
                }
                if self.class_has_method(&cls, method) {
                    // ADR-0019 F6: VM-level direct-dispatch path first (see
                    // `dispatch_compiled_method_with_attrs_cell`'s doc comment).
                    // `self` must stay bound to the Mixin wrapper (`target`) so a
                    // nested `self.foo` inside the method redispatches through the
                    // mixin's role overrides, but the real attribute storage lives
                    // on `inner` — so the real cell is threaded through explicitly
                    // rather than derived from `target`'s own (nonexistent)
                    // attribute cell.
                    if let Some(result) = self.try_dispatch_compiled_method_direct_with_attrs_cell(
                        class_name,
                        &target,
                        &attributes,
                        method,
                        &args,
                    ) {
                        return result;
                    }
                    let attrs = attributes.to_map();
                    let (result, updated) = self.run_instance_method_at(
                        "generalcalldispatch",
                        &cls,
                        attrs,
                        method,
                        args,
                        Some(target.clone()),
                    )?;
                    // Propagate attribute mutations back to the inner instance so
                    // state changes made by the class method persist.
                    if let ValueView::Instance { attributes, .. } = inner.as_ref().view() {
                        attributes.commit_attrs(updated);
                    }
                    return Ok(result);
                }
            }
            // The default object representation names its type
            // (`Foo.new(x => 1)`), and raku names the *composition* there:
            // `Foo+{Bar}.new(x => 1)`. Delegating straight to `inner` renders
            // the bare base class, so retarget the leading type name to the
            // mixin-aware one that `.^name` already reports. Only the leading
            // occurrence is touched, and only when the delegate really did
            // start with the base type's own name (a class with a custom
            // `gist`/`raku`, or one gisting as its backing array/hash, is left
            // exactly as it rendered).
            if matches!(method, "gist" | "raku") && args.is_empty() {
                let rendered =
                    self.call_method_with_values_unviewed(inner.as_ref().clone(), method, args)?;
                let base = crate::value::types::what_type_name(inner.as_ref());
                // The name `.^name` reports: a `.^set_name` on the type object
                // this instance was built from (`Box[Int]`) wins over the
                // synthesized `Box+{BoxOf[Int]}`.
                let composed = self.mixin_instance_type_name(&target, inner, mixins)?;
                if composed != base
                    && let ValueView::Str(text) = rendered.view()
                    && let Some(tail) = text.strip_prefix(base.as_str())
                    && (tail.starts_with(".new") || tail.starts_with('<'))
                {
                    return Ok(Value::str(format!("{composed}{tail}")));
                }
                return Ok(rendered);
            }
            // `(Metamodel::SubsetHOW but Role).new_type(...)` keeps the role on
            // the minted type's HOW (`dispatch_instance_and_fallback`).
            if method == "new_type" && matches!(inner.view(), ValueView::Package(_)) {
                return self.dispatch_instance_and_fallback(target, method, args);
            }
            return self.call_method_with_values_unviewed(inner.as_ref().clone(), method, args);
        }

        // Instance dispatch, package dispatch, and fallback paths
        self.dispatch_instance_and_fallback(target, method, args)
    }

    /// Interpreter-aware `.Capture` for targets the pure `value_to_capture` can't
    /// handle: a `Channel`/`Supply` drains to a list first, and a List/Array/Seq
    /// holding a non-Str `Pair` key needs `.Str` (a custom-class key uses its
    /// `method Str`) to name it. Returns `None` for every other target (the pure
    /// native fast path handles those).
    pub(crate) fn try_interpreter_capture(
        &mut self,
        target: &Value,
    ) -> Option<Result<Value, RuntimeError>> {
        match target.view() {
            // The eager drain (`.list` pulls on demand, #12727).
            ValueView::Channel(ch) => Some(
                Self::channel_list_coercion(&ch, "List")
                    .and_then(|list| self.list_to_capture(&list)),
            ),
            ValueView::Instance { class_name, .. } if class_name.resolve() == "Supply" => Some(
                // `.Seq` taps the supply and collects its emissions (`.list` does
                // not run an on-demand `supply { ... }` block).
                self.call_method_with_values(target.clone(), "Seq", vec![])
                    .and_then(|seq| self.list_to_capture(&seq)),
            ),
            ValueView::Array(..) | ValueView::Seq(_) | ValueView::Slip(_) => {
                crate::builtins::methods_0arg::coercion::capture_needs_str_key(target)
                    .then(|| self.list_to_capture(target))
            }
            // `Mu.Capture` on a user-declared object: the named arguments are
            // the object's PUBLIC attributes, and each one is read through its
            // accessor *method*, so an explicit `method bar { … }` that
            // overrides the auto-generated accessor is what the Capture
            // reports. The pure `value_to_capture` dumped the raw attribute
            // store instead, which both ignored the override and leaked
            // private (`$!x`) attributes into the Capture.
            // Exact native IO::Handle instances carry additional native
            // settings outside the declared path attribute; their Capture
            // exposes the stored settings through the pure path.
            // An on-demand pull list (`Channel.list`, `IO::CatHandle.lines`)
            // has nothing cached until pulled: drain it first.
            ValueView::LazyList(ll) if ll.is_cat_pull() => Some(
                self.force_lazy_list(&ll)
                    .and_then(|items| self.list_to_capture(&Value::array(items))),
            ),
            ValueView::Instance { class_name, .. } if class_name == "IO::Handle" => None,
            ValueView::Instance { class_name, .. } => {
                let cn = class_name.resolve();
                let attrs = self.collect_class_attributes(&cn);
                let public: Vec<String> = attrs
                    .iter()
                    .filter(|a| a.is_public)
                    .map(|a| a.name.clone())
                    .collect();
                // A class with no declared public attributes (or a built-in
                // whose `.Capture` has its own spec — `Match`, `Duration`,
                // `IO::Path`, `Signature`, `Failure`, …) keeps the pure path.
                if public.is_empty() {
                    return None;
                }
                let mut named = ValueMap::default();
                for name in public {
                    match self.call_method_with_values(target.clone(), &name, vec![]) {
                        Ok(v) => {
                            named.insert(name, v);
                        }
                        Err(e) => return Some(Err(e)),
                    }
                }
                Some(Ok(Value::capture(vec![], named)))
            }
            _ => None,
        }
    }

    /// Build a Capture from a list: each `Pair` becomes a named argument (its key
    /// stringified via `.Str`, so a custom-class key uses its `method Str`), every
    /// other element a positional. Mirrors `List.Capture`.
    fn list_to_capture(&mut self, list: &Value) -> Result<Value, RuntimeError> {
        let items = Self::value_to_list(list);
        let mut positional = Vec::new();
        let mut named = ValueMap::default();
        for item in items {
            match item.view() {
                ValueView::Pair(k, v) => {
                    named.insert(k.clone(), v.clone());
                }
                ValueView::ValuePair(k, v) => {
                    let key_str = match k.view() {
                        ValueView::Str(s) => s.to_string(),
                        _ => self
                            .call_method_with_values(k.clone(), "Str", vec![])
                            .map(|s| s.to_string_value())
                            .unwrap_or_else(|_| k.to_string_value()),
                    };
                    named.insert(key_str, v.clone());
                }
                _ => positional.push(item.clone()),
            }
        }
        Ok(Value::capture(positional, named))
    }
}
