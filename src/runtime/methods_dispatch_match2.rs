use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Dispatch collection and iteration methods by name.
    /// Returns Some(result) if the method was handled, None to fall through.
    #[allow(clippy::too_many_lines)]
    pub(super) fn dispatch_method_by_name_2(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        // `Mu` is no `Any`: of the methods below, the `Mu` type object has
        // only `iterator` and `take` (`Mu.unique`, `Mu.minmax` and `Mu.List`
        // are `X::Method::NotFound` in Rakudo).
        if matches!(target.view(), ValueView::Package(name) if name == "Mu")
            && !matches!(method, "iterator" | "take")
        {
            return None;
        }
        // A role-punned Iterable keeps its iterator protocol in the composed
        // role, while the collection reduction methods are supplied by the
        // core Iterable surface rather than by the user's role itself. Drive
        // that iterator before applying the ordinary Array implementation;
        // otherwise a value such as Series does DataSlice reaches Mixin's
        // inner-instance fallback and .sum is reported as missing.
        if matches!(method, "sum" | "min" | "max" | "minmax")
            && matches!(target.view(), ValueView::Mixin(..))
            && self.mixin_composes_method(&target, "iterator")
        {
            let items = match self.drive_user_iterator_items(&target) {
                Ok(items) => items,
                Err(error) => return Some(Err(error)),
            };
            return Some(self.call_method_with_values(Value::array(items), method, args));
        }
        // `Array.list` is `self`, shaped or not (#14248); only a native shaped
        // array's `.list` is the flat List of its leaves.
        if method == "list"
            && args.is_empty()
            && crate::runtime::utils::is_shaped_array(&target)
            && !crate::builtins::method_table::is_native_array(&target)
        {
            return Some(Ok(target));
        }
        // `.List`/`.list`/`.Array` on a shaped array flattens all dimensions and
        // replaces uninitialized (`Nil`) slots with the container's type-default.
        if matches!(method, "List" | "list" | "Array")
            && args.is_empty()
            && let Some(flat) = self.shaped_flatten_with_default(&target)
        {
            let kind = if method == "Array" {
                crate::value::ArrayKind::Array
            } else {
                crate::value::ArrayKind::List
            };
            return Some(Ok(Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(flat)),
                kind,
            )));
        }
        None
    }

    /// Dispatch the "iterator" method.
    ///
    /// `pub(crate)` (not private): also called directly from
    /// `vm::vm_helpers_lazy::reify_or_consume_seq_target`'s `"iterator"`
    /// handling, which must build the FINAL iterator result in place of a
    /// still-`ValueView::Seq` target — see that call site's comment for why
    /// (the VM's own dispatch chain calls `reify_or_consume_seq_target`
    /// through more than one fallback layer for a single logical `.iterator`
    /// call; returning a still-Seq value lets the redundant layer re-run
    /// `take` on an already-`Taken` body and throw `X::Seq::Consumed` on the
    /// FIRST real call — surfaced by `roast/S32-list/squish.t`).
    pub(crate) fn dispatch_iterator_method(
        &mut self,
        target: Value,
    ) -> Result<Value, RuntimeError> {
        if matches!(target.view(), ValueView::Instance { class_name, .. } if class_name == "Iterator")
        {
            return Ok(target);
        }
        // Consumption is handled centrally, before dispatch reaches here
        // (ADR-0034 §2.3: `.iterator` is a consuming method AND one of the
        // two that burns a single read even on a body built already-
        // `Reified` — the `call_method_with_values` guard's
        // `reify_or_consume_seq_target` already claimed that and would have
        // thrown `X::Seq::Consumed` if it was already spent).
        if let ValueView::Seq(items) = target.view() {
            let seq_id = items.identity();
            if let Some(meta) = self.types.squish_iterator_meta.remove(&seq_id) {
                for key in meta.revert_remove {
                    self.env.remove(&key);
                }
                for (key, value) in meta.revert_values {
                    // A `ContainerRef`-celled `@`/`%` lexical must be reverted
                    // THROUGH its cell: the cell is the binding every alias (the
                    // `:as`/`:with` closures included) resolves to, so replacing
                    // the env entry with a plain container would both leave the
                    // eager pass's pushes standing in the cell and detach every
                    // alias from the name. See the snapshot's twin comment in
                    // `unique_squish.rs`.
                    if let Some(ValueView::ContainerRef(cell)) = self.env.get(&key).map(Value::view)
                    {
                        let cell = cell.clone();
                        *cell.lock().unwrap_or_else(|e| e.into_inner()) = value;
                    } else {
                        self.env.insert(key, value);
                    }
                }
                let mut attrs = HashMap::new();
                attrs.insert("squish_source".to_string(), Value::array(meta.source_items));
                attrs.insert("squish_as".to_string(), meta.as_func.unwrap_or(Value::NIL));
                attrs.insert(
                    "squish_with".to_string(),
                    meta.with_func.unwrap_or(Value::NIL),
                );
                attrs.insert("squish_scan_index".to_string(), Value::int(0));
                attrs.insert("squish_prev_key".to_string(), Value::NIL);
                attrs.insert("squish_initialized".to_string(), Value::FALSE);
                return Ok(Value::make_instance(Symbol::intern("Iterator"), attrs));
            }
        }
        // Pure construction (no env / no re-entry): shared with the VM's native
        // `.iterator` dispatch via the single `builtins::iterator_construct` impl.
        Ok(crate::builtins::iterator_construct::build_iterator_instance(&target))
    }

    /// Dispatch the "rotor" method: the `Any.rotor` row's handler and the
    /// cascade's arm.
    // Cost: O(e), e = elements of the invocant.
    pub(crate) fn dispatch_rotor_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        self.dispatch_rotor(target, &args)
    }

    /// Dispatch the "squish" method: the `Any.squish` row's handler and the
    /// cascade's arm.
    // Cost: O(e), e = elements of the invocant (one comparison each).
    pub(crate) fn dispatch_squish_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        self.dispatch_squish(target, &args)
    }

    /// Dispatch the "produce" method.
    pub(crate) fn dispatch_produce_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        let callable = match args.first().cloned() {
            Some(c) => c,
            None => return Some(Err(RuntimeError::new("produce expects a callable"))),
        };
        if !matches!(
            target.view(),
            ValueView::Array(_, _)
                | ValueView::Seq(_)
                | ValueView::Slip(_)
                | ValueView::LazyList(_)
                | ValueView::Range(_, _)
                | ValueView::RangeExcl(_, _)
                | ValueView::RangeExclStart(_, _)
                | ValueView::RangeExclBoth(_, _)
                | ValueView::GenericRange { .. }
                | ValueView::Hash(_)
        ) {
            return Some(Ok(target));
        }
        Some(self.call_function("produce", vec![callable, target]))
    }

    /// Dispatch the "reduce" method.
    pub(crate) fn dispatch_reduce_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let callable = args
            .first()
            .cloned()
            .ok_or_else(|| RuntimeError::new("reduce expects a callable"))?;
        // `.reduce` on an *undefined* invocant — a bare type object (`Range`,
        // `Str`, `Any`) or `Nil` — is `Nil`, because `Any.reduce` is guarded on a
        // defined invocant. (A type object passed as a list *element*, e.g.
        // `(Range,).reduce(&+)`, is a different thing and reduces normally.)
        if !crate::runtime::types::value_is_defined(&target) {
            return Ok(Value::NIL);
        }
        let items = Self::value_to_list(&target);
        self.reduce_items(callable, items)
    }

    /// Dispatch the "elems" method.
    pub(crate) fn dispatch_elems_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        // .elems on a Seq caches it (makes it available for multiple calls)
        if let ValueView::Seq(items) = target.view() {
            items.mark_cache_requested();
        }
        // `elems` takes no arguments. An argument-carrying call reaching here is
        // a MOP-shaped one (`$obj.HOW.elems($obj)`) that no HOW serves; the
        // `call_function` delegation below would bounce straight back into this
        // arm via `builtin_elems` (which is defined AS `$x.elems`) and recurse
        // until the stack overflowed. Decline instead, so the dispatcher reports
        // a real missing-method error.
        if !args.is_empty() {
            return None;
        }
        // The same cycle, on the receiver side. `call_function("elems", ..)`
        // reaches `builtin_elems`, which is DEFINED as `$x.elems`, so the
        // delegation below only terminates when that inner call is served by
        // the arity cascade -- and `native_fastpath_receiver_state_guard`
        // deliberately diverts EVERY `Instance` receiver's `.elems` away from
        // that cascade (so a `Supply` reaches the arm above). For any other
        // instance the bounce therefore came straight back here and recursed
        // until the stack overflowed: `elems(Date.new(2026,1,1))`,
        // `elems(C.new)` for a plain user class, and -- via the implicit-`*%_`
        // retry that re-enters this chain with the adverb removed --
        // `Blob.new(1,2,3).elems(:adverb)`. Answer an instance from the native
        // 0-arg layer directly, which is where the bounce was trying to arrive
        // (a buf-backed class reports its length, a `Stash` its symbol count,
        // anything else raku's `Any.elems` of 1).
        if matches!(target.view(), ValueView::Instance { .. }) {
            return crate::builtins::native_method_0arg(&target, Symbol::intern("elems"));
        }
        Some(self.call_function("elems", vec![target]))
    }

    /// Dispatch the "map" method.
    /// Iteration methods (`.map`/`.grep`) on a role mixin over a list-ish
    /// value iterate the INNER value's elements — including an itemized inner
    /// (`my $x = (1,2); $x does R; $x.map(...)` iterates 1, 2 in raku).
    /// A mixin over a scalar stays the single item. Without this,
    /// JSON::Marshal's `_marshal(Positional:D)` received the whole mixin back
    /// from `.map` and recursed to a stack overflow (JSON::Class
    /// t/050-array.t: `Array[TestObject] but JSON::Class`).
    pub(crate) fn mixin_iteration_target(target: Value) -> Value {
        if let ValueView::Mixin(inner, _) = target.view()
            && matches!(
                inner.view(),
                ValueView::Array(..)
                    | ValueView::Seq(_)
                    | ValueView::HyperSeq(_)
                    | ValueView::RaceSeq(_)
                    | ValueView::Slip(_)
                    | ValueView::LazyList(_)
                    | ValueView::Hash(_)
                    | ValueView::Range(..)
                    | ValueView::RangeExcl(..)
                    | ValueView::RangeExclStart(..)
                    | ValueView::RangeExclBoth(..)
                    | ValueView::GenericRange { .. }
                    | ValueView::Set(..)
                    | ValueView::Bag(..)
                    | ValueView::Mix(..)
            )
        {
            return inner.as_ref().clone();
        }
        target
    }

    /// Validate the callback shared by both `.map` dispatch routes.
    // Cost: O(1).
    pub(crate) fn validate_map_callable(
        func: Option<&Value>,
        target: &Value,
    ) -> Result<(), RuntimeError> {
        let using = func.map_or_else(|| "''".to_string(), |func| {
            format!("'{}'", crate::value::raku_repr::raku_value(func))
        });
        if let Some(func) = func {
        // Look through a role-mixed callable (`&foo but R1`) the same way
        // `call_sub_value` does when actually invoking it.
        let unwrapped = Self::unwrap_callable_mixin(func.clone());
        let is_callable = matches!(
            unwrapped.view(),
            ValueView::Sub(_)
                | ValueView::Routine { .. }
                | ValueView::WeakSub(_)
                | ValueView::Regex(_)
                | ValueView::RegexWithAdverbs(_)
        ) || (matches!(unwrapped.view(), ValueView::Instance { class_name, .. } if class_name.resolve() == "WhateverCode" || class_name.resolve() == "HyperWhateverCode"))
            || matches!(unwrapped.view(), ValueView::Whatever);
        if is_callable {
            return Ok(());
        }
        }
        let mut attrs = HashMap::new();
        let what = super::value_type_name(target);
        attrs.insert("what".to_string(), Value::str(what.to_string()));
        attrs.insert("using".to_string(), Value::str(using.clone()));
        attrs.insert("message".to_string(), Value::str(format!("Cannot map a {what} using {using}")));
        let ex = Value::make_instance(crate::symbol::Symbol::intern("X::Cannot::Map"), attrs);
        let mut err = RuntimeError::new(format!("X::Cannot::Map: Cannot map a {what} using {using}"));
        err.exception = Some(Box::new(ex));
        Err(err)
    }

    /// Cost: O(1) at the call on a non-shaped Array (read at pull time,
    /// `MapGrepItems::Live`); O(e) on any other invocant, e = elements
    /// (copied into the deferred `MapGrep` source). The callback runs at
    /// consumption: once per source element a prefix consumer needs
    /// (`.head(n)`, `.first`, boolification — `pull_map_grep_prefix`), or
    /// over every element for a full read. An infinite/lazy-pipe source
    /// (`make_lazy_pipe`) is O(1) per call and one callback per element
    /// pulled.
    pub(crate) fn dispatch_map_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let target = Self::mixin_iteration_target(target);
        Self::validate_map_callable(args.first(), &target)?;
        // A not-yet-run `.map`/`.grep` receiver: chain onto its source so the
        // two callbacks interleave per element (#11176). A map run inside a
        // gather body stays eager (see below), so it takes the receiver whole.
        let (chain, target) = self.map_grep_receiver_chain(target, self.gather_items_len() == 0)?;
        if let Some(items) = chain {
            return Ok(Value::seq_deferred(crate::value::SeqSource::MapGrep {
                items,
                pos: 0,
                func: args.first().cloned(),
                fatal: self.module.fatal_mode,
                mode: crate::value::MapGrepMode::Map,
                plan: Default::default(),
            }));
        }
        // Infinite/lazy source: return a truly lazy `map` pipeline stage instead
        // of materializing the (possibly infinite) source. Falls back to the
        // eager path for callbacks that need chunked binding (multi-arity /
        // slurpy). See `is_lazy_pipe_source` / `make_lazy_pipe`.
        if Self::is_lazy_pipe_source(&target)
            && let Some(func) = args.first().cloned()
            && let Some(pipe) = Self::make_lazy_pipe(target.clone(), func, false)
        {
            return Ok(pipe);
        }
        // A non-shaped Array is read at pull time rather than copied here
        // (`MapGrepItems::Live`), as Rakudo's `.map` iterates the Array
        // itself: `@a.map(&f).head(3)` must not copy `@a`.
        let live_array = matches!(target.view(), ValueView::Array(..))
            && !crate::runtime::utils::is_shaped_array(&target)
            && self.gather_items_len() == 0;
        let items = if live_array {
            Vec::new()
        } else if matches!(target.view(), ValueView::Mixin(..))
            && self.mixin_composes_method(&target, "iterator")
        {
            // A role-punned Iterable keeps its storage in the wrapped object;
            // the generic list converter cannot see that storage and would
            // map over the mixin itself. Drive the role's iterator just as
            // `for` does, preserving user-defined Iterable semantics.
            self.drive_user_iterator_items(&target)?
        } else if crate::runtime::utils::is_shaped_array(&target) {
            crate::runtime::utils::shaped_array_leaves(&target)
        } else {
            match target.view() {
                ValueView::Array(items, kind) if kind.is_itemized() => items.to_vec(),
                // A Blob/Buf maps over its bytes, and a Uni/NFC/NFD/NFKC/NFKD
                // over its codepoints (both match raku iteration; see
                // `value_to_list_for_receiver`'s doc comment for why `.map`
                // needs the receiver-decomposing variant here, not the plain
                // list-context `value_to_list`).
                _ => Self::buf_as_byte_items(&target)
                    .unwrap_or_else(|| crate::runtime::utils::value_to_list_for_receiver(&target)),
            }
        };
        // A map called while a gather body is being pulled must execute its
        // callback in that body. Deferring it to a Seq lets the gather body
        // finish before callbacks containing `take` run, so only the first
        // yielded value survives the coroutine suspension (Dan's
        // `sliced-slices` helper is the concrete shape). The surrounding
        // gather pull still bounds suspension and resumes the compiled frame.
        if self.gather_items_len() > 0 {
            let result = self.eval_map_over_items(args.first().cloned(), items)?;
            return Ok(Value::seq(crate::runtime::utils::value_to_list(&result)));
        }
        // A map callback has no observable invocation when its source is empty.
        // Returning an already-reified empty Seq avoids allocating the deferred
        // MapGrep source (its Arc, source vector and writeback holder) merely to
        // discover that there is nothing to pull. This stays after callability
        // validation above: `().map(42)` must still throw X::Cannot::Map.
        //
        // This matters for constructor TWEAKs such as the Zef::Distribution
        // shape in bench-ctor, where an empty attribute is normalized with
        // `@!resources.map(*.flat)` on every construction.
        let items = if live_array {
            crate::value::MapGrepItems::Live(target.clone())
        } else {
            crate::value::MapGrepItems::Snapshot(std::sync::Arc::new(items))
        };
        if items.len() == 0 {
            return Ok(Value::seq(Vec::new()));
        }
        // A `return` callback keeps the older `LazyList` deferral for now
        // (ADR-0058 step 4 retires it): that `return` targets the lexically
        // enclosing routine, and if the Seq is forced after that routine has
        // exited it must surface as `X::ControlFlow::Return` with
        // out-of-dynamic-scope set, which the `LazyList` path already gets
        // right.
        //
        // A `...` STUB body no longer takes it. The stub's only requirement
        // is "do not fire while the Seq is never iterated", which
        // `SeqSource::MapGrep` provides — and `...` is `fail`, so it also
        // needs the `use fatal` captured at the `.map` call, which the
        // `LazyList` route has no field for. That was the last thing keeping
        // `t/map-callback-runs-at-consumption.t`'s two Part-1 rows `todo`.
        // ADR-0058 step 2: `.map` returns a Seq whose callback has NOT run
        // yet. The callback runs when something consumes the Seq, through
        // ADR-0034's `reify`/`take`/`sink` split (`SeqSource::MapGrep`'s arm
        // in `pull_seq_source`), which is rakudo's timing: a `die` in the
        // callback escapes a `try` that merely encloses the `.map` call, and
        // a plain side effect happens at first consumption rather than here.
        // A real Array's elements are writable containers even when the Array
        // reached this dispatcher through a value-producing expression such as
        // `@a.list` or a scalar holding `[1, 2, 3]`. The named `@a.map` spelling
        // already carries this source through `MapGrepMode::MapRw`; preserve
        // the same source identity here so the deferred callback can publish
        // its `$_` writeback at consumption time. Immutable List values stay on
        // the plain `Map` path and retain their readonly-topic behavior.
        let mode = match target.view() {
            ValueView::Array(
                _,
                crate::value::ArrayKind::Array | crate::value::ArrayKind::ItemArray,
            ) => crate::value::MapGrepMode::MapRw(target.clone()),
            _ => crate::value::MapGrepMode::Map,
        };
        Ok(Value::seq_deferred(crate::value::SeqSource::MapGrep {
            items,
            pos: 0,
            func: args.first().cloned(),
            fatal: self.module.fatal_mode,
            mode,
            plan: Default::default(),
        }))
    }
    /// Dispatch "min" and "max" methods.
    pub(crate) fn dispatch_min_max_method(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // Range.min/.max with adverbs (`:k`/`:kv`/`:p`/`:v`): a Range's extremum
        // is always its first (min) / last (max) element, so report the index
        // analytically rather than materialising (which can't handle `n..Inf`).
        // Only the bare-adverb form (no `:by` comparator) is special-cased.
        let has_by = args.iter().any(|a| {
            matches!(
                a.view(),
                ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
            ) || matches!(a.view(), ValueView::Pair(n, _) if n == "by")
        });
        if Self::value_is_rangey(&target) && !has_by {
            let (adverb, _) = Self::extract_extrema_adverbs(&args);
            // The extremum value reuses the (correct, Inf-aware) 0-arg path.
            let want_max = method == "max";
            let value = self.call_method_with_values(target.clone(), method, vec![])?;
            let Some(adverb) = adverb else {
                return Ok(value);
            };
            let key = if want_max {
                // Last element's index = elems - 1 (Inf stays Inf).
                let elems = self.call_method_with_values(target, "elems", vec![])?;
                match elems.view() {
                    ValueView::Int(n) => Value::int(n - 1),
                    ValueView::Num(f) if f.is_infinite() => elems.clone(),
                    _ => Value::num(f64::INFINITY),
                }
            } else {
                Value::int(0)
            };
            return Ok(match adverb.as_str() {
                "k" => key,
                "v" => value,
                "kv" => Value::array(vec![key, value]),
                "p" => Value::value_pair(key, value),
                _ => value,
            });
        }
        let mut call_args = vec![self.quanthash_extremum_source(target)?];
        if let Some(first) = args.first() {
            if matches!(
                first.view(),
                ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
            ) {
                call_args.push(Value::pair("by".to_string(), first.clone()));
            } else {
                call_args.extend(args.clone());
            }
        }
        if method == "max" {
            self.builtin_max(&call_args)
        } else {
            self.builtin_min(&call_args)
        }
    }

    /// What `min`/`max`/`minmax` order for `target`. A `Set`/`Bag`/`Mix` (and
    /// their mutable twins) is a collection of `key => weight` pairs, so its
    /// extremum is the largest / smallest `Pair` (`(bag <a b b>).max` is
    /// `:b(2)`), not the collection itself; anything else orders as it is.
    // Cost: O(n), n = elements of a QuantHash; O(1) otherwise.
    fn quanthash_extremum_source(&mut self, target: Value) -> Result<Value, RuntimeError> {
        if matches!(
            target.view(),
            ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..)
        ) {
            return self.call_method_with_values(target, "pairs", Vec::new());
        }
        Ok(target)
    }

    /// Dispatch the "sort" method.
    pub(crate) fn dispatch_sort_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let ValueView::Package(name) = target.view()
            && name == "Supply"
        {
            return Ok(Value::seq(vec![target]));
        }
        // A role mixin over a list-ish value sorts the inner elements, the same
        // way `.map`/`.grep` iterate them. Without this the `Mixin` view fell
        // through `sort_value_generic`'s final "any non-list value sorts as a
        // one-element list of itself" arm, so `(%h but R).sort` answered a
        // 1-element list holding the whole unsorted hash.
        let target = Self::mixin_iteration_target(target);
        let mut caller = crate::runtime::methods_collection_ops::sort::InterpCaller(self);
        crate::runtime::methods_collection_ops::sort::sort_value_generic(&mut caller, target, &args)
    }

    /// Dispatch the "minmax" method.
    pub(crate) fn dispatch_minmax_method(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        if let ValueView::Instance { class_name, .. } = target.view()
            && class_name == "Supply"
        {
            if !args.is_empty() {
                return Some(self.dispatch_supply_transform(target, method, &args));
            }
            // Fall through for 0-arg Supply.minmax
            return None;
        }
        let target = self.quanthash_extremum_source(target).ok()?;
        let mut call_args = vec![target];
        if let Some(first) = args.first() {
            if matches!(
                first.view(),
                ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
            ) {
                call_args.push(Value::pair("by".to_string(), first.clone()));
            } else {
                call_args.extend(args.clone());
            }
        }
        Some(self.builtin_minmax(&call_args))
    }
}
