use super::*;
use crate::value::ValueView;
use crate::value::types::is_stash_class_name;

/// The methods `dispatch_method_by_name_3` answers from a late row of the
/// receiver's MRO (ADR-11276 §9.139).
const MATCH3_LATE_BY_MRO: [&str; 11] = [
    "Numeric",
    "run",
    "Date",
    "DateTime",
    "Str",
    "Stringy",
    "keys",
    "values",
    "AT-KEY",
    "EXISTS-KEY",
    "EXISTS-POS",
];

impl Interpreter {
    /// Dispatch remaining methods by name (Supply, networking, temporal, misc).
    /// Returns Some(result) if the method was handled, None to fall through.
    #[allow(clippy::too_many_lines)]
    pub(super) fn dispatch_method_by_name_3(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        // `Date.today`, `Date.new-from-daycount` and `DateTime.now` are rows of
        // the method table, found by their owner for the type object of the
        // class or a subclass (ADR-11276 §9.123).
        if let Some(owners) = self.temporal_class_owners(&target)
            && let Some(result) = crate::builtins::method_table::invoke_owner(
                self,
                owners,
                method,
                &args,
                || target.clone(),
            )
        {
            return Some(result);
        }
        // The late rows of the receiver's MRO (ADR-11276 §9.139): a `Seq`'s
        // `Numeric`, the temporal type objects' coercions, `Thread.run`, a
        // list's or a regex's `Str`, and the hash-likes' `keys`/`values`/
        // `AT-KEY`/`EXISTS-*`. `Mu`'s late rows are the cascade's last resort,
        // reached only after the user methods this step precedes (§9.138).
        if MATCH3_LATE_BY_MRO.contains(&method) {
            let mro = self.classhow_mro_unhidden_names(&target);
            let owners: Vec<&str> = mro
                .iter()
                .map(String::as_str)
                .filter(|owner| *owner != "Mu")
                .collect();
            return crate::builtins::method_table::invoke_mro_late(
                self, &owners, method, &args, &target,
            );
        }
        None
    }

    /// Dispatch the "grep" method: the `Any.grep` row's handler and the
    /// cascade's arm (ADR-11276 slice 3C remainder).
    // Cost: O(1) at the call on a lazy or live source; O(e) on any other
    // invocant, e = elements, one matcher call each.
    pub(crate) fn dispatch_grep_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        // `Supply.grep` on a *live* (Supplier-backed) supply must stay
        // live: register a filter transform tap that forwards matching
        // values to a derived supply. A materialized supply
        // (`Supply.from-list`, on-demand) falls through to `dispatch_grep`
        // below, which already filters its buffered values with correct
        // smart-match semantics (grep(Int)/grep(/rx/) etc.).
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
            && class_name == "Supply"
            && crate::runtime::native_methods::supplier_id_from_attrs(&attributes.as_map())
                .is_some()
        {
            let matcher = args.first().cloned().unwrap_or(Value::NIL);
            if let Some(live) = self.make_live_transform_supply(
                &attributes.as_map(),
                matcher,
                crate::runtime::native_methods::TransformMode::Grep,
            ) {
                return Some(Ok(live));
            }
        }
        // In Raku, `.grep` always returns a `Seq` — including over an
        // Array. `dispatch_grep` builds a `List`-kind array whose elements
        // are the matched source slots as shared `ContainerRef` cells (so a
        // writeback loop `for @a.grep(...) { $_++ }` still mutates `@a`
        // through them); wrapping those same cells in a `Seq` preserves the
        // writeback while giving the correct `Seq` type.
        Some(self.dispatch_grep(target, &args).map(|v| {
            if let ValueView::Array(items, crate::value::ArrayKind::List) = v.view() {
                return Value::seq(items.to_vec());
            }
            v
        }))
    }

    /// Dispatch the "first" method: the `Any.first` rows' handler and the
    /// cascade's arms.
    // Cost: O(e) matcher calls, e = elements scanned until the first hit.
    pub(crate) fn dispatch_first_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        if !args.is_empty() {
            return Some(self.dispatch_first(target, &args));
        }
        // `.first` with no matcher is the first element of the invocant's
        // list (`Any.first` smartmatches `True`).
        Some(Ok(crate::builtins::method_table::terminals::first_one(&target)))
    }

    /// Dispatch the zero-argument "eager": the `Any.eager` row's handler and
    /// the cascade's arm.
    // Cost: O(e), e = elements of the invocant.
    pub(crate) fn dispatch_eager_row(
        &mut self,
        target: Value,
    ) -> Option<Result<Value, RuntimeError>> {
        crate::builtins::method_table::list::eager(&target, &[])
            .or_else(|| Some(self.dispatch_eager_method(target)))
    }

    /// `.grab` on the `Supply` type object (`Supply.grab` of an instance is a
    /// row, ADR-11276 §9.130; the QuantHash `grab` is a `Handler::Mut` row,
    /// §9.23).
    pub(crate) fn dispatch_grab_method(
        &mut self,
        target: Value,
        _args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        if let ValueView::Package(class_name) = target.view()
            && class_name == "Supply"
        {
            return Some(Err(RuntimeError::new(
                "Cannot call .grab on a Supply type object",
            )));
        }
        None
    }

    /// Dispatch the "skip" method.
    ///
    /// Cost: O(r + s), r = elements produced, s = skip/produce specs, on an
    /// Array, a List or a reified Seq (the invocant is borrowed and only the
    /// produced spans are cloned); O(e), e = elements, on any other list-like.
    pub(crate) fn dispatch_skip_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // Consumption is handled centrally, before dispatch reaches here
        // (ADR-0034 §2.3: `.skip` is a consuming method AND one of the two
        // that burns a single read even on a body built already-`Reified` —
        // the `call_method_with_values` guard's `reify_or_consume_seq_target`
        // already claimed that and would have thrown `X::Seq::Consumed` if
        // it was already spent).
        // The skip/produce specs are resolved against the length alone and
        // turned into index spans; only the produced items are cloned out of
        // the borrowed invocant at the end.
        let len = crate::runtime::utils::list_items_len(&target);
        let collect_spans = |spans: &[(usize, usize)]| {
            Value::seq(crate::runtime::utils::with_list_items(&target, |items| {
                // A spec callback may have shrunk the invocant since `len`
                // was read, so clamp to what is there now.
                let n = items.len();
                spans
                    .iter()
                    .flat_map(|&(a, b)| items[a.min(n)..b.min(n)].iter().cloned())
                    .collect()
            }))
        };
        if args.len() == 1 && matches!(args[0].view(), ValueView::Sub(..)) {
            // Callable arg: call with list length to get actual skip count
            let len_arg = Value::int(len as i64);
            let result = self.call_sub_value(args[0].clone(), vec![len_arg])?;
            let n = result.to_f64().max(0.0) as usize;
            return Ok(collect_spans(&[(n, len)]));
        }

        // In 6.e, multiple arguments alternate between a number of values to
        // produce and a number of values to skip. A Seq/Slip argument is already a
        // flattened argument stream; expand it here as well so an unbounded
        // repeat such as `|(2, 3) xx *` can provide its cached prefix without
        // being mistaken for one numeric argument.
        let mut specs = Vec::new();
        for arg in args {
            match arg.view() {
                // A lazy (possibly infinite) spec stream (`|(2, 3) xx *`) is
                // pulled only as far as the invocant can use: every
                // produce/skip pair covers at least one element unless both
                // counts are zero, so twice the element count is enough.
                ValueView::LazyList(ll) if ll.is_genuinely_lazy() => {
                    for i in 0..len.saturating_mul(2).saturating_add(2) {
                        match self.pull_source_element(&arg, i)? {
                            Some(v) => specs.push(v),
                            None => break,
                        }
                    }
                }
                ValueView::Seq(_) | ValueView::LazyList(_) | ValueView::Slip(_) => {
                    specs.extend(crate::runtime::utils::value_to_list(&arg));
                }
                _ => specs.push(arg),
            }
        }
        if specs.is_empty() {
            specs.push(Value::int(1));
        }

        if specs.len() > 1 && !crate::parser::current_language_version().starts_with("6.e") {
            return Err(super::methods_signature_errors::make_multi_no_match_error(
                "skip",
            ));
        }

        let mut cursor = 0usize;
        let mut spans: Vec<(usize, usize)> = Vec::new();
        // The one-argument forms retain their original skip-first meaning.
        let mut skipping = specs.len() == 1;
        for spec in specs {
            let count = match spec.view() {
                // `Whatever` in a skip position discards the rest; in a
                // produce position it keeps the rest. Either way, no later
                // argument can affect the result.
                ValueView::Whatever | ValueView::HyperWhatever => {
                    if !skipping {
                        spans.push((cursor, len));
                    }
                    cursor = len;
                    break;
                }
                _ => spec.to_f64().max(0.0) as usize,
            };
            let end = cursor.saturating_add(count).min(len);
            if !skipping {
                spans.push((cursor, end));
            }
            cursor = end;
            skipping = !skipping;
            if cursor == len {
                break;
            }
        }

        // The tail continues in the phase following the final count.
        if cursor < len && !skipping {
            spans.push((cursor, len));
        }
        Ok(collect_spans(&spans))
    }

    /// Dispatch `.Str`/`.Stringy` on a list whose elements need the
    /// interpreter: an `Instance` element may define its own `Str`, which the
    /// pure `to_string_value` renderer cannot call (it would print the
    /// `C()` fallback instead). The native fast path returns `None` for
    /// exactly those lists (`dispatch_core_coerce`), so anything reaching
    /// here is one.
    ///
    /// The elements are resolved in place and the list is then handed to the
    /// SAME pure renderer, so the list-shape rules (space separation, nested
    /// flattening) stay in one place -- only the per-element stringification
    /// moves.
    pub(crate) fn dispatch_list_str_method(&mut self, target: Value) -> Option<Result<Value, RuntimeError>> {
        if !Self::list_str_needs_interpreter(&target) {
            return None;
        }
        let resolved = match self.resolve_list_element_stringifiers(&target) {
            Ok(v) => v,
            Err(e) => return Some(Err(e)),
        };
        Some(Ok(Value::str(resolved.to_string_value())))
    }

    /// Dispatch the "join" method.
    pub(crate) fn dispatch_join_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        if crate::builtins::is_join_lazy(&target) {
            return Some(Ok(Value::str("...".to_string())));
        }
        // `Any.join` is `self.list.join`: an instance, a type object or `Nil`
        // is a one-element list, whose element is stringified below with its
        // own `Str` (a user method, or the type object's warning). The `Any.join`
        // row (`method_table::collections::join`) declines these to here.
        let one_element = matches!(
            target.view(),
            ValueView::Instance { .. } | ValueView::Package(_) | ValueView::Nil
        );
        if !one_element
            && !matches!(
                target.view(),
                ValueView::Array(..)
                    | ValueView::Seq(..)
                    | ValueView::Slip(..)
                    | ValueView::Range(..)
                    | ValueView::RangeExcl(..)
                    | ValueView::RangeExclStart(..)
                    | ValueView::RangeExclBoth(..)
                    | ValueView::GenericRange { .. }
            )
        {
            return None;
        }
        let sep = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let items = if one_element {
            vec![target.clone()]
        } else if crate::runtime::utils::is_shaped_array(&target) {
            crate::runtime::utils::shaped_array_leaves(&target)
        } else if let Some(inner) = target.as_list_items() {
            inner.to_vec()
        } else {
            Self::value_to_list(&target)
        };
        let (variable_name, sparse_holes) = match target.view() {
            ValueView::Array(data, _) if !crate::runtime::utils::is_shaped_array(&target) => (
                data.descriptor_name.as_deref().map(str::to_owned),
                Some((0..items.len()).map(|index| data.hole_at(index)).collect::<Vec<_>>()),
            ),
            ValueView::Array(data, _) => (data.descriptor_name.as_deref().map(str::to_owned), None),
            _ => (None, None),
        };
        let mut warned_sparse_hole = false;
        // Resolve every non-Junction element to its final Str value up front
        // (Instance/Mixin through user `.Str`, same as before); a Junction
        // element is left as-is so `thread_junctions_in_items` below can
        // thread the whole `join` over its eigenstates
        // (`("a"|"b","c","d").join` => `any(acd, bcd)`) instead of
        // stringifying it in place.
        let mut resolved = Vec::with_capacity(items.len());
        for (index, v) in items.iter().enumerate() {
            // Decontainerize a `ContainerRef` element (grep rw alias / `:=`-bound
            // slot) so a cell-wrapped Instance still gets its user-defined `.Str`.
            let v = v.deref_container();
            // Look through itemization too: an element assigned from a scalar
            // arrives itemized (`my @h = $x` compiles an `ItemizeVar`), and
            // `.join` stringifies each element with `.Str`, whose dispatch
            // deconts its invocant. Without this `my @h = $c` (an `is Array`
            // subclass instance) joined to the `SA()` fallback.
            let v = v.descalarize().clone();
            if matches!(v.view(), ValueView::Nil) {
                let resumed = match self.raise_resumable_warning(
                    "Use of Nil in string context",
                    Value::str(String::new()),
                ) {
                    Ok(value) => value,
                    Err(error) => return Some(Err(error)),
                };
                resolved.push(resumed);
                continue;
            }
            if let ValueView::Package(name) = v.view() {
                let type_name = name.resolve();
                if type_name != "Mu" && !self.has_user_method(&type_name, "Str") {
                    let is_sparse_hole = sparse_holes
                        .as_ref()
                        .and_then(|holes| holes.get(index))
                        .copied()
                        .unwrap_or(false);
                    if is_sparse_hole && warned_sparse_hole {
                        resolved.push(Value::str(String::new()));
                        continue;
                    }
                    match self.warn_type_object_string_context_named(
                        &type_name,
                        (!is_sparse_hole)
                            .then_some(variable_name.as_deref())
                            .flatten(),
                        false,
                    ) {
                        Ok(value) => resolved.push(value),
                        Err(error) => return Some(Err(error)),
                    }
                    warned_sparse_hole |= is_sparse_hole;
                    continue;
                }
                match self.call_method_with_values(v.clone(), "Str", vec![]) {
                    Ok(value) => {
                        resolved.push(Value::str(value.to_string_value()));
                        continue;
                    }
                    Err(error) => return Some(Err(error)),
                }
            }
            // A `Str` subclass element already is a `Str`: rakudo's join takes
            // it as is (its payload), not through its own `.Str` (#11026).
            if let Some(payload) = crate::runtime::str_subclass_payload(&v) {
                resolved.push(payload);
            } else if matches!(v.view(), ValueView::Instance { .. } | ValueView::Mixin(..))
                // A deferred inner `.map` Seq (an outer map's block returned
                // `(3,4).map({...})`) has not run its callback yet; its `.Str`
                // runs it, where `to_str_context` would read the empty seed.
                || matches!(v.view(), ValueView::Seq(s) if s.awaits_vm_reify())
            {
                let s = match self.call_method_with_values(v.clone(), "Str", vec![]) {
                    Ok(s) => s,
                    Err(e) => return Some(Err(e)),
                };
                resolved.push(Value::str(s.to_string_value()));
            } else if (v.as_list_items().is_some() || matches!(v.view(), ValueView::Hash(_)))
                && crate::value::gist::str_needs_dispatch(&v)
            {
                // A nested list/hash holding an instance (Terminal::Print's
                // grid rows of `Cell`s): its `.Str` reaches the elements' own
                // `.Str`, which `to_str_context` cannot run.
                let s = match self.call_method_with_values(v.clone(), "Str", vec![]) {
                    Ok(s) => s,
                    Err(e) => return Some(Err(e)),
                };
                resolved.push(Value::str(s.to_string_value()));
            } else {
                resolved.push(v);
            }
        }
        // A zero-denominator Rational element dies like its own `.Str`
        // (GH #9621) instead of rendering `Inf`.
        if let Some(err) = resolved
            .iter()
            .find_map(crate::runtime::utils::zero_denominator_rational_error)
        {
            return Some(Err(err));
        }
        if let Some(threaded) = crate::builtins::thread_junctions_in_items(&resolved, &|c| {
            Value::str(
                c.iter()
                    .map(|v| v.to_str_context())
                    .collect::<Vec<_>>()
                    .join(&sep),
            )
        }) {
            return Some(Ok(threaded));
        }
        let joined = resolved
            .iter()
            .map(|v| v.to_str_context())
            .collect::<Vec<_>>()
            .join(&sep);
        Some(Ok(Value::str(joined)))
    }

    /// Dispatch the "eager" method.
    fn dispatch_eager_method(&mut self, target: Value) -> Result<Value, RuntimeError> {
        if let ValueView::LazyList(list) = target.view() {
            // A single-pass `.lazy` Seq (docs/adr/0034's consumption model,
            // extended in `claim_lazy_seq_touch`'s doc comment) throws on a
            // second `.eager` instead of silently re-answering (#9789).
            crate::value::claim_lazy_seq_touch(&list)?;
            // Route through the VM force path (falls back to the interpreter
            // bridge itself when there is no compiled code): a prior BOUNDED
            // pull (e.g. `$s[0]`) can leave a gather coroutine suspended
            // mid-body with `list.cache` holding only a prefix, and only the
            // VM path's coroutine-aware resume knows how to continue it to
            // completion instead of re-running the whole body from scratch or
            // returning the partial cache as if it were complete (#8512).
            let items = self.force_lazy_list_vm(&list)?;
            // A lazy list that was assigned into an `@` variable IS that
            // array's element store, so forcing it yields an Array
            // (`my @a = lazy {...}; say @a.eager` is `[0 1 4]` in raku, not the
            // List gist `(0 1 4)`). A bare lazy Seq stays a List.
            return Ok(if list.in_array_context() {
                Value::real_array(items)
            } else {
                Value::array(items)
            });
        }
        if matches!(target.view(), ValueView::Array(..)) {
            let (items, kind) = target.into_array().unwrap();
            if items
                .iter()
                .any(|v| matches!(v.view(), ValueView::LazyList(_)))
            {
                // `.eager` reifies a lazy TAIL preserved inside the array (a
                // `|(lazy gather …)` slipped into `@a` sits as a `LazyList`
                // element). Force each such element so its deferred body/side
                // effects run now, flattening the produced values in place.
                let mut out = Vec::with_capacity(items.len());
                for it in items.iter() {
                    if let ValueView::LazyList(ll) = it.view() {
                        out.extend(self.force_lazy_list_bridge(&ll)?);
                    } else {
                        out.push(it.clone());
                    }
                }
                return Ok(Value::array_with_kind(
                    crate::gc::Gc::new(crate::value::ArrayData::new(out)),
                    kind,
                ));
            }
            return Ok(Value::array_with_kind(items, kind));
        }
        if let ValueView::Seq(items) = target.view() {
            // `Seq.eager` reifies the Seq into a List (rakudo: `.eager.WHAT` is List).
            Ok(Value::array(items.to_vec()))
        } else if matches!(target.view(), ValueView::Slip(..)) {
            Ok(target)
        } else if matches!(
            target.view(),
            ValueView::Range(..)
                | ValueView::RangeExcl(..)
                | ValueView::RangeExclStart(..)
                | ValueView::RangeExclBoth(..)
                | ValueView::GenericRange { .. }
        ) {
            Ok(Value::array(crate::runtime::utils::value_to_list(&target)))
        } else {
            // A plain scalar interprets itself as a single-element List.
            Ok(Value::array(vec![target]))
        }
    }

    /// Dispatch the "is-lazy" method.
    /// Cost: O(e) on a list/array, e = elements of the invocant (each element is
    /// checked for a lazy tail); O(1) on a LazyList or Range. Plain `@a.is-lazy`
    /// is answered O(1) by the native arm in `dispatch_core_str.rs` first.
    pub(crate) fn dispatch_is_lazy_method(&self, target: &Value) -> Value {
        let value_is_lazy = |v: &Value| match v.view() {
            ValueView::LazyList(list) => list.is_genuinely_lazy(),
            ValueView::Array(_, kind) if kind.is_lazy() => true,
            ValueView::Range(..)
            | ValueView::RangeExcl(..)
            | ValueView::RangeExclStart(..)
            | ValueView::RangeExclBoth(..)
            | ValueView::GenericRange { .. } => crate::builtins::is_infinite_range(v),
            _ => false,
        };
        let is_lazy = if value_is_lazy(target) {
            true
        } else if let Some(items) = target.as_list_items() {
            items.iter().any(value_is_lazy)
        } else {
            false
        };
        Value::truth(is_lazy)
    }

    /// Dispatch "keys" method.
    pub(crate) fn dispatch_keys_method(&mut self, target: Value) -> Option<Result<Value, RuntimeError>> {
        match target.view() {
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if is_stash_class_name(class_name.as_str()) => {
                let keys = match attributes.as_map().get("symbols").map(Value::view) {
                    Some(ValueView::Hash(map)) => {
                        map.keys().cloned().map(Value::str).collect::<Vec<Value>>()
                    }
                    _ => Vec::new(),
                };
                Some(Ok(Value::seq(keys)))
            }
            ValueView::Hash(map) => {
                if let Some(info) = self.container_type_metadata(&target)
                    && info.key_type.is_some()
                {
                    // Object hash: use the original key objects embedded in the
                    // hash's `HashData` (single source, COW-stable).
                    if let Some(ref orig) = utils::hash_original_keys_snapshot(&target)
                        && !orig.is_empty()
                    {
                        let keys: Vec<Value> = map
                            .keys()
                            .map(|k| utils::hash_typed_key(&target, k))
                            .collect();
                        return Some(Ok(Value::seq(keys)));
                    }
                    // Fallback: try coercion from string key
                    let key_constraint = info.key_type.unwrap();
                    let mut keys = Vec::with_capacity(map.len());
                    for key in map.keys() {
                        let key_value = Value::str(key.clone());
                        let coerced = self
                            .try_coerce_value_for_constraint(&key_constraint, key_value)
                            .unwrap_or_else(|_| Value::str(key.clone()));
                        keys.push(coerced);
                    }
                    return Some(Ok(Value::seq(keys)));
                }
                let keys = map.keys().cloned().map(Value::str).collect::<Vec<Value>>();
                Some(Ok(Value::seq(keys)))
            }
            ValueView::Mixin(inner, _) if matches!(inner.as_ref().view(), ValueView::Hash(_)) => {
                Some(self.call_method_with_values_unviewed(inner.as_ref().clone(), "keys", vec![]))
            }
            _ => None,
        }
    }

    /// Dispatch "values" method.
    /// Cost: O(e), e = elements (or hash values) of the invocant, copied into a Seq.
    pub(crate) fn dispatch_values_method(&self, target: Value) -> Result<Value, RuntimeError> {
        match target.view() {
            // `Foo::.values` — the stash's symbol values (enum members, nested
            // packages, ...), mirroring the Stash arm of dispatch_keys_method.
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if is_stash_class_name(class_name.as_str()) => {
                let vals = match attributes.as_map().get("symbols").map(Value::view) {
                    Some(ValueView::Hash(map)) => map.values().cloned().collect(),
                    _ => Vec::new(),
                };
                Ok(Value::seq(vals))
            }
            ValueView::Capture { positional, named } => {
                let mut vals = positional.clone();
                vals.extend(named.values().cloned());
                Ok(Value::seq(vals))
            }
            ValueView::Array(items, ..) => Ok(Value::seq(items.to_vec())),
            // A List over a gather still being pulled is its own `.values`
            // (ADR-11678): it reifies from the coroutine as it is read.
            ValueView::Seq(body) if body.is_gather_source() => Ok(target.clone()),
            ValueView::Hash(map) => Ok(Value::seq(map.values().cloned().collect())),
            ValueView::Pair(_, value) => Ok(Value::seq(vec![value.clone()])),
            ValueView::ValuePair(_, value) => Ok(Value::seq(vec![value.clone()])),
            _ => Ok(Value::seq(Vec::new())),
        }
    }

    /// Dispatch "EXISTS-KEY" on a Match (does the match have the named capture?)
    /// or a Stash (does the package have the named symbol?). `CORE::.EXISTS-KEY`
    /// is used by dists (e.g. head-skip-tail) to detect a symbol clash before
    /// exporting. Returns None for other targets so they fall through.
    pub(crate) fn dispatch_match_exists_key(
        &self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
        {
            if target.is_match_instance() {
                let key = args[0].to_string_value();
                let named_v = target.match_named();
                let exists = matches!(
                    named_v.as_ref().map(Value::view),
                    Some(ValueView::Hash(named)) if named.contains_key(key.as_str())
                );
                return Some(Ok(Value::truth(exists)));
            }
            if is_stash_class_name(class_name.as_str()) {
                // Exact-key membership only — unlike the Stash `AT-KEY` arm, raku's
                // `EXISTS-KEY` does NOT sigil-fallback (`Foo::.EXISTS-KEY('baz')` is
                // False even when `$baz` exists). Dists query the full sigil'd name
                // (`CORE::.EXISTS-KEY('&head')`).
                let exists = matches!(
                    attributes.as_map().get("symbols").map(Value::view),
                    Some(ValueView::Hash(symbols))
                        if symbols.contains_key(args[0].to_string_value().as_str())
                );
                return Some(Ok(Value::truth(exists)));
            }
        }
        None
    }

    /// Dispatch "EXISTS-POS" on a Match: is the positional capture index in range?
    /// Returns None for non-Match targets so they fall through to other handlers.
    pub(crate) fn dispatch_match_exists_pos(
        &self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if target.is_match_instance() {
            let idx = crate::runtime::to_int(&args[0]);
            let list_v = target.match_list();
            let exists = idx >= 0
                && matches!(
                    list_v.as_ref().map(Value::view),
                    Some(ValueView::Array(items, ..)) if (idx as usize) < items.len()
                );
            return Some(Ok(Value::truth(exists)));
        }
        None
    }

    /// Dispatch "AT-KEY" method.
    // Cost: O(k * b), k = indexed keys (1 for a scalar key), b = bytes per key;
    // each Stash symbol-table lookup is an expected constant-time hash probe.
    pub(crate) fn dispatch_at_key_method(
        &self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        match (target.view(), &args[0]) {
            (
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                },
                idx,
            ) if is_stash_class_name(class_name.as_str()) => {
                let missing = if class_name.as_str() == "Stash" {
                    Value::package(crate::symbol::wk::any())
                } else {
                    Value::NIL
                };
                if let Some(ValueView::Hash(symbols)) =
                    attributes.as_map().get("symbols").map(Value::view)
                {
                    let stash_lookup = |raw_key: &str| {
                        if let Some(value) = symbols.get(raw_key) {
                            return Some(value.clone());
                        }
                        if !raw_key.starts_with('$')
                            && !raw_key.starts_with('@')
                            && !raw_key.starts_with('%')
                            && !raw_key.starts_with('&')
                        {
                            let scalar = format!("${raw_key}");
                            if let Some(value) = symbols.get(&scalar) {
                                return Some(value.clone());
                            }
                        }
                        None
                    };

                    if let ValueView::Array(items, ..) = idx.view() {
                        let values = items
                            .iter()
                            .map(|item| {
                                let key = item.to_string_value();
                                stash_lookup(&key).unwrap_or_else(|| missing.clone())
                            })
                            .collect::<Vec<_>>();
                        return Some(Ok(Value::array(values)));
                    }

                    let key = idx.to_string_value();
                    if let Some(value) = stash_lookup(&key) {
                        return Some(Ok(value));
                    }
                    // CORE::/SETTING:: expose the (huge) core symbol table, which
                    // is not materialized into the stash. Resolve a missing key
                    // lazily so `CORE::.<&not>` / `::("CORE")::('&not')` find core
                    // routines. Qualify with the stash name so a user shadow of the
                    // same name is bypassed (roast: `CORE::.<&not>` when shadowed).
                    // TODO: extend to non-code core symbols ($ vars, type objects).
                    if let Some(ValueView::Str(stash_name)) =
                        attributes.as_map().get("name").map(Value::view)
                        && (stash_name.as_str() == "CORE" || stash_name.as_str() == "SETTING")
                        && let Some(bare) = key.strip_prefix('&')
                    {
                        let v = self.resolve_code_var(&format!("{}::{bare}", stash_name.as_str()));
                        if !v.is_nil() {
                            return Some(Ok(v));
                        }
                    }
                }
                Some(Ok(missing))
            }
            (ValueView::Pair(key, value), idx) => {
                if key == &idx.to_string_value() {
                    Some(Ok(value.clone()))
                } else {
                    Some(Ok(Value::NIL))
                }
            }
            (ValueView::ValuePair(key, value), idx) => {
                if key.to_string_value() == idx.to_string_value() {
                    Some(Ok(value.clone()))
                } else {
                    Some(Ok(Value::NIL))
                }
            }
            // `$match.AT-KEY("name")` — named-capture lookup, mirroring the
            // `$match<name>` postcircumfix. Routed here by hyper method calls such
            // as `@<chars>».<spaces>`, which dispatch `.AT-KEY` per element.
            (ValueView::Instance { .. }, idx) if target.is_match_instance() => {
                let key = idx.to_string_value();
                let named_v = target.match_named();
                if let Some(ValueView::Hash(named)) = named_v.as_ref().map(Value::view) {
                    Some(Ok(named.get(key.as_str()).cloned().unwrap_or(Value::NIL)))
                } else {
                    Some(Ok(Value::NIL))
                }
            }
            _ => None,
        }
    }
}
