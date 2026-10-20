use super::*;
use crate::value::ValueView;
use std::collections::HashMap as StdHashMap;

impl Interpreter {
    pub(super) fn builtin_shift(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            let msg = "X::TypeCheck::Argument: Calling shift(Any) will never work with declared signature ($)".to_string();
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            let ex = Value::make_instance(Symbol::intern("X::TypeCheck::Argument"), attrs);
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(ex));
            return Err(err);
        }
        if args.len() > 1 {
            return Err(RuntimeError::new(format!(
                "Too many positionals passed; expected 1 argument but got {}",
                args.len()
            )));
        }
        Ok(match args.first().cloned().and_then(|v| v.into_array()) {
            Some((mut items, _)) => {
                if items.is_empty() {
                    make_empty_array_failure("shift")
                } else {
                    // Write through the shared node: the arg is the caller's
                    // own array container (container identity, §3) — a COW
                    // detach here would silently drop the removal.
                    crate::value::gc_data_mut(&mut items).remove(0)
                }
            }
            _ => make_empty_array_failure("shift"),
        })
    }

    pub(super) fn builtin_pop(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            let msg =
                "Calling pop() will never work with signature of the proto ($, *%)".to_string();
            let mut attrs = StdHashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            let ex = Value::make_instance(Symbol::intern("X::TypeCheck::Argument"), attrs);
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(ex));
            return Err(err);
        }
        if args.len() > 1 {
            return Err(RuntimeError::new(format!(
                "Too many positionals passed to pop; expected 1 argument but got {}",
                args.len()
            )));
        }
        if let Some(v) = args.first()
            && let ValueView::Array(_, kind) = v.view()
            && kind.is_lazy()
        {
            return Err(RuntimeError::cannot_lazy("pop"));
        }
        Ok(match args.first().cloned().and_then(|v| v.into_array()) {
            Some((mut items, _)) => {
                if items.is_empty() {
                    make_empty_array_failure("pop")
                } else {
                    // Same shared-node write-through as `builtin_shift` above.
                    crate::value::gc_data_mut(&mut items)
                        .pop()
                        .unwrap_or(Value::NIL)
                }
            }
            _ => make_empty_array_failure("pop"),
        })
    }

    pub(super) fn builtin_join(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let sep = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        // `join(sep, *@rest)` joins the flattened rest. Force any lazy lists
        // (join is eager over the flattened list) then run the single shared
        // `join_flat` helper — the same one the pure `native_function("join", ..)`
        // path uses. The previous inline copy here drifted from native (it had a
        // separate Range/Seq/itemized walk).
        let mut rest = Vec::with_capacity(args.len().saturating_sub(1));
        // `args.get(1..)` (not `&args[1..]`) so bare `join()` / `join(sep)` with no
        // list args doesn't panic on an empty slice.
        for v in args.get(1..).unwrap_or(&[]) {
            if crate::builtins::is_join_lazy(v) {
                rest.push(Value::str("...".to_string()));
                continue;
            }
            if let ValueView::LazyList(list) = v.view() {
                // A genuinely-lazy `@`-array stays opaque (rendered `...` by
                // `flat_val`) rather than forcing its capped prefix — this is the
                // `"@a[]"` interpolation path (`join " ", @a`). (L2)
                if list.in_array_context() && list.is_genuinely_lazy() {
                    rest.push(v.clone());
                } else {
                    rest.push(Value::array(self.force_lazy_list(&list)?));
                }
            } else {
                rest.push(v.clone());
            }
        }
        // `join_flat` is a pure helper, so it can only render an element with
        // `to_string_value` — a user-defined or role-composed `.Str` needs the
        // interpreter. Pre-render exactly those elements (and nothing else, so
        // Pair/Match/built-in rendering is untouched) before handing the list
        // over. Without this, `join(">", @a but role { method Str {...} })`
        // rendered the base list while `print` on the same value rendered the
        // role's `Str`.
        //
        // A `Proxy` element is the same shape of problem for the same reason
        // (ADR-0040 §9.2): rendering one means running its FETCH, which a pure
        // helper cannot do. `"@a[]"` interpolation compiles to this very
        // `join(" ", @a)` call, so this is where that spelling gets its FETCH —
        // the three method-dispatch guards never see it.
        // `join` flattens nested lists, so a deferred inner `.map` Seq (an
        // outer map's block returned `(1,).map({ ... })`) must run its
        // callback first: the pure `join_flat` would read its empty seed.
        for v in &rest {
            self.reify_nested_deferred_seqs(v, 0)?;
        }
        let mut rendered = Vec::with_capacity(rest.len());
        for v in &rest {
            let v = if Self::holds_nested_proxy(v) {
                self.resolve_proxies_in_value(v)?
            } else {
                v.clone()
            };
            match self.join_prerender_user_stringifier(&v)? {
                Some(replacement) => rendered.push(replacement),
                None => rendered.push(v),
            }
        }
        // A Junction anywhere in the flattened list threads the whole `join`
        // over its eigenstates (`("a"|"b","c","d").join` => `any(acd, bcd)`)
        // instead of stringifying it in place. Flatten `rendered` the same
        // way `join_flat` itself would (its own flattening is opaque to a
        // caller, so redo it here to inspect the leaves) — only when a
        // Junction is actually present does this replace the plain
        // `join_flat` call below; the common case pays one extra flatten.
        let mut flat_items = Vec::new();
        for v in &rendered {
            if crate::runtime::utils::is_shaped_array(v) {
                flat_items.extend(crate::runtime::utils::shaped_array_leaves(v));
            } else {
                crate::builtins::flat_val(v, &mut flat_items, true);
            }
        }
        // `join_flat` below rejects a zero-denominator Rational itself; the
        // Junction-threading render bypasses it, so check that path here.
        if flat_items
            .iter()
            .any(|v| matches!(v.view(), ValueView::Junction { .. }))
            && let Some(err) = flat_items
                .iter()
                .find_map(crate::runtime::utils::zero_denominator_rational_error)
        {
            return Err(err);
        }
        if let Some(threaded) = crate::builtins::thread_junctions_in_items(&flat_items, &|c| {
            Value::str(
                c.iter()
                    .map(|v| v.to_str_context())
                    .collect::<Vec<_>>()
                    .join(&sep),
            )
        }) {
            return Ok(threaded);
        }
        match crate::builtins::join_flat(&sep, &rendered) {
            Some(joined) => Ok(Value::str(joined?)),
            None => Ok(Value::str(String::new())),
        }
    }

    /// Reify every deferred Seq `join`'s flattening would descend into
    /// (non-itemized Arrays/Seqs, recursively), in place, so their elements are
    /// visible to the pure `join_flat`.
    // Cost: O(n), n = elements reached; each deferred Seq is pulled once.
    fn reify_nested_deferred_seqs(&mut self, v: &Value, depth: usize) -> Result<(), RuntimeError> {
        const MAX_DEPTH: usize = 64;
        if depth > MAX_DEPTH {
            return Ok(());
        }
        let children: Vec<Value> = match v.view() {
            ValueView::Seq(body) => {
                if body.awaits_vm_reify() {
                    self.reify_seq_body(&body)?
                } else {
                    body.to_vec()
                }
            }
            ValueView::Array(items, kind) if !kind.is_itemized() => items.to_vec(),
            _ => return Ok(()),
        };
        for child in &children {
            self.reify_nested_deferred_seqs(child, depth + 1)?;
        }
        Ok(())
    }

    /// Replace every element `join` will stringify that carries a *user*
    /// stringifier (a class-declared `Str`/`Stringy`, or one composed by a
    /// `but`/`does` role mixin) with its dispatched string, recursing into the
    /// non-itemized arrays `join_flat` itself flattens. `None` means "nothing
    /// here needs the interpreter" — the value is then left completely alone so
    /// the pure path keeps its exact identity, kind, and built-in rendering
    /// (Pair, Match, Range, ... are untouched).
    fn join_prerender_user_stringifier(
        &mut self,
        value: &Value,
    ) -> Result<Option<Value>, RuntimeError> {
        match value.view() {
            // An element still inside its container cell (`@a.reverse` hands
            // back the array's own cells) renders as the value it holds.
            ValueView::ContainerRef(_) => {
                let inner = value.deref_container();
                self.join_prerender_user_stringifier(&inner)
            }
            ValueView::Array(items, kind) if !kind.is_itemized() => {
                let originals: Vec<Value> = items.iter().cloned().collect();
                let mut mapped = Vec::with_capacity(originals.len());
                let mut changed = false;
                for v in &originals {
                    match self.join_prerender_user_stringifier(v)? {
                        Some(replacement) => {
                            changed = true;
                            mapped.push(replacement);
                        }
                        None => mapped.push(v.clone()),
                    }
                }
                Ok(changed.then(|| Value::array(mapped)))
            }
            // A Seq / Slip flattens into `join` like a List, so its elements
            // need the same user-`.Str` pre-render (`join "|", @objs.sort(...)`
            // handed the sort's Seq to `join_flat`, which stringified the
            // objects natively).
            ValueView::Seq(_) | ValueView::Slip(_) => {
                let originals: Vec<Value> = match value.view() {
                    ValueView::Seq(items) => items.iter().cloned().collect(),
                    ValueView::Slip(items) => items.iter().cloned().collect(),
                    _ => Vec::new(),
                };
                let mut mapped = Vec::with_capacity(originals.len());
                let mut changed = false;
                for v in &originals {
                    match self.join_prerender_user_stringifier(v)? {
                        Some(replacement) => {
                            changed = true;
                            mapped.push(replacement);
                        }
                        None => mapped.push(v.clone()),
                    }
                }
                Ok(changed.then(|| Value::array(mapped)))
            }
            // A NON-ITEMIZED Array-inner mixin (`@a but R`, `@a` a real
            // Positional) flattens through its inner elements for a slurpy,
            // exactly as `flat_val`'s own `Array`-inner `Mixin` arm does — the
            // role's `Str` override applies to the value as a whole, not to
            // the elements a flattening slurpy exposes instead of it. Only
            // recurse looking for a NESTED user stringifier; an ITEMIZED inner
            // (`my $r = @a but R` boxes it in a Scalar, `$r.raku` reads
            // `$[...]`) does NOT flatten — a scalar container never crosses a
            // slurpy boundary — so it stays the flattened element and keeps
            // its own `.Str`, same as any other non-container inner below.
            ValueView::Mixin(inner, _) if matches!(inner.as_ref().view(), ValueView::Array(_, kind) if !kind.is_itemized()) => {
                self.join_prerender_user_stringifier(inner)
            }
            ValueView::Mixin(..) => {
                // `join` stringifies elements through `.Str`, not through the
                // Stringy-first string-context helper used by `~`.
                match self.mixin_user_str(value) {
                    Some(r) => Ok(Some(Value::str(r?.to_string_value()))),
                    None => Ok(None),
                }
            }
            // A user `^find_method` answers `.Str` (`find_method_intercept`).
            ValueView::Instance { .. } | ValueView::Package(_)
                if let Some(r) = self.user_find_method_stringify(value, "Str") =>
            {
                Ok(Some(r?))
            }
            // A `Str` subclass element already is a `Str`: join takes its
            // payload, not its own `.Str` (#11026).
            ValueView::Instance { .. }
                if let Some(payload) = crate::runtime::str_subclass_payload(value) =>
            {
                Ok(Some(payload))
            }
            ValueView::Instance { class_name, .. } => {
                let cn = class_name.resolve();
                if self.has_user_method(&cn, "Str") {
                    let r = self.call_method_with_values(value.clone(), "Str", vec![])?;
                    return Ok(Some(Value::str(r.to_string_value())));
                }
                Ok(None)
            }
            _ => Ok(None),
        }
    }

    pub(super) fn builtin_list(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        // `list` on a single Seq is a no-op (returns the Seq as-is)
        if args.len() == 1
            && let ValueView::Seq(_) = args[0].view()
        {
            return Ok(args[0].clone());
        }
        let mut result = Vec::new();
        for arg in args {
            result.extend(Self::value_to_list(arg));
        }
        Ok(Value::array(result))
    }

    pub(super) fn builtin_cache(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        // `cache(gather {...})` is the gather's own `.cache`: a List over the
        // cached Seq that pulls the body only as far as it is read, so an
        // infinite gather works (ADR-11678). The Seq stays readable.
        if let [arg] = args
            && Self::is_gather_seq(arg)
            && let ValueView::Seq(body) = arg.view()
        {
            body.mark_cache_requested();
            return Ok(Value::seq_list_view(&body));
        }
        // `cache` eagerly evaluates and caches the values into a List
        let mut result = Vec::new();
        for arg in args {
            result.extend(Self::value_to_list(arg));
        }
        Ok(Value::array(result))
    }

    /// Pull every still-deferred `Seq` that `flat` is about to descend into
    /// (`flat((1, 2).map: -> $x { ($x, $x).map: ... })`: the outer map's
    /// elements are themselves unread `map` Seqs). The shared `flat_val`
    /// helper is pure and only sees a deferred body as empty.
    // Cost: O(n), n = elements reached through the descended Seqs and Lists.
    pub(crate) fn touch_flat_operand(&mut self, v: &Value) -> Result<(), RuntimeError> {
        match v.view() {
            ValueView::Seq(body) | ValueView::HyperSeq(body) | ValueView::RaceSeq(body) => {
                if matches!(
                    body.view(),
                    crate::value::SeqView::ItemSeq | crate::value::SeqView::ItemList
                ) {
                    return Ok(());
                }
                let body = std::sync::Arc::clone(&body);
                let items = if body.needs_touch() {
                    self.reify_seq_body(&body)?
                } else {
                    body.iter().cloned().collect()
                };
                for item in &items {
                    self.touch_flat_operand(item)?;
                }
            }
            ValueView::Array(items, crate::value::ArrayKind::List) => {
                for item in items.iter() {
                    self.touch_flat_operand(item)?;
                }
            }
            ValueView::Slip(items) => {
                for item in items.iter() {
                    self.touch_flat_operand(item)?;
                }
            }
            // A plain gather argument arrives as a pipe over its Seq
            // (ADR-11678): run it, and touch the gathers it took.
            ValueView::LazyList(list) if list.seq_stream_source().is_some() => {
                for item in &self.force_lazy_list_vm(&list)? {
                    self.touch_flat_operand(item)?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    pub(super) fn builtin_flat(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // A plain gather (streamed to `flat` as a pipe over its Seq) is
        // flattened as it is pulled (ADR-11678), so an infinite gather works.
        if let [arg] = args
            && let ValueView::LazyList(ll) = arg.view()
            && let Some(seq) = ll.seq_stream_source()
        {
            return Ok(Self::lazy_flat_gather(seq));
        }
        for arg in args {
            // The sole operand is un-itemized before it is flattened (see
            // below), so a `$`-held Seq is read too.
            if args.len() == 1 {
                self.touch_flat_operand(&crate::builtins::deitemize_flat_operand(arg))?;
            } else {
                self.touch_flat_operand(arg)?;
            }
        }
        // `flat(a, b, c)` flattens the argument *list*, so wrap the args in a
        // `List` and run the single shared `flat` helper (the same one the pure
        // `native_function("flat", ..)` path uses). The previous `flat_into`
        // copy unconditionally flattened nested `[...]`, which over-flattened
        // `flat(1, [2, [3, 4]], (5, 6))` (raku keeps the inner `[3 4]`).
        // Onearg rule: `flat($(1,2,3))` un-itemizes its sole argument and
        // descends, but `flat(0, $(1,2,3))` keeps each itemized arg single.
        let items: Vec<Value> = if args.len() == 1 {
            vec![crate::builtins::deitemize_flat_operand(&args[0])]
        } else {
            args.to_vec()
        };
        let list = Value::array_with_kind(
            crate::gc::Gc::new(crate::value::ArrayData::new(items)),
            crate::value::ArrayKind::List,
        );
        let mut result = Vec::new();
        crate::builtins::flat_val(&list, &mut result, true);
        Ok(Value::seq(result))
    }

    pub(super) fn builtin_slip(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        // `slip(+args)`: under the single-argument rule a lone lazy list is
        // the list, and it slips as a lazy tail (#13117).
        if let [only] = args
            && let Some(lazy) = crate::builtins::lazy_slip::lazy_slip_tail(only)
        {
            return Ok(lazy);
        }
        let mut items = Vec::new();
        for arg in args {
            match arg.view() {
                ValueView::Array(elems, ..) => items.extend(elems.iter().cloned()),
                ValueView::Seq(elems) => items.extend(elems.iter().cloned()),
                ValueView::Slip(elems) => items.extend(elems.iter().cloned()),
                ValueView::LazyList(ll) => {
                    if ll.scan_spec.is_some() {
                        items.extend(crate::builtins::lazy_scan::force_scan_to(&ll, 200_000));
                    } else {
                        let cached = ll.cache.lock().unwrap().clone().unwrap_or_default();
                        items.extend(cached);
                    }
                }
                _ => items.push(arg.clone()),
            }
        }
        Ok(Value::slip(items))
    }

    pub(super) fn builtin_reverse(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // `reverse()` (empty) and `reverse(a, b, c)` reverse the argument *list*
        // (so `reverse()` is `()`, not `Nil`). Only the single-arg form reverses a
        // collection.
        if args.len() != 1 {
            let mut items: Vec<Value> = args.to_vec();
            items.reverse();
            return Ok(Value::seq(items));
        }
        // A not-yet-read Seq source (deferred Iterator or IO::Handle.lines —
        // ADR-0034) must be reified by the interpreter (native bails).
        if let ValueView::Seq(body) = args[0].view()
            && body.needs_touch()
        {
            let body = std::sync::Arc::clone(&body);
            let mut items = self.reify_seq_body(&body)?;
            items.reverse();
            return Ok(Value::seq(items));
        }
        // `gather` produces a finite LazyList whose elements are supplied by
        // a VM coroutine. The native single-argument reverse routine cannot
        // see that coroutine through `ValueView`, so it would treat the whole
        // lazy list as one item (`reverse gather ...` => `()`). Reify finite
        // lazy lists at this eager list-op boundary; genuinely lazy sources
        // remain on the native path and keep their existing lazy semantics.
        if let ValueView::LazyList(list) = args[0].view()
            && !list.is_genuinely_lazy()
        {
            let mut items = self.force_lazy_list_vm(&list)?;
            items.reverse();
            return Ok(Value::seq(items));
        }
        // Single arg: delegate to the single shared native `reverse` (Array / Seq
        // / Slip / Range / 1-D shaped / Str), instead of a drifting second copy
        // that lacked Range/Slip/shaped arms.
        crate::builtins::native_function(Symbol::intern("reverse"), args).unwrap_or(Ok(Value::NIL))
    }

    pub(super) fn builtin_sort(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // Extract named args (:k, :by) and positional args
        let mut return_k = false;
        let mut by_callable: Option<Value> = None;
        let mut callable: Option<Value> = None;
        let mut positional: Vec<Value> = Vec::new();

        for arg in args {
            match arg.view() {
                ValueView::Pair(key, val) if key == "k" => {
                    return_k = val.truthy();
                }
                ValueView::Pair(key, val) if key == "by" => {
                    by_callable = Some(val.clone());
                }
                _ => positional.push(arg.clone()),
            }
        }

        // A lazy/infinite source cannot be sorted: throw X::Cannot::Lazy instead
        // of materializing it (matches raku). The comparator (a Sub) is never
        // lazy, so scanning all positionals is safe.
        if positional.iter().any(Self::is_lazy_for_coerce) {
            return Err(RuntimeError::cannot_lazy("sort"));
        }

        // sort(comparator, list, ...) or sort(list, ...) or sort(items...)
        if positional.len() >= 2 {
            let first = &positional[0];
            if matches!(first.view(), ValueView::Sub(_) | ValueView::Routine { .. }) {
                callable = Some(first.clone());
                positional.remove(0);
            }
        }

        // Prefer :by over positional callable
        if by_callable.is_some() {
            callable = by_callable;
        }

        // Flatten positional args into items
        let mut items: Vec<Value> = Vec::new();
        for arg in &positional {
            if crate::runtime::utils::is_shaped_array(arg) {
                items.extend(crate::runtime::utils::shaped_array_leaves(arg));
            } else {
                match arg.view() {
                    ValueView::Array(elems, ..) => items.extend(elems.iter().cloned()),
                    ValueView::Seq(elems) => items.extend(elems.iter().cloned()),
                    ValueView::Range(..)
                    | ValueView::RangeExcl(..)
                    | ValueView::RangeExclStart(..)
                    | ValueView::RangeExclBoth(..)
                    | ValueView::GenericRange { .. } => {
                        items.extend(Self::value_to_list(arg));
                    }
                    // A finite closure `...` sequence (already forced by
                    // `try_native_function`'s pre-dispatch reification, and
                    // already past the `is_lazy_for_coerce` guard above)
                    // flattens its cached elements like Array/Seq. Any other
                    // `LazyList` reaching here with no cache falls through to
                    // pushing itself whole, same as before.
                    ValueView::LazyList(ll) => {
                        if let Some(cached) = ll.cache.lock().unwrap().clone() {
                            items.extend(cached);
                        } else {
                            items.push(arg.clone());
                        }
                    }
                    _ => items.push(arg.clone()),
                }
            }
        }

        // Build sort args for the shared sort entry point.
        let mut sort_args: Vec<Value> = Vec::new();
        if let Some(c) = callable {
            sort_args.push(c);
        }
        if return_k {
            sort_args.push(Value::pair("k".to_string(), Value::TRUE));
        }

        // `sort;` / `sort()` with no positional argument at all (not even an
        // empty list) is a runtime error as of Rakudo 2022.07 — distinct from
        // `sort(())`, where an empty list *was* specified and sorting it
        // legitimately yields `()`.
        if positional.is_empty() {
            return Err(RuntimeError::typed_msg(
                "X::AdHoc",
                "Must specify something to sort",
            ));
        }
        if items.is_empty() {
            return Ok(Value::array(Vec::new()));
        }

        let mut caller = crate::runtime::methods_collection_ops::sort::InterpCaller(self);
        crate::runtime::methods_collection_ops::sort::sort_value_generic(
            &mut caller,
            Value::array(items),
            &sort_args,
        )
    }

    // Cost: as `builtin_uniq_family`.
    pub(super) fn builtin_unique(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        self.builtin_uniq_family(args, "unique")
    }

    // Cost: as `builtin_uniq_family`.
    pub(super) fn builtin_repeated(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        self.builtin_uniq_family(args, "repeated")
    }

    // Cost: as `builtin_uniq_family`.
    pub(super) fn builtin_squish(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        self.builtin_uniq_family(args, "squish")
    }

    /// The routine forms of `unique`/`repeated`/`squish` (`+values, :&as,
    /// :&with`): the `:as`/`:with` pairs go to the method, the rest is the
    /// list under the single-argument rule, with a `Slip` argument slipping
    /// into it (`unique((1, 2).Slip, (2, 3).Slip)` is `(1 2 3)`, #13506).
    // Cost: O(n) to gather the arguments, n = arguments plus slipped items,
    // then the method's own cost.
    fn builtin_uniq_family(&mut self, args: &[Value], method: &str) -> Result<Value, RuntimeError> {
        let mut positional = Vec::new();
        let mut method_args = Vec::new();
        let mut slipped = false;
        for arg in args {
            match arg.view() {
                ValueView::Pair(key, _) if key == "as" || key == "with" => {
                    method_args.push(arg.clone());
                }
                ValueView::Slip(items) => {
                    slipped = true;
                    positional.extend(items.iter().cloned());
                }
                _ => positional.push(arg.clone()),
            }
        }
        if positional.is_empty() {
            return Ok(Value::array(Vec::new()));
        }
        let target = if positional.len() == 1 && !slipped {
            positional.pop().unwrap_or(Value::NIL)
        } else {
            Value::array(positional)
        };
        if method == "unique" {
            self.warm_which_identity(&target);
        }
        self.call_method_with_values(target, method, method_args)
    }
}
