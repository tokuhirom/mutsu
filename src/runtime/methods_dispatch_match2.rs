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
        match method {
            "Seq" if args.is_empty() => Some(self.dispatch_seq_coercion(target)),
            "list" | "Array" if args.is_empty() => {
                if let ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } = target.view()
                    && class_name == "Supply"
                {
                    return Some(
                        self.supply_list_values(&attributes.as_map(), true)
                            .map(Value::array),
                    );
                }
                None
            }
            "List" if args.is_empty() => Some(self.dispatch_list_coercion(target)),
            "Set" | "SetHash" if args.is_empty() => {
                let mut result = match self.dispatch_to_set_with_what(target, method) {
                    Ok(r) => r,
                    Err(e) => return Some(Err(e)),
                };
                // If SetHash, ensure the mutable flag is set
                if method == "SetHash" {
                    result.with_set_mut(|_, is_mut| *is_mut = true);
                }
                Some(Ok(result))
            }
            "Bag" | "BagHash" if args.is_empty() => {
                let mut result = match self.dispatch_to_bag_with_what(target, method) {
                    Ok(r) => r,
                    Err(e) => return Some(Err(e)),
                };
                // If BagHash, ensure the mutable flag is set
                if method == "BagHash" {
                    result.with_bag_mut(|_, is_mut| *is_mut = true);
                }
                Some(Ok(result))
            }
            "new-from-pairs" => {
                // Bag.new-from-pairs(...) / BagHash.new-from-pairs(...)
                // Takes pairs as arguments and uses pair values as counts.
                if let ValueView::Package(name) = target.view() {
                    let cn = name.resolve();
                    if matches!(cn.as_str(), "Bag" | "BagHash") {
                        // Check for lazy inputs
                        for a in &args {
                            if Self::is_lazy_for_coerce(a) {
                                return Some(Err(RuntimeError::cannot_lazy_what(&cn)));
                            }
                        }
                        // Collect all items, flattening arrays
                        let mut all_items = Vec::new();
                        for arg in &args {
                            match arg.view() {
                                ValueView::Array(items, ..) => {
                                    all_items.extend(items.iter().cloned());
                                }
                                _ => all_items.push(arg.clone()),
                            }
                        }
                        // Convert items to a temporary array and use dispatch_to_bag
                        let arr = Value::array(all_items);
                        let result = match self.dispatch_to_bag_with_what(arr, &cn) {
                            Ok(r) => r,
                            Err(e) => return Some(Err(e)),
                        };
                        if cn == "BagHash"
                            && let ValueView::Bag(items, _) = result.view()
                        {
                            return Some(Ok(Value::bag_parts(items.clone(), true)));
                        }
                        return Some(Ok(result));
                    }
                }
                None
            }
            "Mix" if args.is_empty() => Some(self.dispatch_to_mix_with_what(target, method)),
            "MixHash" if args.is_empty() => {
                // `.MixHash` is `.Mix` plus the mutable flag and embedded `MixHash`
                // type metadata; the metadata lives in the `Mix` Arc, so the
                // whole coercion is a pure value op shared with the VM native path.
                Some(crate::builtins::quanthash_coerce::to_mixhash(target))
            }
            "Setty" | "Baggy" | "Mixy" if args.is_empty() => {
                self.dispatch_setty_baggy_mixy(&target, method)
            }
            "Map" | "Hash" if args.is_empty() => {
                if matches!(target.view(), ValueView::Package(_)) {
                    return Some(Ok(Value::package(Symbol::intern(method))));
                }
                // `.Map` / `.Hash` are pure value coercions (the `Map`
                // declared-type is embedded in the resulting `Hash` Arc,
                // not an interpreter-owned side table). Single impl shared with
                // the VM native path.
                if method == "Map" {
                    return Some(crate::builtins::map_hash_coerce::to_map(target));
                }
                // `.Hash` on an undefined invocant (`my $d`; a bare `Nil`) is the
                // empty hash, like the `.hash` method. (`.Map` still throws on
                // an undefined invocant, matching raku, so this is Hash-only.)
                if target.is_nil() {
                    return Some(Ok(Value::hash(std::collections::HashMap::new())));
                }
                Some(crate::builtins::map_hash_coerce::to_hash(target, true))
            }
            "hash" if args.is_empty() && !matches!(target.view(), ValueView::Instance { .. }) => {
                Some(self.dispatch_to_hash(target))
            }
            "any" | "all" | "one" | "none" if args.is_empty() => {
                let kind = match method {
                    "any" => JunctionKind::Any,
                    "all" => JunctionKind::All,
                    "one" => JunctionKind::One,
                    _ => JunctionKind::None,
                };
                let values = Self::value_to_list(&target);
                Some(Ok(Value::junction(kind, values)))
            }
            "slice" => {
                // Seq.slice(@indices) — return elements at the given indices as a Seq.
                // Seq.slice() with no args returns an empty Seq.
                let items = if let ValueView::Seq(arc) = target.view() {
                    arc.as_ref().clone()
                } else {
                    crate::runtime::utils::value_to_list(&target)
                };
                if args.is_empty() {
                    Some(Ok(Value::seq_arc(std::sync::Arc::new(Vec::new()))))
                } else {
                    let mut result = Vec::with_capacity(args.len());
                    for arg in &args {
                        // Expand each argument to zero or more 0-based indices, in
                        // order: a plain number is a single index, while a Range
                        // (`3..6`) contributes every index it spans, so
                        // `.slice(0, 3..6, 8)` gathers indices 0,3,4,5,6,8.
                        let indices: Vec<usize> = match arg.view() {
                            ValueView::Int(i) if i >= 0 => vec![i as usize],
                            ValueView::Num(n) if n >= 0.0 => vec![n as usize],
                            _ if arg.is_range() => crate::runtime::utils::value_to_list(arg)
                                .iter()
                                .filter_map(|v| match v.view() {
                                    ValueView::Int(i) if i >= 0 => Some(i as usize),
                                    _ => None,
                                })
                                .collect(),
                            _ => Vec::new(),
                        };
                        for i in indices {
                            if i < items.len() {
                                result.push(items[i].clone());
                            }
                        }
                    }
                    Some(Ok(Value::seq_arc(std::sync::Arc::new(result))))
                }
            }
            "iterator" if args.is_empty() => Some(self.dispatch_iterator_method(target)),
            "produce" => self.dispatch_produce_method(target, args),
            "reduce" => Some(self.dispatch_reduce_method(target, args)),
            "elems" => self.dispatch_elems_method(target, args),
            "map" => Some(self.dispatch_map_method(target, args)),
            "flatmap" => {
                // flatmap is equivalent to .map(...).flat
                Some(self.dispatch_map_method(target, args).map(|mapped| {
                    let items = Self::value_to_list(&mapped);
                    let mut flat_items = Vec::new();
                    for item in items {
                        match item.view() {
                            ValueView::Array(sub_items, _) => {
                                flat_items.extend(sub_items.iter().cloned());
                            }
                            ValueView::Seq(sub_items) => {
                                flat_items.extend(sub_items.iter().cloned());
                            }
                            _ => flat_items.push(item.clone()),
                        }
                    }
                    Value::seq_arc(std::sync::Arc::new(flat_items))
                }))
            }
            "duckmap" => {
                let block = args.first().cloned().unwrap_or(Value::NIL);
                Some(self.duckmap_iterate(&block, &target))
            }
            "deepmap" => {
                let block = args.first().cloned().unwrap_or(Value::NIL);
                Some(self.deepmap_iterate(&block, &target))
            }
            "nodemap" => {
                let block = args.first().cloned().unwrap_or(Value::NIL);
                Some(self.nodemap_iterate(&block, &target))
            }
            "max" | "min" => Some(self.dispatch_min_max_method(target, method, args)),
            "minpairs" | "maxpairs" if args.is_empty() => {
                Some(self.dispatch_minmaxpairs(target, method))
            }
            "pop" => Some(self.dispatch_pop_method(target, args)),
            "sort" => Some(self.dispatch_sort_method(target, args)),
            "unique" => {
                if !matches!(target.view(), ValueView::Instance { class_name, .. } if class_name == "Supply")
                {
                    Some(self.dispatch_unique(target, &args))
                } else {
                    None
                }
            }
            "repeated" => {
                if !matches!(target.view(), ValueView::Instance { class_name, .. } if class_name == "Supply")
                {
                    Some(self.dispatch_repeated(target, &args))
                } else {
                    None
                }
            }
            "squish" => {
                if let ValueView::Instance { class_name, .. } = target.view()
                    && class_name == "Supply"
                {
                    Some(self.dispatch_supply_transform(target, "squish", &args))
                } else {
                    Some(self.dispatch_squish(target, &args))
                }
            }
            "minmax" => self.dispatch_minmax_method(target, method, args),
            "snip" => self.dispatch_snip_method(target, method, args),
            "head" | "flat" | "batch" | "throttle" | "comb" | "words" | "wait" | "zip"
            | "zip-latest" | "stable" => {
                if let ValueView::Instance { class_name, .. } = target.view()
                    && class_name == "Supply"
                {
                    Some(self.dispatch_supply_transform(target, method, &args))
                } else {
                    None
                }
            }
            "set" | "primary" | "secondary" | "tertiary" | "quaternary" | "gist" if matches!(target.view(), ValueView::Instance { class_name, .. } if class_name == "Collation") => {
                Some(self.dispatch_collation_method(target, method, &args))
            }
            "collate" if args.is_empty() => Some(self.dispatch_collate(target)),
            "take" if args.is_empty() => {
                match self.take_value(target.clone()) {
                    Ok(_) => {}
                    Err(e) => return Some(Err(e)),
                }
                Some(Ok(target))
            }
            "rotor" => {
                if let ValueView::Instance { class_name, .. } = target.view()
                    && class_name == "Supply"
                {
                    Some(self.dispatch_supply_transform(target, "rotor", &args))
                } else {
                    Some(self.dispatch_rotor(target, &args))
                }
            }
            _ => None,
        }
    }

    /// Dispatch the "iterator" method.
    fn dispatch_iterator_method(&mut self, target: Value) -> Result<Value, RuntimeError> {
        if matches!(target.view(), ValueView::Instance { class_name, .. } if class_name == "Iterator")
        {
            return Ok(target);
        }
        // Check consumed state for Seq: throw X::Seq::Consumed if not cached
        if let ValueView::Seq(items) = target.view() {
            if crate::value::seq_is_consumed(&items) && !crate::value::seq_is_cached(&items) {
                return Err(crate::value::seq_consumed_error());
            }
            // Mark as consumed only if not cached (cached Seqs allow multiple iterations)
            if !crate::value::seq_is_cached(&items) {
                crate::value::seq_consume(&items).ok();
            }
        }
        if let ValueView::Seq(items) = target.view() {
            let seq_id = std::sync::Arc::as_ptr(&items) as usize;
            if let Some(meta) = self.squish_iterator_meta.remove(&seq_id) {
                for key in meta.revert_remove {
                    self.env.remove(&key);
                }
                for (key, value) in meta.revert_values {
                    self.env.insert(key, value);
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

    /// Dispatch the "produce" method.
    fn dispatch_produce_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        if let ValueView::Instance { class_name, .. } = target.view()
            && class_name == "Supply"
        {
            return Some(self.dispatch_supply_transform(target, "produce", &args));
        }
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
    fn dispatch_reduce_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let callable = args
            .first()
            .cloned()
            .ok_or_else(|| RuntimeError::new("reduce expects a callable"))?;
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
            && class_name == "Supply"
        {
            let attrs_clone = attributes.to_map();
            return self.dispatch_supply_reduce(target, &attrs_clone, callable);
        }
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
    fn dispatch_elems_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
            && class_name == "Supply"
        {
            return Some(self.dispatch_supply_elems(&(attributes).as_map(), &args));
        }
        // .elems on a Seq caches it (makes it available for multiple calls)
        if let ValueView::Seq(items) = target.view() {
            crate::value::seq_mark_cached(&items);
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

    fn dispatch_map_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let target = Self::mixin_iteration_target(target);
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
            && class_name == "Supply"
        {
            return self.dispatch_supply_map(&(attributes).as_map(), &args);
        }
        // Validate that the map argument is callable (X::Cannot::Map)
        if let Some(func) = args.first() {
            let is_callable = matches!(
                func.view(),
                ValueView::Sub(_)
                    | ValueView::Routine { .. }
                    | ValueView::WeakSub(_)
                    | ValueView::Regex(_)
                    | ValueView::RegexWithAdverbs(_)
            ) || (matches!(func.view(), ValueView::Instance { class_name, .. } if class_name.resolve() == "WhateverCode" || class_name.resolve() == "HyperWhateverCode"))
                || matches!(func.view(), ValueView::Whatever);
            if !is_callable {
                let mut attrs = HashMap::new();
                attrs.insert(
                    "message".to_string(),
                    Value::str(format!(
                        "Cannot map a {} to a {}, it's not callable.",
                        super::value_type_name(func),
                        super::value_type_name(&target),
                    )),
                );
                let ex =
                    Value::make_instance(crate::symbol::Symbol::intern("X::Cannot::Map"), attrs);
                let mut err = RuntimeError::new(format!(
                    "X::Cannot::Map: Cannot map a {} to a {}",
                    super::value_type_name(func),
                    super::value_type_name(&target),
                ));
                err.exception = Some(Box::new(ex));
                return Err(err);
            }
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
        let items = if crate::runtime::utils::is_shaped_array(&target) {
            crate::runtime::utils::shaped_array_leaves(&target)
        } else {
            match target.view() {
                ValueView::Array(items, kind) if kind.is_itemized() => items.to_vec(),
                // A Blob/Buf maps over its bytes (matches raku iteration).
                _ => {
                    Self::buf_as_byte_items(&target).unwrap_or_else(|| Self::value_to_list(&target))
                }
            }
        };
        // In Raku, `.map` returns a lazy Seq. mutsu evaluates map eagerly for
        // performance, which is observationally equivalent except when the
        // callback contains a `return`: that `return` targets the lexically
        // enclosing routine, and if the Seq is forced after that routine has
        // exited it must surface as `X::ControlFlow::Return` with
        // out-of-dynamic-scope set. To get that right without making every map
        // lazy (which perturbs list shape/context in many call sites), only
        // defer evaluation when the callback body actually contains a `return`.
        // A stub body (`...`) must likewise stay unevaluated until the Seq is
        // forced: `map -> $x, $y { ... }, @list` lives in Raku as long as the
        // result is never iterated.
        if let Some(ValueView::Sub(sub_data)) = args.first().map(Value::view)
            && (Self::body_contains_return(&sub_data.body)
                || Self::is_stub_routine_body(&sub_data.body))
        {
            return Ok(self.create_lazy_map_list(items, &sub_data));
        }
        let result = self.eval_map_over_items(args.first().cloned(), items)?;
        // .map() returns a Seq per Raku spec
        Ok(match result.view() {
            ValueView::Array(items, _) => Value::seq_arc(std::sync::Arc::new(items.to_vec())),
            _ => result.clone(),
        })
    }

    /// Create a `LazyList` that lazily maps `callback` over `items`.
    ///
    /// Instead of eagerly evaluating the map, stores the items and callback
    /// in a `LazyList`. When the list is forced, `eval_map_over_items` runs
    /// the callback for each item. This ensures that `return` inside the
    /// callback correctly detects when the lexically enclosing routine has
    /// already exited (out-of-dynamic-scope).
    pub(super) fn create_lazy_map_list(
        &self,
        items: Vec<Value>,
        callback: &crate::gc::Gc<crate::value::SubData>,
    ) -> Value {
        let mut env = self.env.clone();
        env.insert(
            "__mutsu_lazy_map_items".to_string(),
            Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(items)),
                crate::value::ArrayKind::List,
            ),
        );
        env.insert(
            "__mutsu_lazy_map_func".to_string(),
            Value::sub_value(callback.clone()),
        );
        // Mark as gather-based so the VM auto-forces it for chained methods.
        env.insert("__mutsu_lazylist_from_gather".to_string(), Value::TRUE);

        let list = crate::value::LazyList {
            body: Vec::new(),
            env,
            cache: std::sync::Mutex::new(None),
            compiled_code: None,
            compiled_fns: None,
            elems_count: None,
            scan_spec: None,
            sequence_spec: None,
            coroutine: None,
            lazy_pipe: None,
            closure_seq: None,
            walk_pending: None,
            cat_pull: None,
        };
        Value::lazy_list(crate::gc::Gc::new(list))
    }

    /// Dispatch "min" and "max" methods.
    fn dispatch_min_max_method(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let ValueView::Instance { class_name, .. } = target.view()
            && class_name == "Supply"
        {
            return self.dispatch_supply_running_extrema(target, method, &args);
        }
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
                let elems = self.call_method_with_values(target.clone(), "elems", vec![])?;
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
        let mut call_args = vec![target.clone()];
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

    /// Dispatch the "pop" method.
    fn dispatch_pop_method(
        &mut self,
        mut target: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if !args.is_empty() {
            return Err(RuntimeError::new(format!(
                "Too many positionals passed; expected 1 argument but got {}",
                args.len() + 1
            )));
        }
        if let ValueView::Array(_, kind) = target.view()
            && kind.is_lazy()
        {
            return Err(RuntimeError::cannot_lazy("pop"));
        }
        if let Some(result) = target.with_array_mut(|items, _| {
            // The mutated node may be shared with a live holder (this fallback
            // takes `target` by value with no write-back), so write through
            // the shared node — container identity (§3).
            let items_mut = crate::value::gc_data_mut(items);
            if items_mut.is_empty() {
                make_empty_array_failure("pop")
            } else {
                items_mut.pop().unwrap_or(Value::NIL)
            }
        }) {
            return Ok(result);
        }
        Ok(make_empty_array_failure("pop"))
    }

    /// Dispatch the "sort" method.
    fn dispatch_sort_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let ValueView::Instance { class_name, .. } = target.view()
            && class_name == "Supply"
        {
            return self.dispatch_supply_transform(target, "sort", &args);
        }
        if let ValueView::Package(name) = target.view()
            && name == "Supply"
        {
            return Ok(Value::seq_arc(std::sync::Arc::new(vec![target])));
        }
        let mut caller = crate::runtime::methods_collection_ops::sort::InterpCaller(self);
        crate::runtime::methods_collection_ops::sort::sort_value_generic(&mut caller, target, &args)
    }

    /// Dispatch the "minmax" method.
    fn dispatch_minmax_method(
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
        let mut call_args = vec![target.clone()];
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

    /// Dispatch the "snip" method.
    fn dispatch_snip_method(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        if let ValueView::Instance { class_name, .. } = target.view()
            && class_name == "Supply"
        {
            return Some(self.dispatch_supply_transform(target, method, &args));
        }
        if !args.is_empty() {
            let matcher = args[0].clone();
            let items = crate::runtime::utils::value_to_list(&target);
            return Some(self.eval_snip(matcher, items));
        }
        None
    }
}
