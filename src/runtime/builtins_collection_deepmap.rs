use super::*;
use crate::value::ValueView;

/// A `Range` is `Iterable`, so the `*map` family descends into it exactly as it
/// descends into a `List`: `(1..4).deepmap(*+1)` is `(2 4 6 8)`, and a nested
/// one itemizes like a List sublist — `(1, (2..3)).deepmap(*+1)` is
/// `(2, $(3, 4))`. mutsu keeps ranges out of the `Array` view variant, so
/// without this every arm below treats a range as a *leaf* and hands the whole
/// Range to the block, which silently answers a Range (`2..8`) instead of
/// calling the block per element.
/// Is this `Array`/`Hash` element a deepmap *leaf* (hand it to the block) or a
/// container to descend into?
///
/// ADR-0040 slices 1-2: a `Range`/`Seq` stored as a real `Array`/`Hash` element
/// is itemized as `Scalar(inner)`, so the test has to see through the element's
/// own itemization — the question is what the value IS, not whether it sits in
/// a container. Without this, `%(a => 1, b => (2..3)).deepmap(*+1)` treats the
/// Range as a leaf and answers `{:a(2), :b(3..4)}` where raku answers
/// `{:a(2), :b($(3, 4))}`. (An itemized `Array`/`Hash` element needs no
/// unwrapping: its itemization is a kind/flag, so it still matches the
/// `ValueView::Array`/`ValueView::Hash` arms.) The *result*'s itemization is
/// decided separately, by `deepmap_iterate_inner`'s `itemize_result`.
fn deepmap_element_is_leaf(v: &Value) -> bool {
    let v = v.descalarize();
    !v.is_range()
        && !matches!(
            v.view(),
            ValueView::Package(_) | ValueView::Array(..) | ValueView::Seq(_) | ValueView::Hash(_)
        )
}

fn range_as_list(value: &Value) -> Option<Value> {
    value
        .is_range()
        .then(|| Value::array(crate::runtime::utils::value_to_list(value)))
}

impl Interpreter {
    /// `cross(@a, @b, ...)` — Cartesian product of lists.
    /// With `with => &op`, applies the operator to each pair instead of making tuples.
    pub(super) fn builtin_cross(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        let mut lists: Vec<Vec<Value>> = Vec::new();
        let mut with_func: Option<Value> = None;

        for arg in &args {
            match arg.view() {
                ValueView::Pair(k, v) if k.as_str() == "with" => {
                    with_func = Some(v.clone());
                }
                _ => {
                    let mut values = super::utils::value_to_list(arg);
                    if values.len() == 1
                        && let Some(single) = values.first()
                    {
                        match single.view() {
                            ValueView::Array(items, _) => {
                                values = items.as_ref().clone().into_items();
                            }
                            ValueView::Seq(items) => {
                                values = items.to_vec();
                            }
                            ValueView::Slip(items) => {
                                values = items.as_ref().clone();
                            }
                            _ => {}
                        }
                    }
                    lists.push(values);
                }
            }
        }

        if lists.is_empty() {
            return Ok(Value::seq(vec![]));
        }

        // Compute Cartesian product iteratively
        let mut result: Vec<Vec<Value>> = vec![vec![]];
        for list in &lists {
            let mut new_result = Vec::new();
            for combo in &result {
                for item in list {
                    let mut new_combo = combo.clone();
                    new_combo.push(item.clone());
                    new_result.push(new_combo);
                }
            }
            result = new_result;
        }

        // Apply `with` function or create tuples. `cross` returns a Seq (so
        // `.^name` is Seq, `.raku` shows `.Seq`), matching Rakudo and the `X`
        // metaop.
        if let Some(func) = with_func {
            let mut final_result = Vec::new();
            for combo in result {
                let val = self.call_sub_value(func.clone(), combo, false)?;
                final_result.push(val);
            }
            Ok(Value::seq(final_result))
        } else {
            // Return as list of lists (tuples)
            let tuples: Vec<Value> = result.into_iter().map(Value::array).collect();
            Ok(Value::seq(tuples))
        }
    }

    /// The `rotor` **subroutine** (`Type/List.rakudoc`: "From language version
    /// 6.e onward, there is also a subroutine `rotor`"). Its signature puts the
    /// list LAST and the cycle spec first —
    /// `rotor(**@cycle, \thing, Bool() :$partial)` — so it is exactly
    /// `thing.rotor(@cycle, :$partial)` with the arguments rotated. Delegating
    /// to the `.rotor` method keeps one implementation of the cycle semantics
    /// (`Pair` gaps/overlaps, cycling, `:partial`).
    ///
    /// Rakudo gates it on `use v6.e.PREVIEW`; without the pragma a bare `rotor`
    /// is an undeclared routine. mutsu applies the same gate here, at the call.
    /// The `snitch` **subroutine** (`Type/Any.rakudoc`:
    /// `multi snitch(\snitchee)` / `multi snitch(&snitcher, \snitchee)`), the
    /// sub form of the `.snitch` debugging probe. The snitchee is the LAST
    /// argument so the feed operator reads naturally
    /// (`(1..3).Seq ==> snitch() ==> map(*+2)`); an optional leading `Callable`
    /// replaces the default `note` logger. Delegates to `.snitch` so there is
    /// one implementation, including the 6.e gate.
    pub(super) fn builtin_snitch(&mut self, raw_args: &[Value]) -> Result<Value, RuntimeError> {
        if !crate::parser::current_language_version().starts_with("6.e") {
            return Err(RuntimeError::new(
                "Undeclared routine: snitch -- the snitch subroutine needs `use v6.e.PREVIEW`",
            ));
        }
        let Some((snitchee, rest)) = raw_args.split_last() else {
            return Err(RuntimeError::new(
                "Too few positionals passed to 'snitch'; expected 1 or 2 arguments but got 0",
            ));
        };
        match self.dispatch_snitch(snitchee, rest) {
            Some(result) => result,
            // Unreachable: the 6.e gate above already passed, which is the only
            // reason `dispatch_snitch` declines.
            None => Ok(snitchee.clone()),
        }
    }

    pub(super) fn builtin_rotor(&mut self, raw_args: &[Value]) -> Result<Value, RuntimeError> {
        if !crate::parser::current_language_version().starts_with("6.e") {
            return Err(RuntimeError::new(
                "Undeclared routine: rotor -- the rotor subroutine needs `use v6.e.PREVIEW`",
            ));
        }
        let mut partial: Option<Value> = None;
        let mut positional: Vec<Value> = Vec::with_capacity(raw_args.len());
        for arg in raw_args {
            let named_partial = match arg.view() {
                ValueView::Pair(key, value) if key == "partial" => Some(value.clone()),
                ValueView::ValuePair(key, value) if key.to_string_value() == "partial" => {
                    Some(value.clone())
                }
                _ => None,
            };
            match named_partial {
                Some(value) => partial = Some(value),
                None => positional.push(arg.clone()),
            }
        }
        let Some(thing) = positional.pop() else {
            return Err(RuntimeError::new(
                "Calling rotor() will never work with declared signature (**@cycle, \\thing, Bool() :$partial)",
            ));
        };
        if positional.is_empty() {
            return Err(RuntimeError::new(
                "Calling rotor(\\thing) will never work with declared signature (**@cycle, \\thing, Bool() :$partial)",
            ));
        }
        if let Some(partial) = partial {
            positional.push(Value::pair("partial".to_string(), partial));
        }
        self.call_method_with_values(thing, "rotor", positional)
    }

    pub(super) fn builtin_roundrobin(&self, raw_args: &[Value]) -> Result<Value, RuntimeError> {
        // Split off the `:slip` adverb (a `slip => Bool` named arg); the rest are
        // the lists-of-lists streams. With `:slip`, the tuples are concatenated
        // into one flat Seq instead of a Seq of tuples.
        let mut slip = false;
        let mut positional: Vec<Value> = Vec::with_capacity(raw_args.len());
        for a in raw_args {
            match a.view() {
                ValueView::Pair(k, v) if k == "slip" => slip = v.truthy(),
                ValueView::ValuePair(k, v) if k.to_string_value() == "slip" => slip = v.truthy(),
                _ => positional.push(a.clone()),
            }
        }
        let args = &positional[..];
        if args.is_empty() {
            return Ok(Value::seq(Vec::new()));
        }

        // Implement Raku's single-arg rule (+@lol): when called with a single
        // iterable arg, iterate it to get the list of streams.
        let effective_args: Vec<Value> = if args.len() == 1 {
            match args[0].view() {
                ValueView::Array(_items, kind) if kind.is_itemized() => args.to_vec(),
                ValueView::Array(items, _) => items.iter().cloned().collect(),
                ValueView::Seq(items) => items.iter().cloned().collect(),
                ValueView::Slip(items) => items.iter().cloned().collect(),
                _ => args.to_vec(),
            }
        } else {
            args.to_vec()
        };

        if effective_args.is_empty() {
            return Ok(Value::seq(Vec::new()));
        }

        let streams: Vec<Vec<Value>> = effective_args
            .iter()
            .map(|arg| match arg.view() {
                ValueView::Capture { positional, named }
                    if named.is_empty() && positional.len() == 1 =>
                {
                    vec![arg.clone()]
                }
                ValueView::Array(_items, kind) if kind.is_itemized() => vec![arg.clone()],
                ValueView::Array(items, _) => items.iter().cloned().collect(),
                ValueView::Seq(items) => items.iter().cloned().collect(),
                ValueView::Slip(items) => items.iter().cloned().collect(),
                ValueView::Range(a, b) => (a..=b).map(Value::int).collect(),
                ValueView::RangeExcl(a, b) => (a..b).map(Value::int).collect(),
                _ if arg.is_range() => crate::runtime::utils::value_to_list(arg),
                _ => vec![arg.clone()],
            })
            .collect();

        let mut indices = vec![0usize; streams.len()];
        let mut rounds = Vec::new();
        loop {
            let mut tuple = Vec::new();
            let mut progressed = false;
            for (i, stream) in streams.iter().enumerate() {
                if indices[i] < stream.len() {
                    tuple.push(stream[indices[i]].clone());
                    indices[i] += 1;
                    progressed = true;
                }
            }
            if !progressed {
                break;
            }
            rounds.push(Value::array(tuple));
        }

        if slip {
            let flat: Vec<Value> = rounds
                .iter()
                .flat_map(|r| match r.view() {
                    ValueView::Array(items, _) => items.iter().cloned().collect::<Vec<_>>(),
                    _ => vec![r.clone()],
                })
                .collect();
            return Ok(Value::seq(flat));
        }
        Ok(Value::seq(rounds))
    }

    /// `duckmap(&block, \obj)` — apply block to each element; on type mismatch
    /// descend recursively into iterables, or return the element unchanged.
    pub(super) fn builtin_duckmap(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new("duckmap requires a block and an object"));
        }
        let block = args[0].clone();
        let obj = args[1].clone();
        self.duckmap_iterate(&block, &obj)
    }

    /// `deepmap(&block, \obj)` — apply block to every leaf element, preserving structure.
    pub(super) fn builtin_deepmap(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new("deepmap requires a block and an object"));
        }
        let block = args[0].clone();
        let obj = args[1].clone();
        self.deepmap_iterate(&block, &obj)
    }

    pub(super) fn builtin_nodemap(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new("nodemap requires a block and an object"));
        }
        let block = args[0].clone();
        let obj = args[1].clone();
        self.nodemap_iterate(&block, &obj)
    }

    /// Iterate over the elements of a value, applying duckmap to each.
    /// This is the entry point for both the method and function forms.
    pub(crate) fn duckmap_iterate(
        &mut self,
        block: &Value,
        target: &Value,
    ) -> Result<Value, RuntimeError> {
        if let Some(list) = range_as_list(target) {
            return self.duckmap_iterate(block, &list);
        }
        // This construct handles `next`/`last`/`redo`, so a loop-control
        // statement raised anywhere in its dynamic extent has somewhere to go
        // (`runtime/loop_handler_depth.rs`). Without the guard the raise site
        // would convert the signal into a thrown `X::ControlFlow` and silently
        // break this loop.
        let _loop_handler = crate::runtime::loop_handler_depth::LoopHandlerGuard::new();
        match target.view() {
            ValueView::Array(items, kind) => {
                // A descended-into sublist is itemized only when its parent is
                // a List, not a real Array — the same rule deepmap applies:
                // `(1, [2,3]).duckmap(-> Int $x {...})` -> `(10, $[20, 30])`
                // but `[1, [2,3]].duckmap(...)` -> `[10, [20, 30]]`.
                let child_itemize = !kind.is_real_array();
                let mut result = Vec::new();
                for item in items.iter() {
                    match self.duckmap_element(block, item, child_itemize) {
                        Ok(v) => result.push(v),
                        Err(e) if e.is_next() => continue,
                        Err(e) if e.is_last() => break,
                        Err(e) => return Err(e),
                    }
                }
                if kind.is_real_array() {
                    Ok(Value::real_array(result))
                } else {
                    Ok(Value::array(result))
                }
            }
            ValueView::Seq(items) => {
                let mut result = Vec::new();
                for item in items.iter() {
                    match self.duckmap_element(block, item, true) {
                        Ok(v) => result.push(v),
                        Err(e) if e.is_next() => continue,
                        Err(e) if e.is_last() => break,
                        Err(e) => return Err(e),
                    }
                }
                // duckmap on a Seq returns a List (rakudo:
                // `(...).Seq.duckmap(...).WHAT` is `List`).
                Ok(Value::array(result))
            }
            ValueView::Hash(map) => {
                let mut result = std::collections::HashMap::new();
                for (k, v) in map.iter() {
                    match self.duckmap_element(block, v, true) {
                        Ok(mapped) => {
                            result.insert(k.clone(), mapped);
                        }
                        Err(e) if e.is_next() => continue,
                        Err(e) if e.is_last() => break,
                        Err(e) => return Err(e),
                    }
                }
                Ok(Value::hash_with_data(Value::hash_arc(result)))
            }
            // Single non-iterable value: try the block on it directly
            _ => self.duckmap_element(block, target, false),
        }
    }

    /// Recursively apply a block to every leaf element, preserving structure.
    pub(crate) fn deepmap_iterate(
        &mut self,
        block: &Value,
        target: &Value,
    ) -> Result<Value, RuntimeError> {
        self.deepmap_iterate_inner(block, target, false)
    }

    /// Call the deepmap block on a leaf element through a transient
    /// `ContainerRef` cell, so a mutating callable (`++*`, `*--`, `$_++`)
    /// writes through — Raku's `deepmap` passes each leaf as a *container*
    /// and mutations are visible in the source structure. Returns the
    /// block's (decontainerized) result plus the cell's post-call value for
    /// the caller to write back into the source slot.
    fn deepmap_leaf_call(
        &mut self,
        block: &Value,
        leaf: &Value,
    ) -> Result<(Value, Value), RuntimeError> {
        let cell = crate::gc::Gc::new(std::sync::Mutex::new(leaf.clone()));
        let res = self.call_sub_value(
            block.clone(),
            vec![Value::container_ref(cell.clone())],
            false,
        )?;
        let new_val = cell.lock().unwrap().clone();
        Ok((res.deref_container(), new_val))
    }

    /// Inner recursive helper. `itemize_result` is true for nested calls
    /// so that sublists get wrapped in Scalar containers.
    fn deepmap_iterate_inner(
        &mut self,
        block: &Value,
        target: &Value,
        itemize_result: bool,
    ) -> Result<Value, RuntimeError> {
        // ADR-0040 slices 1-2: a `Range`/`Seq` stored as a real `Array`/`Hash`
        // element is itemized as `Scalar(inner)`. The leaf-vs-descend decision
        // is about the VALUE, not about whether it sits in a container, so see
        // through the element's own itemization — the *result*'s itemization is
        // decided independently by `itemize_result`. (An itemized `Array`/
        // `Hash` element needs no unwrapping: its itemization is a kind/flag,
        // so it still matches `ValueView::Array`/`ValueView::Hash`.)
        let target = target.descalarize();
        if let Some(list) = range_as_list(target) {
            return self.deepmap_iterate_inner(block, &list, itemize_result);
        }
        // This construct handles `next`/`last`/`redo`, so a loop-control
        // statement raised anywhere in its dynamic extent has somewhere to go
        // (`runtime/loop_handler_depth.rs`). Without the guard the raise site
        // would convert the signal into a thrown `X::ControlFlow` and silently
        // break this loop.
        let _loop_handler = crate::runtime::loop_handler_depth::LoopHandlerGuard::new();
        match target.view() {
            // Type objects (e.g. Array, Hash) — return as-is to avoid hanging
            ValueView::Package(_) => Ok(target.clone()),
            ValueView::Array(items, kind) => {
                // A sublist is itemized (wrapped in a Scalar container) only when
                // its *parent* is a List, not when the parent is a real Array.
                // Compare Rakudo: `(1,[2,3]).deepmap(*+1)` -> `(2, $[3, 4])` but
                // `[1,[2,3]].deepmap(*+1)` -> `[2, [3, 4]]`.
                let child_itemize = !kind.is_real_array();
                let mut result = Vec::new();
                for (idx, item) in items.iter().enumerate() {
                    let is_leaf = deepmap_element_is_leaf(item);
                    if is_leaf {
                        match self.deepmap_leaf_call(block, item) {
                            Ok((v, new_src)) => {
                                if new_src != *item {
                                    // Write the mutated leaf back into the source
                                    // array in place so all holders of the Arc see
                                    // it (Raku container semantics).
                                    // SAFETY: aliased in-place mutation of a shared
                                    // container; see `gc_contents_mut`.
                                    unsafe {
                                        crate::value::gc_contents_mut(&items).items_mut()[idx] =
                                            new_src;
                                    }
                                }
                                result.push(v);
                            }
                            Err(e) if e.is_next() => continue,
                            Err(e) => return Err(e),
                        }
                        continue;
                    }
                    match self.deepmap_iterate_inner(block, item, child_itemize) {
                        Ok(v) => result.push(v),
                        Err(e) if e.is_next() => continue,
                        Err(e) => return Err(e),
                    }
                }
                let mut use_real_array = kind.is_real_array();
                // If the source array has a type constraint (e.g. `my Str @a`)
                // and the mapped values don't conform, downgrade to a List.
                if use_real_array && let Some(info) = self.container_type_metadata(target) {
                    let vt = &info.value_type;
                    if !vt.is_empty() && result.iter().any(|v| !v.isa_check(vt)) {
                        use_real_array = false;
                    }
                }
                let arr_kind = if use_real_array {
                    if itemize_result {
                        crate::value::ArrayKind::ItemArray
                    } else {
                        crate::value::ArrayKind::Array
                    }
                } else if itemize_result {
                    crate::value::ArrayKind::ItemList
                } else {
                    crate::value::ArrayKind::List
                };
                Ok(Value::array_with_kind(
                    crate::gc::Gc::new(crate::value::ArrayData::new(result)),
                    arr_kind,
                ))
            }
            ValueView::Seq(items) => {
                let mut result = Vec::new();
                for item in items.iter() {
                    match self.deepmap_iterate_inner(block, item, true) {
                        Ok(v) => result.push(v),
                        Err(e) if e.is_next() => continue,
                        Err(e) => return Err(e),
                    }
                }
                if itemize_result {
                    // Itemize the result as a list
                    Ok(Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(result)),
                        crate::value::ArrayKind::ItemList,
                    ))
                } else {
                    Ok(Value::seq(result))
                }
            }
            ValueView::Hash(map) => {
                let mut result = std::collections::HashMap::new();
                for (k, v) in map.iter() {
                    let is_leaf = deepmap_element_is_leaf(v);
                    if is_leaf {
                        match self.deepmap_leaf_call(block, v) {
                            Ok((val, new_src)) => {
                                if let ValueView::Slip(items) = val.view()
                                    && items.is_empty()
                                {
                                    continue;
                                }
                                if new_src != *v {
                                    // Write the mutated leaf back into the source
                                    // hash in place (see the Array arm).
                                    // SAFETY: aliased in-place mutation of a shared
                                    // container; see `gc_contents_mut`.
                                    unsafe {
                                        crate::value::gc_contents_mut(&map)
                                            .map
                                            .insert(k.clone(), new_src);
                                    }
                                }
                                result.insert(k.clone(), val);
                            }
                            Err(e) if e.is_next() => continue,
                            Err(e) => return Err(e),
                        }
                        continue;
                    }
                    match self.deepmap_iterate_inner(block, v, true) {
                        Ok(val) => {
                            // Empty slip means the block returned Empty;
                            // drop the key from the result hash.
                            if let ValueView::Slip(items) = val.view()
                                && items.is_empty()
                            {
                                continue;
                            }
                            result.insert(k.clone(), val);
                        }
                        Err(e) if e.is_next() => continue,
                        Err(e) => return Err(e),
                    }
                }
                Ok(Value::hash_with_data(Value::hash_arc(result)))
            }
            // Leaf value: apply the block (through a transient container so
            // mutating callables write through to a bare top-level leaf too).
            _ => self.deepmap_leaf_call(block, target).map(|(v, _)| v),
        }
    }

    /// `nodemap` — apply a block to each element without descending into sublists.
    pub(crate) fn nodemap_iterate(
        &mut self,
        block: &Value,
        target: &Value,
    ) -> Result<Value, RuntimeError> {
        if let Some(list) = range_as_list(target) {
            return self.nodemap_iterate(block, &list);
        }
        // This construct handles `next`/`last`/`redo`, so a loop-control
        // statement raised anywhere in its dynamic extent has somewhere to go
        // (`runtime/loop_handler_depth.rs`). Without the guard the raise site
        // would convert the signal into a thrown `X::ControlFlow` and silently
        // break this loop.
        let _loop_handler = crate::runtime::loop_handler_depth::LoopHandlerGuard::new();
        match target.view() {
            // nodemap always returns a List, even from a real Array or a Seq.
            // Compare Rakudo: `[2,3].nodemap(*+1).WHAT` is `List`.
            ValueView::Array(items, _kind) => {
                let mut result = Vec::new();
                for item in items.iter() {
                    match self.call_sub_value(block.clone(), vec![item.clone()], false) {
                        Ok(v) => result.push(v),
                        Err(e) if e.is_next() => continue,
                        Err(e) => return Err(e),
                    }
                }
                Ok(Value::array(result))
            }
            ValueView::Seq(items) => {
                let mut result = Vec::new();
                for item in items.iter() {
                    match self.call_sub_value(block.clone(), vec![item.clone()], false) {
                        Ok(v) => result.push(v),
                        Err(e) if e.is_next() => continue,
                        Err(e) => return Err(e),
                    }
                }
                Ok(Value::array(result))
            }
            // On an Associative, nodemap acts on the values, keeping the keys
            // (raku: "it will act on the values"), and returns a Hash.
            ValueView::Hash(map) => {
                let mut result = std::collections::HashMap::new();
                for (k, v) in map.iter() {
                    match self.call_sub_value(block.clone(), vec![v.clone()], false) {
                        Ok(mapped) => {
                            result.insert(k.clone(), mapped);
                        }
                        Err(e) if e.is_next() => continue,
                        Err(e) => return Err(e),
                    }
                }
                Ok(Value::hash_with_data(Value::hash_arc(result)))
            }
            // Single value: apply the block directly
            _ => self.call_sub_value(block.clone(), vec![target.clone()], false),
        }
    }

    /// Apply duckmap to a single element: try the block, on failure descend.
    /// `itemize` is true when this element sits in a List/Seq/Hash parent —
    /// rakudo itemizes what a descend returns there (`(1, (2, 3)).duckmap(->
    /// Int $x { $x * 10 })` is `(10, $(20, 30))`), so the sublist is one
    /// element of the result rather than something that can flatten. A real
    /// Array parent does not itemize (same rule as deepmap).
    fn duckmap_element(
        &mut self,
        block: &Value,
        value: &Value,
        itemize: bool,
    ) -> Result<Value, RuntimeError> {
        // This construct handles `next`/`last`/`redo`, so a loop-control
        // statement raised anywhere in its dynamic extent has somewhere to go
        // (`runtime/loop_handler_depth.rs`). Without the guard the raise site
        // would convert the signal into a thrown `X::ControlFlow` and silently
        // break this loop.
        let _loop_handler = crate::runtime::loop_handler_depth::LoopHandlerGuard::new();
        let list_kind = |itemize: bool| {
            if itemize {
                crate::value::ArrayKind::ItemList
            } else {
                crate::value::ArrayKind::List
            }
        };
        let with_kind = |result: Vec<Value>, kind: crate::value::ArrayKind| {
            Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(result)),
                kind,
            )
        };
        // Try to call the block with this value
        match self.call_sub_value(block.clone(), vec![value.clone()], false) {
            Ok(result) => Ok(result),
            Err(e) if e.is_next() || e.is_last() || e.is_redo() => {
                // Propagate loop control signals (next, last, redo)
                Err(e)
            }
            Err(_) => {
                // Block rejected the value (type mismatch, etc.)
                // Try to descend into iterable structures
                if let Some(list) = range_as_list(value) {
                    let mut result = Vec::new();
                    for item in list.as_list_items().unwrap_or_default() {
                        result.push(self.duckmap_element(block, item, true)?);
                    }
                    return Ok(with_kind(result, list_kind(itemize)));
                }
                match value.view() {
                    ValueView::Array(items, kind) => {
                        let child_itemize = !kind.is_real_array();
                        let mut result = Vec::new();
                        for item in items.iter() {
                            result.push(self.duckmap_element(block, item, child_itemize)?);
                        }
                        let arr_kind = if kind.is_real_array() {
                            if itemize {
                                crate::value::ArrayKind::ItemArray
                            } else {
                                crate::value::ArrayKind::Array
                            }
                        } else {
                            list_kind(itemize)
                        };
                        Ok(with_kind(result, arr_kind))
                    }
                    ValueView::Seq(items) => {
                        let mut result = Vec::new();
                        for item in items.iter() {
                            result.push(self.duckmap_element(block, item, true)?);
                        }
                        // A Seq descend comes back as a List (rakudo itemizes
                        // it into an itemized *List*, not a Seq).
                        Ok(with_kind(result, list_kind(itemize)))
                    }
                    ValueView::Hash(map) => {
                        let mut result = std::collections::HashMap::new();
                        for (k, v) in map.iter() {
                            result.insert(k.clone(), self.duckmap_element(block, v, true)?);
                        }
                        let hash = Value::hash_with_data(Value::hash_arc(result));
                        if itemize {
                            Ok(Value::scalar(hash))
                        } else {
                            Ok(hash)
                        }
                    }
                    // Not iterable — return unchanged
                    _ => Ok(value.clone()),
                }
            }
        }
    }
}
