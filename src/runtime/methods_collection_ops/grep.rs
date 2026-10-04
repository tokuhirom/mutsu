use super::*;
use crate::ast::ControlFlowKind;
use crate::ast_visit::{Visit, walk_expr, walk_stmt, walk_stmts};
use crate::value::ValueView;

impl Interpreter {
    /// Cost: O(e) matcher calls, e = elements of the invocant, eager on a finite
    /// source (a later `.head` does not stop it); `:k`/`:kv`/`:p` only change the
    /// shape of each O(1) result entry.
    pub(in crate::runtime) fn dispatch_grep(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // This construct handles `next`/`last`/`redo`, so a loop-control
        // statement raised anywhere in its dynamic extent has somewhere to go
        // (`runtime/loop_handler_depth.rs`). Without the guard the raise site
        // would convert the signal into a thrown `X::ControlFlow` and silently
        // break this loop.
        let _loop_handler = crate::runtime::loop_handler_depth::LoopHandlerGuard::new();
        // A role mixin over a list-ish value greps the inner elements (see
        // mixin_iteration_target on the map dispatch).
        let target = Self::mixin_iteration_target(target);
        // The matcher binds to grep's `Mu $test` parameter, which reads a
        // `Proxy` once: `@t.grep($obj.state)` with an `is rw` `state` returning
        // a Proxy (Tinky) greps by the value FETCH answers, not by the Proxy.
        let fetched_args;
        let args = match args.split_first() {
            Some((first, rest)) if first.is_proxy_value() => {
                fetched_args = std::iter::once(self.auto_fetch_proxy(first)?)
                    .chain(rest.iter().cloned())
                    .collect::<Vec<_>>();
                fetched_args.as_slice()
            }
            _ => args,
        };
        // Parse named adverbs (:k, :v, :kv, :p) from args
        let mut has_k = false;
        let mut has_kv = false;
        let mut has_p = false;
        let mut positional_args: Vec<Value> = Vec::new();
        for arg in args {
            match arg.view() {
                ValueView::Pair(key, value) if key == "k" => has_k = value.truthy(),
                ValueView::Pair(key, value) if key == "kv" => has_kv = value.truthy(),
                ValueView::Pair(key, value) if key == "p" => has_p = value.truthy(),
                ValueView::Pair(key, value) if key == "v" => {
                    if !value.truthy() {
                        return Err(RuntimeError::unexpected_adverb(
                            &["v".to_string()],
                            "grep",
                            crate::runtime::utils::value_type_name(&target),
                        ));
                    }
                    // :v is the default behavior, just ignore when truthy
                }
                ValueView::Pair(key, _) => {
                    return Err(RuntimeError::unexpected_adverb(
                        std::slice::from_ref(key),
                        "grep",
                        crate::runtime::utils::value_type_name(&target),
                    ));
                }
                _ => positional_args.push(arg.clone()),
            }
        }
        let grep_adverb = if has_k {
            GrepAdverb::K
        } else if has_kv {
            GrepAdverb::Kv
        } else if has_p {
            GrepAdverb::P
        } else {
            GrepAdverb::V
        };
        // Every `grep` candidate takes a matcher (`($: Bool:D $t, *%_)` and
        // `($: Mu $t, *%_)`), so a call without one resolves none of them
        // (#11630) -- adverbs or not.
        if positional_args.is_empty() {
            return Err(Self::grep_no_matcher_error(&target, args));
        }
        let args = &positional_args;

        // A not-yet-run `.map`/`.grep` receiver: chain onto its source so the
        // two callbacks interleave per element (#11176). The adverbed forms
        // need indices over the whole result, so they take it whole.
        let (chain, target) =
            self.map_grep_receiver_chain(target, matches!(grep_adverb, GrepAdverb::V))?;
        if let Some(items) = chain {
            return Ok(Value::seq_deferred(crate::value::SeqSource::MapGrep {
                items,
                pos: 0,
                func: args.first().cloned(),
                fatal: self.module.fatal_mode,
                mode: crate::value::MapGrepMode::Grep,
                plan: Default::default(),
            }));
        }

        // Infinite/lazy source with the default (`:v`) adverb: return a truly
        // lazy `grep` pipeline stage instead of materializing the (possibly
        // infinite) source. Adverbed greps (`:k`/`:kv`/`:p`) need positional
        // indices over the whole result, so they keep the eager path.
        if matches!(grep_adverb, GrepAdverb::V)
            && Self::is_lazy_pipe_source(&target)
            && let Some(func) = args.first().cloned()
            && let Some(pipe) = Self::make_lazy_pipe(target.clone(), func, true)
        {
            return Ok(pipe);
        }

        match target.view() {
            ValueView::Package(class_name) if class_name == "Supply" => Err(RuntimeError::new(
                "Cannot call .grep on a Supply type object",
            )),
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Supply" => {
                // On-demand source: the filtered supply taps it per tap of its
                // own (see `native_methods::supply_derive`).
                if attributes.as_map().contains_key("on_demand_callback") {
                    return Ok(Self::make_on_demand_derived_supply(
                        target.clone(),
                        crate::runtime::native_methods::TransformMode::Grep,
                        args.first().cloned().unwrap_or(Value::NIL),
                    ));
                }
                let source_values = attributes
                    .as_map()
                    .get("values")
                    .and_then(|v| {
                        if let ValueView::Array(items, ..) = v.view() {
                            Some(items.to_vec())
                        } else {
                            None
                        }
                    })
                    .unwrap_or_default();
                let filtered = self.eval_grep_over_items(args.first().cloned(), source_values)?;
                let filtered_values = Self::value_to_list(&filtered);
                let mut attrs = HashMap::new();
                attrs.insert("values".to_string(), Value::array(filtered_values));
                attrs.insert("taps".to_string(), Value::array(Vec::new()));
                attrs.insert(
                    "live".to_string(),
                    attributes
                        .as_map()
                        .get("live")
                        .cloned()
                        .unwrap_or(Value::FALSE),
                );
                Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
            }
            // A multi-dimensional shaped array is grepped over its leaves, as
            // `map`, `sort` and iteration see it -- not over its rows, and
            // without the promoting path below, which rebuilt the outer level
            // and so lost the shape of the array itself.
            ValueView::Array(items, crate::value::ArrayKind::Shaped)
                if items
                    .iter()
                    .any(|v| matches!(v.view(), ValueView::Array(..))) =>
            {
                let leaves = crate::runtime::utils::shaped_array_leaves(&target);
                self.eval_grep_with_adverb(args.first().cloned(), leaves, &grep_adverb)
            }
            ValueView::Array(items, _arr_kind) => {
                // ADR-0058 step 3b: with the default `:v` adverb the callback
                // runs when the Seq is CONSUMED, not here. The adverbed forms
                // (`:k`/`:kv`/`:p`) need positional indices over the whole
                // result, so they keep the eager path -- the same exemption
                // they already take from `make_lazy_pipe`.
                if matches!(grep_adverb, GrepAdverb::V) {
                    return Ok(Value::seq_deferred(crate::value::SeqSource::MapGrep {
                        items: crate::value::MapGrepItems::Live(target.clone()),
                        pos: 0,
                        func: args.first().cloned(),
                        fatal: self.module.fatal_mode,
                        mode: crate::value::MapGrepMode::GrepArray(target.clone()),
                        plan: Default::default(),
                    }));
                }
                self.grep_over_array_promoting(items.clone(), args.first().cloned(), &grep_adverb)
            }
            ValueView::Range(..)
            | ValueView::RangeExcl(..)
            | ValueView::RangeExclStart(..)
            | ValueView::RangeExclBoth(..) => {
                // Route integer ranges through the unified pull iterator so an
                // open-ended range (`1..Inf` == `Range(1, i64::MAX)`) is
                // truncated at the cap instead of panicking with
                // `capacity overflow` (ANALYSIS §8.2). This matches the cap the
                // `GenericRange` arm already uses via `value_to_list`.
                let items = crate::runtime::value_iterator::materialize_capped(
                    &target,
                    crate::runtime::utils::MAX_RANGE_EXPAND as usize,
                );
                // ADR-0058 step 3b: a range receiver has no source slots to
                // promote, so it defers as a plain `Grep`. The adverbed forms
                // stay eager -- they need indices over the whole result.
                if matches!(grep_adverb, GrepAdverb::V) {
                    return Ok(Value::seq_deferred(crate::value::SeqSource::MapGrep {
                        items: crate::value::MapGrepItems::Snapshot(std::sync::Arc::new(items)),
                        pos: 0,
                        func: args.first().cloned(),
                        fatal: self.module.fatal_mode,
                        mode: crate::value::MapGrepMode::Grep,
                        plan: Default::default(),
                    }));
                }
                self.eval_grep_with_adverb(args.first().cloned(), items, &grep_adverb)
            }
            ValueView::GenericRange { .. } => {
                if let ValueView::GenericRange {
                    start,
                    end,
                    excl_start,
                    ..
                } = target.view()
                {
                    let end_num = end.to_f64();
                    if end_num.is_infinite()
                        && end_num.is_sign_positive()
                        && let Some(func) = args.first().cloned()
                        && let ValueView::Sub(data) = func.view()
                        && body_contains_last(&data.body)
                    {
                        let mut current = start.to_f64() as i64;
                        if excl_start {
                            current += 1;
                        }
                        let mut result = Vec::new();
                        let mut result_indices = Vec::new();
                        let limit = 1_000_000usize;
                        let mut item_idx = 0usize;
                        while result.len() < limit {
                            let item = match start.as_ref().view() {
                                ValueView::Num(_) => Value::num(current as f64),
                                ValueView::Rat(_, den) => {
                                    crate::value::make_rat(current * den, den)
                                }
                                _ => Value::int(current),
                            };
                            'redo_item: loop {
                                match self.call_sub_value(func.clone(), vec![item.clone()], false) {
                                    Ok(pred) => {
                                        if self.eval_predicate_truthy(&pred) {
                                            result.push(item.clone());
                                            result_indices.push(item_idx);
                                        }
                                        break 'redo_item;
                                    }
                                    Err(e) if e.is_redo() => continue 'redo_item,
                                    Err(e) if e.is_next() => break 'redo_item,
                                    Err(e) if e.is_last() => {
                                        return grep_adverb.transform_result(
                                            Value::array(result),
                                            &result_indices,
                                        );
                                    }
                                    Err(e) => return Err(e),
                                }
                            }
                            current += 1;
                            item_idx += 1;
                        }
                        return grep_adverb.transform_result(Value::array(result), &result_indices);
                    }
                    if end_num.is_infinite() && end_num.is_sign_positive() {
                        // Preserve laziness for open-ended ranges in grep.
                        return Ok(target);
                    }
                }
                let items = crate::runtime::utils::value_to_list(&target);
                self.eval_grep_with_adverb(args.first().cloned(), items, &grep_adverb)
            }
            ValueView::Str(s) => {
                if let Some(ValueView::Sub(data)) = args.first().map(Value::view)
                    && let Some(Stmt::Expr(expr)) = data.body.last()
                    && matches!(
                        expr,
                        Expr::Literal(lit) | Expr::RegexLiteral { value: lit, .. }
                            if matches!(lit.view(), ValueView::Regex(_))
                    )
                {
                    return self.eval_grep_with_adverb(
                        args.first().cloned(),
                        vec![Value::str_arc(s.clone())],
                        &grep_adverb,
                    );
                }
                match grep_adverb {
                    GrepAdverb::K => Ok(Value::int(0)),
                    GrepAdverb::Kv => {
                        Ok(Value::array(vec![Value::int(0), Value::str_arc(s.clone())]))
                    }
                    GrepAdverb::P => {
                        Ok(Value::value_pair(Value::int(0), Value::str_arc(s.clone())))
                    }
                    GrepAdverb::V => Ok(Value::str_arc(s.clone())),
                }
            }
            ValueView::Seq(items) => {
                self.eval_grep_with_adverb(args.first().cloned(), items.to_vec(), &grep_adverb)
            }
            ValueView::Slip(items) => {
                self.eval_grep_with_adverb(args.first().cloned(), items.to_vec(), &grep_adverb)
            }
            ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..) => {
                let items = crate::runtime::utils::value_to_list(&target);
                self.eval_grep_with_adverb(args.first().cloned(), items, &grep_adverb)
            }
            ValueView::Hash(..) => {
                // A Hash held in a scalar is itemized as an element, but a
                // Hash receiver still iterates its own key-value pairs.
                let items = crate::runtime::utils::value_to_list_for_receiver(&target);
                self.eval_grep_with_adverb(args.first().cloned(), items, &grep_adverb)
            }
            ValueView::Instance { class_name, .. }
                if crate::value::types::is_stash_class_name(&class_name.resolve()) =>
            {
                let items = crate::runtime::utils::value_to_list(&target);
                self.eval_grep_with_adverb(args.first().cloned(), items, &grep_adverb)
            }
            ValueView::Uni(_) => {
                // A Uni/NFC/NFD/NFKC/NFKD receiver greps over its codepoints
                // (matches raku iteration; see `value_to_list_for_receiver`'s
                // doc comment).
                let items = crate::runtime::utils::value_to_list_for_receiver(&target);
                self.eval_grep_with_adverb(args.first().cloned(), items, &grep_adverb)
            }
            _ => {
                // A Blob/Buf greps over its bytes (matches raku iteration).
                if let Some(bytes) = Self::buf_as_byte_items(&target) {
                    return self.eval_grep_with_adverb(args.first().cloned(), bytes, &grep_adverb);
                }
                // Treat any other value as a single-element list for grep
                self.eval_grep_with_adverb(args.first().cloned(), vec![target], &grep_adverb)
            }
        }
    }

    /// Helper: run grep over items, compute indices, and apply adverb transformation.
    fn eval_grep_with_adverb(
        &mut self,
        func: Option<Value>,
        items: Vec<Value>,
        adverb: &GrepAdverb,
    ) -> Result<Value, RuntimeError> {
        let original_items = items.clone();
        let (filtered, matched_indices) = self.eval_grep_over_items_indexed(func, items)?;
        if matches!(adverb, GrepAdverb::V) {
            return Ok(filtered);
        }
        // Prefer the indices the grep loop reported: re-deriving them by
        // scanning the source for a value `===` to each result element cannot
        // find a `Proxy` slot, which shifts every key after it. The scan remains
        // the fallback for a chunked grep, which has no one-to-one mapping.
        let indices = match matched_indices {
            Some(indices) => indices,
            None => compute_grep_indices(&original_items, &filtered),
        };
        adverb.transform_result(filtered, &indices)
    }

    /// The `.grep` over a concrete array: run the callback, promote every
    /// MATCHED source slot to a shared element cell (so a writeback loop
    /// mutates through into the source), and build the result out of the same
    /// cells.
    ///
    /// Split out of `dispatch_grep`'s `ValueView::Array` arm for ADR-0058 step
    /// 3b: with the default `:v` adverb the arm is now DEFERRED, so this body
    /// runs at the pull instead of at the `.grep` call. That is only sound
    /// because the promotion is published by mutating the source `ArrayData` in
    /// place rather than by re-binding its name in the current frame -- the
    /// pull happens wherever the Seq is consumed, long after that frame is gone
    /// (`news/2026-09/grep-promotion-is-published-in-place.md`).
    pub(crate) fn grep_over_array_promoting(
        &mut self,
        items: crate::gc::Gc<crate::value::ArrayData>,
        func: Option<Value>,
        grep_adverb: &GrepAdverb,
    ) -> Result<Value, RuntimeError> {
        let len = items.len();
        self.grep_over_array_promoting_range(
            items,
            func,
            grep_adverb,
            0..len,
            None,
            &mut crate::runtime::map_grep_plan::MapGrepPlanSlot::default(),
        )
        .map(|(result, _)| result)
    }

    /// [`Self::grep_over_array_promoting`] over the source slots `range`
    /// only: a deferred `.grep` pulled a prefix at a time (#9158,
    /// `pull_map_grep_prefix`) greps the source a chunk at a time. The
    /// indices the `:k`/`:kv`/`:p` adverbs see are absolute. `plan` carries
    /// what earlier chunks computed about the callback
    /// (`runtime/map_grep_plan.rs`).
    ///
    /// With `max_matches`, the grep reads the slots from `range.start` on as
    /// it reaches them and stops at that many matches instead of at
    /// `range.end` (a prefix pull, #11515). Returns the result and the end of
    /// the slots it consumed.
    // Cost: one callback call per slot consumed, plus O(m) promotions,
    // m = matched slots.
    pub(crate) fn grep_over_array_promoting_range(
        &mut self,
        items: crate::gc::Gc<crate::value::ArrayData>,
        func: Option<Value>,
        grep_adverb: &GrepAdverb,
        range: std::ops::Range<usize>,
        max_matches: Option<usize>,
        plan: &mut crate::runtime::map_grep_plan::MapGrepPlanSlot,
    ) -> Result<(Value, usize), RuntimeError> {
        let start = range.start.min(items.len());
        let (filtered, mutated_items, matched_indices) = match max_matches {
            Some(max_matches) => {
                let fetch = |i: usize| items.get(start + i).cloned();
                let feed = crate::runtime::resolution_grep_loop::GrepFeed::new(&fetch, max_matches);
                self.eval_grep_over_feed_planned(func, feed, plan)?
            }
            None => {
                let end = range.end.min(items.len()).max(start);
                self.eval_grep_over_items_planned(func, items[start..end].to_vec(), plan)?
            }
        };
        let end = start + mutated_items.len();
        // Which source positions matched, so those slots can be shared
        // with the result as first-class element containers. The grep
        // loop reports them; they used to be re-derived here by scanning
        // the source for a value `===` to each result element, which
        // could not find a `Proxy` slot (the result holds the FETCHed
        // value, the slot holds the Proxy). The miss then truncated the
        // result below, because it is rebuilt from the located slots.
        //
        // `None` is a chunked grep (`grep -> $a, $b {...}`): no
        // one-to-one element/slot mapping, so nothing is aliased.
        let indices: Vec<usize> = matched_indices
            .unwrap_or_default()
            .into_iter()
            .map(|i| i + start)
            .collect();
        // Promote each matched source slot to a shared `ContainerRef`
        // cell and reference the SAME cells from the grep result. A
        // writeback loop (`for @a.grep(...) { $_++ }` / `@a.grep(...)>>++`)
        // then mutates THROUGH the cell into @a's slot via the ordinary
        // element-cell write path — no GrepView side channel needed. A
        // later `=` assignment (`my @g = @a.grep(...)`) decontainerizes the
        // cells, so the named copy owns its values and never writes back.
        let mut promoted = mutated_items;
        let mut shared_cells: Vec<Value> = Vec::with_capacity(indices.len());
        for &i in &indices {
            // A `:delete`d (or never-assigned) slot has no element
            // container to alias, and promoting it would *create* one:
            // `ArrayData::hole_at` recognises a hole by the gap marker
            // value (`Package("Any")`/the declared type) sitting in the
            // slot AND its absence from `initialized`, so wrapping that
            // marker in a `ContainerRef` makes the slot read as a live
            // element while `initialized` still calls it empty. The two
            // then disagree, and a later trailing-slot `:delete` stops
            // truncating the array (`@a[2]:delete; @a.grep({True});
            // @a[3]:delete` left 3 elements instead of 2). Hand the
            // grep result the raw marker instead — Raku yields `Any`
            // there, not an alias into a slot that does not exist.
            if items.hole_at(i) {
                shared_cells.push(promoted[i - start].clone());
                continue;
            }
            let cell = match promoted[i - start].view() {
                ValueView::ContainerRef(_) => promoted[i - start].clone(),
                _ => Value::container_ref(crate::gc::Gc::new(crate::value::ContainerCell::new(
                    promoted[i - start].clone(),
                ))),
            };
            promoted[i - start] = cell.clone();
            shared_cells.push(cell);
        }
        // Publish the promotion by mutating the source `ArrayData` IN
        // PLACE rather than building a replacement and re-binding every
        // name that pointed at the old one. Both are visible to every
        // alias, but the old route reached them through
        // `overwrite_array_bindings_by_identity`, which walks the
        // CURRENT frame's `env` — so it only ever found the aliases
        // that happened to be lexically visible right here, and needed
        // a `pending_rw_writeback_sources` drain to keep the caller's
        // local slot from going stale behind it. Writing through the
        // `Gc` (ADR-0013 §7 made this sound at the primitive) reaches
        // every alias by construction, needs no drain, and does not
        // depend on which frame is running — which is what ADR-0058
        // step 3b needs, since a deferred grep promotes at PULL time,
        // in a frame where the source's names are long gone.
        {
            let data = unsafe { crate::value::gc_contents_mut(&items) };
            let slots = data.items_mut();
            for (i, v) in promoted.into_iter().enumerate() {
                if start + i < slots.len() {
                    slots[start + i] = v;
                }
            }
        }
        // Build the result array from the shared cells (default `:v`
        // adverb). The `:k`/`:kv`/`:p` adverbs rebuild a fresh array in
        // `transform_result` from `indices`, which drops the aliasing (a
        // keys/pairs copy owns its values).
        //
        // The cells replace the result's items wholesale, so there must
        // be exactly one per matched element or the result would be
        // silently truncated -- which is what the old identity scan did
        // whenever it failed to locate a slot.
        let filtered = if !indices.is_empty()
            && let ValueView::Array(filtered_items, fkind) = filtered.view()
        {
            debug_assert_eq!(
                shared_cells.len(),
                filtered_items.len(),
                "grep aliasing must cover every matched element"
            );
            if shared_cells.len() != filtered_items.len() {
                Value::array_with_kind(filtered_items.clone(), fkind)
            } else {
                let mut data = (**filtered_items).clone();
                *data.items_mut() = shared_cells;
                Value::array_with_kind(crate::gc::Gc::new(data), fkind)
            }
        } else {
            filtered
        };
        grep_adverb
            .transform_result(filtered, &indices)
            .map(|result| (result, end))
    }
}

/// Whether a `last` appears anywhere in a grep matcher's body — in a nested
/// block or closure too, which the infinite-range path above treats as the
/// matcher's way of ending the search (ADR-0137 visitor).
// Cost: O(n), n = size of the AST of `body`; stops at the first `last`.
fn body_contains_last(body: &[Stmt]) -> bool {
    let mut scan = ContainsLast(false);
    walk_stmts(&mut scan, body);
    scan.0
}

struct ContainsLast(bool);

impl<'ast> Visit<'ast> for ContainsLast {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.0 {
            return;
        }
        if matches!(stmt, Stmt::Last(_)) {
            self.0 = true;
        } else {
            walk_stmt(self, stmt);
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if self.0 {
            return;
        }
        if matches!(
            expr,
            Expr::ControlFlow {
                kind: ControlFlowKind::Last,
                ..
            }
        ) {
            self.0 = true;
        } else {
            walk_expr(self, expr);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn contains_last(src: &str) -> bool {
        let stmts = crate::parse_dispatch::parse_source(src)
            .map(|(stmts, _)| stmts)
            .unwrap();
        body_contains_last(&stmts)
    }

    #[test]
    fn a_last_in_any_position_is_found() {
        assert!(contains_last("last if $_ > 3; True"));
        assert!(contains_last("my $s = \"{ last if $_ > 3 }\"; True"));
        assert!(contains_last("foo(:x($_ > 3 && last)); True"));
        assert!(!contains_last("next if $_ > 3; True"));
        assert!(!contains_last("say 'last'; True"));
    }
}
