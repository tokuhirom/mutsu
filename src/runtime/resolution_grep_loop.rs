//! The inline `.grep` loop: a callback run over a list of source elements,
//! reporting the matches, the `$_`-written-back source elements and the
//! matched indices. A deferred `.grep` pulled a prefix at a time runs it over
//! a [`GrepFeed`] instead of a copied chunk, so one loop run serves a whole
//! pull however many source elements it has to skip (#11515).

use super::*;
use crate::runtime::map_grep_plan::{InlineLoopKind, MapGrepPlanSlot};
use crate::runtime::resolution_map_grep::bind_loop_topic;
use crate::value::ValueView;

/// What a grep over a list of items produced: the matched values, the source
/// items after any `$_` write-backs, and — when the grep consumed one element
/// per iteration — the source index each matched value came from.
///
/// See [`Interpreter::eval_grep_over_items_with_mutated`] for why the indices
/// are reported by the loop rather than re-derived by the caller.
pub(crate) type GrepOutcome = (Value, Vec<Value>, Option<Vec<usize>>);

/// Where a lazily fed grep loop reads its source elements: `fetch(i)` is the
/// `i`-th element from where the pull starts, read when the loop reaches it,
/// `None` past the end. A Live array's elements are therefore read as they
/// are when the loop gets there, as Rakudo's array iterator reads them.
pub(crate) struct GrepFeed<'a> {
    fetch: &'a dyn Fn(usize) -> Option<Value>,
    /// The loop stops after this many matches.
    pub(crate) max_matches: usize,
}

impl<'a> GrepFeed<'a> {
    // Cost: O(1).
    pub(crate) fn new(fetch: &'a dyn Fn(usize) -> Option<Value>, max_matches: usize) -> Self {
        Self { fetch, max_matches }
    }

    // Cost: O(1) plus `fetch`'s cost.
    fn fetch(&self, i: usize) -> Option<Value> {
        (self.fetch)(i)
    }

    /// The first `max_matches` elements: what a chunked pull greps when the
    /// callback cannot run over a feed (every match needs a source element,
    /// so the chunk never runs the callback over one the pull does not need).
    // Cost: O(max_matches).
    fn window(self) -> Vec<Value> {
        (0..self.max_matches).map_while(|i| self.fetch(i)).collect()
    }
}

impl Interpreter {
    /// Run `func` over `list_items`, returning the matched values, the
    /// (possibly `$_`-mutated) source items, and — when the grep consumed one
    /// element per iteration — the source index each matched value came from.
    ///
    /// The indices are reported by the loop itself rather than re-derived by the
    /// caller. Recovering them afterwards by scanning the source for a value
    /// `===` to each result element is only correct when a matched element is
    /// still recognisable in the source: a `Proxy` element reaches the result as
    /// its FETCHed value while the source slot still holds the `Proxy`, so the
    /// scan skipped it — which silently *dropped* the element from the result
    /// (the caller rebuilds the result from the located slots) and shifted every
    /// `:k`/`:kv`/`:p` key after it.
    ///
    /// `None` means there is no one-to-one element/slot mapping to report: a
    /// multi-parameter block (`grep -> $a, $b { ... }`) consumes the source in
    /// chunks, so a matched value corresponds to a *range* of source slots.
    pub(super) fn eval_grep_over_items_with_mutated(
        &mut self,
        func: Option<Value>,
        list_items: Vec<Value>,
    ) -> Result<GrepOutcome, RuntimeError> {
        self.eval_grep_over_items_planned(func, list_items, &mut MapGrepPlanSlot::default())
    }

    /// [`Self::eval_grep_over_items_with_mutated`] with the callback's loop
    /// plan kept in `slot` across calls (see `runtime/map_grep_plan.rs`).
    // Cost: one callback call per element (per `arity` elements for a
    // multi-parameter block).
    pub(crate) fn eval_grep_over_items_planned(
        &mut self,
        func: Option<Value>,
        list_items: Vec<Value>,
        slot: &mut MapGrepPlanSlot,
    ) -> Result<GrepOutcome, RuntimeError> {
        self.eval_grep_loop(func, list_items, None, slot)
    }

    /// [`Self::eval_grep_over_items_planned`] over the source elements `feed`
    /// reads, stopping at its `max_matches`-th match: one loop run (one env
    /// merge, one register reset) for a whole prefix pull, however many source
    /// elements it skips. The returned source items are the elements the loop
    /// consumed — the pull advances by their count.
    // Cost: one callback call per element consumed, up to the
    // `max_matches`-th match.
    pub(crate) fn eval_grep_over_feed_planned(
        &mut self,
        func: Option<Value>,
        feed: GrepFeed<'_>,
        slot: &mut MapGrepPlanSlot,
    ) -> Result<GrepOutcome, RuntimeError> {
        self.eval_grep_loop(func, Vec::new(), Some(feed), slot)
    }

    /// The grep loop: over `list_items`, or — with `feed` — over the elements
    /// it reads, `list_items` collecting them as they are consumed.
    // Cost: see the two callers.
    fn eval_grep_loop(
        &mut self,
        func: Option<Value>,
        mut list_items: Vec<Value>,
        mut feed: Option<GrepFeed<'_>>,
        slot: &mut MapGrepPlanSlot,
    ) -> Result<GrepOutcome, RuntimeError> {
        // This construct handles `next`/`last`/`redo`, so a loop-control
        // statement raised anywhere in its dynamic extent has somewhere to go
        // (`runtime/loop_handler_depth.rs`). Without the guard the raise site
        // would convert the signal into a thrown `X::ControlFlow` and silently
        // break this loop.
        let _loop_handler = crate::runtime::loop_handler_depth::LoopHandlerGuard::new();
        // Look through a role-mixed callable (`&foo but R1`) so the
        // compile-once fast path below (which requires a bare `Sub`) still
        // takes it, instead of silently falling through to smartmatch-style
        // filtering (which never truthily matches a Mixin, dropping every
        // element) -- see `todo/tickets/map-rejects-role-mixed-sub-as-callable.md`.
        let func = func.map(Self::unwrap_callable_mixin);
        if let Some(func_ref) = func.as_ref()
            && let ValueView::Sub(data) = func_ref.view()
        {
            let data = data.clone();
            let mut result = Vec::new();
            // Source index of each matched element, recorded by the loop (see
            // the doc comment): only meaningful for a one-element-per-iteration
            // grep, so it is discarded below when `arity > 1`.
            let mut matched: Vec<usize> = Vec::new();
            // A destructuring sub-signature (`grep -> [ \a, \u, \v ] { u %% v }`) has to go
            // through the real binder. The fast path below inserts each parameter into the
            // env *by name*, which cannot take an element apart, so the inner names stayed
            // unbound. `map` already routes such signatures to `call_sub_value`.
            // A plan in `slot` means an earlier chunk of this same Seq already
            // classified the callback as inline-loop material (see the rw map).
            let cached_plan = self.cached_inline_loop_plan(&data, InlineLoopKind::Grep, slot);
            let needs_full_binding = cached_plan.is_none()
                && (data
                    .param_defs
                    .iter()
                    .any(|pd| pd.sub_signature.is_some() || pd.outer_sub_signature.is_some())
                    || !data.assumed_positional.is_empty()
                || !data.assumed_named.is_empty()
                // A body-less routine Sub (plan-derived, ADR-0019 C6e-3)
                // carries only bytecode — the compile-the-AST fast path below
                // would evaluate an empty predicate; run the real call path.
                    || (data.body.is_empty() && data.compiled_routine.is_some())
                    || super::resolution_map_grep::sub_reads_block_var(&data));
            if needs_full_binding {
                if let Some(feed) = feed.take() {
                    list_items = feed.window();
                }
                let mut matched = Vec::new();
                for (i, item) in list_items.iter().enumerate() {
                    let callable = Value::sub_value(data.clone());
                    let pred =
                        if !data.assumed_positional.is_empty() || !data.assumed_named.is_empty() {
                            self.vm_call_on_value(callable, vec![item.clone()], None)?
                        } else {
                            self.call_sub_value(callable, vec![item.clone()], false)?
                        };
                    if self.eval_predicate_truthy(&pred) {
                        result.push(item.clone());
                        matched.push(i);
                    }
                }
                return Ok((Value::array(result), list_items, Some(matched)));
            }
            let arity = crate::runtime::map_grep_plan::inline_loop_arity(&data);
            // Only the inline loop below reads a feed lazily, and only one
            // element per call; anything else greps the window a chunked pull
            // used to copy.
            if (arity != 1
                || (cached_plan.is_none()
                    && super::resolution_map_grep::sub_is_call_carrier(&data)))
                && let Some(feed) = feed.take()
            {
                list_items = feed.window();
            }
            // Carrier Subs (.assuming wrapper, composed callable, multi-candidate
            // dispatcher) — delegate to call_sub_value which resolves the markers.
            if cached_plan.is_none() && super::resolution_map_grep::sub_is_call_carrier(&data) {
                let mut i = 0usize;
                let mut matched = Vec::new();
                while i < list_items.len() {
                    if arity > 1 && i + arity > list_items.len() {
                        break;
                    }
                    let chunk: Vec<Value> = if arity == 1 {
                        vec![list_items[i].clone()]
                    } else {
                        list_items[i..i + arity].to_vec()
                    };
                    let pred =
                        self.call_sub_value(Value::sub_value(data.clone()), chunk.clone(), false)?;
                    if self.eval_predicate_truthy(&pred) {
                        if arity == 1 {
                            result.push(chunk[0].clone());
                        } else {
                            result.push(Value::array(chunk));
                        }
                        matched.push(i);
                    }
                    i += arity;
                }
                // A chunked grep has no one-to-one element/slot mapping.
                let matched = (arity == 1).then_some(matched);
                return Ok((Value::array(result), list_items, matched));
            }

            // Compile once, reuse VM for every iteration (and reuse a cached
            // compile across repeated calls to this same closure literal —
            // see `compile_loop_block_cached`). `return` inside this block
            // should propagate up to the lexically enclosing routine (if
            // any); `compile_loop_block_cached` marks the compiler as
            // lexically nested in a routine whenever one is currently on the
            // dynamic call stack. The plan also holds the capture merge's
            // classification (`runtime/map_grep_plan.rs`).
            //
            // Same caller-priority merge `eval_map_over_items` makes, and for
            // the same reasons (`enter_inline_loop_env`). Grep used to
            // overwrite EVERY captured name unconditionally while saving only
            // the names the caller did not already have, so a name present in
            // both was clobbered with the capture-time value and never put
            // back. That became visible the moment ADR-0058 step 3b deferred
            // grep to its pull: `my $s = (1,2,3,4).grep({ $_ %% 2 }); $s.elems`
            // left `$s` as `Any`, because `s` was captured while the
            // `my $s = ...` statement was still evaluating its own RHS.
            let plan = match cached_plan {
                Some(plan) => plan,
                None => self.inline_loop_plan(&data, InlineLoopKind::Grep, slot),
            };
            let (code, compiled_fns) = (&plan.code, &plan.fns);
            let topic_source_key = crate::symbol::wk::grep_topic_source();
            let saved = self.enter_inline_loop_env(&data, &plan);

            let keeps_outer_topic = plan.keeps_outer_topic;
            let outer_topic = self.env.get_sym(crate::symbol::wk::topic()).cloned();
            // See `CompiledCode::immutable_topic` / `set_loop_topic_readonly`.
            let immutable_topic = plan.immutable_topic;

            // CP-3 collapse: run the grep loop with fresh execution registers
            // (replaces the `mem::take(self)` + `VM::new` sub-VM). The closure
            // returns Ok(()) / Err on the loop; `with_nested_registers` restores
            // the outer registers and flags env_dirty. The `saved` env restore is
            // hoisted to after the call (ran on every old exit path).
            // Runtime transitive vouching: see `frame_authoritative_set`.
            let block_authoritative = &plan.block_authoritative;
            // ADR-0027: see the matching comment in `eval_map_over_items`
            // (`resolution_map_grep.rs`).
            let block_owned = &data.owned_captures;
            let loop_result: Result<(), RuntimeError> = self.with_nested_registers(|vm| {
                // Scope `state` variables to the closure instance (see
                // `eval_map_over_items`).
                vm.state_scope_id.set(Some(data.id));
                let mut i = 0usize;
                let mut stop = false;
                loop {
                    if let Some(feed) = &feed {
                        // `i` is always the next element to read here: a feed
                        // runs one element per call.
                        if result.len() >= feed.max_matches {
                            break;
                        }
                        match feed.fetch(i) {
                            Some(item) => list_items.push(item),
                            None => break,
                        }
                    } else if i >= list_items.len() {
                        break;
                    }
                    if arity > 1 && i + arity > list_items.len() {
                        break;
                    }
                    let chunk: Vec<Value> = if arity == 1 {
                        vec![list_items[i].clone()]
                    } else {
                        list_items[i..i + arity].to_vec()
                    };
                    'body_redo: loop {
                        vm.frame_authoritative = block_authoritative.clone();
                        vm.frame_owned = block_owned.clone();
                        {
                            let assumed_count = data.assumed_positional.len();
                            for (idx, val) in data.assumed_positional.iter().enumerate() {
                                if let Some(&p) = plan.param_syms.get(idx) {
                                    vm.env_mut().insert_sym_noting(p, val.clone());
                                }
                            }
                            if arity == 1 {
                                if let Some(&p) = plan.param_syms.get(assumed_count) {
                                    vm.env_mut().insert_sym_noting(p, chunk[0].clone());
                                }
                                bind_loop_topic(
                                    vm.env_mut(),
                                    &chunk[0],
                                    keeps_outer_topic,
                                    &outer_topic,
                                );
                                if !keeps_outer_topic {
                                    vm.env_mut().insert_sym(topic_source_key, chunk[0].clone());
                                }
                            } else {
                                for (idx, &p) in
                                    plan.param_syms.iter().skip(assumed_count).enumerate()
                                {
                                    if idx < chunk.len() {
                                        vm.env_mut().insert_sym_noting(p, chunk[idx].clone());
                                    }
                                }
                                bind_loop_topic(
                                    vm.env_mut(),
                                    &chunk[0],
                                    keeps_outer_topic,
                                    &outer_topic,
                                );
                            }
                        }
                        // `$_` holding the caller's topic is not an alias for the
                        // element, so it must not write back to it.
                        vm.set_topic_source_var(
                            (arity == 1 && !keeps_outer_topic)
                                .then(|| topic_source_key.as_str().to_string()),
                        );
                        let saved_when_matched = vm.when_matched();
                        // Same per-iteration readonly scope the two map loops
                        // open: this loop also binds the block's params by a
                        // direct `env.insert` and runs the body through
                        // `run_reuse`, without `push_call_frame`, so a mark made
                        // inside it would otherwise skip the undo journal and
                        // leak permanently (see `mark_readonly_sym_with`).
                        let _readonly_guard =
                            crate::vm::vm_call_state_guard::ReadonlyFrameGuard::new(vm);
                        vm.mark_placeholder_params_readonly(&data.params);
                        super::resolution_map_grep::set_loop_topic_readonly(vm, immutable_topic);
                        match vm.run_reuse(code, compiled_fns) {
                            Ok(()) => {
                                let pred = vm
                                    .last_stack_value()
                                    .cloned()
                                    .or_else(|| vm.env().get("_").cloned())
                                    .unwrap_or(Value::NIL);
                                let updated_item = if arity == 1 {
                                    vm.env()
                                        .get_sym(topic_source_key)
                                        .cloned()
                                        .unwrap_or_else(|| chunk[0].clone())
                                } else {
                                    chunk[0].clone()
                                };
                                if arity == 1 {
                                    list_items[i] = updated_item.clone();
                                }
                                if vm.eval_predicate_truthy(&pred) {
                                    if arity == 1 {
                                        result.push(updated_item);
                                    } else {
                                        result.push(Value::array(chunk));
                                    }
                                    matched.push(i);
                                }
                                break 'body_redo;
                            }
                            Err(e) if e.is_redo() => continue 'body_redo,
                            Err(e) if e.is_next() => break 'body_redo,
                            Err(e) if e.is_last() => {
                                vm.async_state.map_grep_last_depth =
                                    Some(crate::runtime::loop_handler_depth::loop_handler_depth());
                                stop = true;
                                break 'body_redo;
                            }
                            // A matched `when`/`default` escapes as a succeed
                            // signal instead of returning normally — its value
                            // is the predicate result, same as the `Ok` arm.
                            Err(e) if e.is_succeed() => {
                                vm.set_when_matched(saved_when_matched);
                                let pred = e.return_value.unwrap_or(Value::NIL);
                                let updated_item = if arity == 1 {
                                    vm.env()
                                        .get_sym(topic_source_key)
                                        .cloned()
                                        .unwrap_or_else(|| chunk[0].clone())
                                } else {
                                    chunk[0].clone()
                                };
                                if arity == 1 {
                                    list_items[i] = updated_item.clone();
                                }
                                if vm.eval_predicate_truthy(&pred) {
                                    if arity == 1 {
                                        result.push(updated_item);
                                    } else {
                                        result.push(Value::array(chunk));
                                    }
                                    matched.push(i);
                                }
                                break 'body_redo;
                            }
                            Err(e) => {
                                return Err(e);
                            }
                        }
                    }
                    if stop {
                        break;
                    }
                    i += arity;
                }
                Ok(())
            });

            self.leave_inline_loop_env(saved);
            if loop_result.is_ok() {
                self.record_eager_block_free_var_writeback(code, &data.params);
            }
            loop_result?;
            // A chunked grep has no one-to-one element/slot mapping.
            let matched = (arity == 1).then_some(matched);
            return Ok((Value::array(result), list_items, matched));
        }
        if let Some(pattern) = func {
            if matches!(pattern.view(), ValueView::Bool(_)) {
                return Err(RuntimeError::match_bool(".grep"));
            }
            let mut result = Vec::new();
            let mut matched = Vec::new();
            if let Some(feed) = feed {
                while result.len() < feed.max_matches
                    && let Some(item) = feed.fetch(list_items.len())
                {
                    if self.smart_match(&item, &pattern) {
                        result.push(item.clone());
                        matched.push(list_items.len());
                    }
                    list_items.push(item);
                }
                return Ok((Value::array(result), list_items, Some(matched)));
            }
            for (i, item) in list_items.iter().enumerate() {
                if self.smart_match(item, &pattern) {
                    result.push(item.clone());
                    matched.push(i);
                }
            }
            return Ok((Value::array(result), list_items, Some(matched)));
        }
        if let Some(func) = func {
            let mut result = Vec::new();
            let mut matched = Vec::new();
            for (i, item) in list_items.iter().enumerate() {
                let pred = self.call_sub_value(func.clone(), vec![item.clone()], false)?;
                if self.eval_predicate_truthy(&pred) {
                    result.push(item.clone());
                    matched.push(i);
                }
            }
            return Ok((Value::array(result), list_items, Some(matched)));
        }
        if let Some(feed) = feed {
            list_items = feed.window();
        }
        let all = (0..list_items.len()).collect();
        Ok((Value::array(list_items.clone()), list_items, Some(all)))
    }
}
