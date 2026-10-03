//! Pulling a deferred `.map` / `.grep` Seq (`SeqSource::MapGrep`,
//! docs/adr/0058): the callback runs when the Seq is consumed, not at the
//! `.map` call.
//!
//! A full pull runs the callback over every source element not yet pulled.
//! A prefix pull (#9158) runs it over only as many elements as the consumer
//! needs, as Rakudo's pull-one iterator does: a consuming `.head(n)` /
//! `.first` (`take_seq_prefix`) and a boolification (`?@a.grep(...)`,
//! `eval_truthy`, which keeps the Seq and resumes it later) both go through
//! [`Interpreter::pull_map_grep_prefix`].
//!
//! The prefix pull drives the ordinary eager loops (`eval_map_over_items`,
//! `eval_grep_over_items`, the rw map, the promoting grep) over successive
//! chunks of the source. A chunk is exactly as long as the number of
//! elements still missing: every element produced needs at least one source
//! element, so the callback never runs over an element the consumer did not
//! need.

use super::*;
use crate::runtime::map_grep_plan::MapGrepPlanSlot;
use crate::value::{MapGrepItems, MapGrepMode, SeqSource};

impl Interpreter {
    /// Pull every not-yet-pulled element of a deferred `.map`/`.grep`
    /// (`source` must be a [`SeqSource::MapGrep`]).
    // Cost: one callback call per source element from `pos` on, e - pos,
    // e = source elements.
    pub(super) fn pull_map_grep_rest(
        &mut self,
        source: &SeqSource,
    ) -> Result<Vec<Value>, RuntimeError> {
        let SeqSource::MapGrep {
            items,
            pos,
            func,
            fatal,
            mode,
            plan,
        } = source
        else {
            return Ok(Vec::new());
        };
        // One loop run over the whole rest: the plan is built once either
        // way, so a scratch copy of the slot costs nothing.
        let mut plan = plan.clone();
        if let MapGrepItems::Chain(chain) = items {
            let mut pos = *pos;
            return self
                .pull_map_grep_chain(
                    func,
                    *fatal,
                    mode,
                    &mut plan,
                    items,
                    chain,
                    &mut pos,
                    usize::MAX,
                )
                .map(|(out, _)| out);
        }
        self.run_map_grep_chunk(func, *fatal, mode, &mut plan, items, *pos, items.len())
    }

    /// [`Self::pull_map_grep_rest`] for a source that stays in use afterwards
    /// (an `Iterator`'s stream, #10186): advances `pos` to the end, so the
    /// source reports itself exhausted. Returns the same shape as
    /// [`Self::pull_map_grep_prefix`].
    // Cost: one callback call per source element from `pos` on.
    pub(crate) fn pull_map_grep_remaining(
        &mut self,
        source: &mut SeqSource,
    ) -> Result<(Vec<Value>, bool), RuntimeError> {
        let out = self.pull_map_grep_rest(source)?;
        if let SeqSource::MapGrep { items, pos, .. } = source {
            *pos = items.len();
        }
        Ok((out, true))
    }

    /// Pull at least `needed` more elements from a deferred `.map`/`.grep`
    /// `source` (a [`SeqSource::MapGrep`]), advancing its `pos`. Returns the
    /// elements produced (a `Slip` from the callback can overshoot `needed`)
    /// and whether the source is now exhausted — it ran out, or the callback
    /// said `last`.
    ///
    /// Only a callback that binds one element per call and has no `FIRST` /
    /// `LAST` phaser can be run a chunk at a time: a multi-parameter block
    /// consumes the source in groups, and the loops fire those phasers once
    /// per loop run. Anything else, and a map over a shaped array's leaves,
    /// is pulled whole.
    // Cost: one callback call per source element up to the `needed`-th
    // element produced (for `.grep`, up to the `needed`-th match).
    pub(crate) fn pull_map_grep_prefix(
        &mut self,
        source: &mut SeqSource,
        needed: usize,
    ) -> Result<(Vec<Value>, bool), RuntimeError> {
        let SeqSource::MapGrep {
            items,
            pos,
            func,
            fatal,
            mode,
            plan,
        } = source
        else {
            return Ok((Vec::new(), true));
        };
        if !plan.prefix_pullable(|| map_grep_pullable_by_prefix(func.as_ref(), mode)) {
            let out =
                self.run_map_grep_chunk(func, *fatal, mode, plan, items, *pos, items.len())?;
            *pos = items.len();
            return Ok((out, true));
        }
        if let MapGrepItems::Chain(chain) = items {
            let chain = chain.clone();
            return self.pull_map_grep_chain(func, *fatal, mode, plan, items, &chain, pos, needed);
        }
        let mut out = Vec::new();
        while out.len() < needed && *pos < items.len() {
            let end = (*pos + (needed - out.len())).min(items.len());
            let depth = crate::runtime::loop_handler_depth::loop_handler_depth();
            self.async_state.map_grep_last_depth = None;
            let chunk = self.run_map_grep_chunk(func, *fatal, mode, plan, items, *pos, end)?;
            *pos = end;
            out.extend(chunk);
            // The loop the chunk ran in sat one handler level below us; a
            // `last` a loop nested inside the callback caught sits deeper.
            if self.async_state.map_grep_last_depth.take() == Some(depth + 1) {
                return Ok((out, true));
            }
        }
        let exhausted = *pos >= items.len();
        Ok((out, exhausted))
    }

    /// The receiver of a `.map`/`.grep` that is itself a not-yet-run
    /// `.map`/`.grep` Seq (`reify_or_consume_seq_target` leaves those
    /// untouched). With `chain`, steal its source as the new stage's
    /// [`MapGrepItems::Chain`] (#11176), which consumes the receiver as any
    /// `.map` does; without, take it whole so the eager path reads its
    /// elements. `(None, target)` for every other receiver.
    // Cost: O(1), or O(p) for p elements a prefix pull already produced;
    // without `chain`, a full pull of the receiver.
    pub(crate) fn map_grep_receiver_chain(
        &mut self,
        target: Value,
        chain: bool,
    ) -> Result<(Option<MapGrepItems>, Value), RuntimeError> {
        let ValueView::Seq(body) = target.view() else {
            return Ok((None, target));
        };
        if !body.has_map_grep_stream_source() {
            return Ok((None, target));
        }
        if chain && let Some((prefix, source)) = body.take_map_grep_stream_source() {
            let items = MapGrepItems::Chain(crate::value::MapGrepChain::new(prefix, source));
            return Ok((Some(items), target));
        }
        let (items, outcome) = self.take_seq_body(&body)?;
        Ok((
            None,
            if matches!(outcome, crate::value::SeqTaken::Taken) {
                Value::seq(items)
            } else {
                target
            },
        ))
    }

    /// [`Self::pull_map_grep_prefix`] for a `.map`/`.grep` chained onto
    /// another one (`MapGrepItems::Chain`, #11176): pull the upstream one
    /// element at a time and run this callback over each element as it
    /// arrives, so the two callbacks interleave per element as Rakudo's pull
    /// pipeline does. A callback that cannot be run a chunk at a time (see
    /// `pull_map_grep_prefix`) drains the upstream first and runs once.
    // Cost: one upstream pull plus one callback call per upstream element up
    // to the `needed`-th element produced.
    #[allow(clippy::too_many_arguments)]
    fn pull_map_grep_chain(
        &mut self,
        func: &Option<Value>,
        fatal: bool,
        mode: &MapGrepMode,
        plan: &mut MapGrepPlanSlot,
        items: &MapGrepItems,
        chain: &crate::value::MapGrepChain,
        pos: &mut usize,
        needed: usize,
    ) -> Result<(Vec<Value>, bool), RuntimeError> {
        if !plan.prefix_pullable(|| map_grep_pullable_by_prefix(func.as_ref(), mode)) {
            while chain.extend(|source| self.pull_map_grep_prefix(source, usize::MAX))? > 0 {}
            let out = self.run_map_grep_chunk(func, fatal, mode, plan, items, *pos, items.len())?;
            *pos = items.len();
            return Ok((out, true));
        }
        let mut out = Vec::new();
        while out.len() < needed {
            if *pos >= items.len() {
                if chain.exhausted() {
                    break;
                }
                chain.extend(|source| self.pull_map_grep_prefix(source, 1))?;
                continue;
            }
            let end = items.len();
            let depth = crate::runtime::loop_handler_depth::loop_handler_depth();
            self.async_state.map_grep_last_depth = None;
            let chunk = self.run_map_grep_chunk(func, fatal, mode, plan, items, *pos, end)?;
            *pos = end;
            out.extend(chunk);
            if self.async_state.map_grep_last_depth.take() == Some(depth + 1) {
                return Ok((out, true));
            }
        }
        let exhausted = *pos >= items.len() && chain.exhausted();
        Ok((out, exhausted))
    }

    /// Run a deferred `.map`/`.grep` callback over the source elements
    /// `start..end`, with the call site's `use fatal` and the callback's
    /// declaring package in force. `plan` carries what earlier chunks of the
    /// same Seq computed about the callback (`runtime/map_grep_plan.rs`).
    ///
    /// The callbacks run inside the `.map`'s iteration, which is a method
    /// call however late the Seq is forced: a `{*}` in one evaluates to `Nil`
    /// instead of reaching a proto body (#10746).
    // Cost: one callback call per element of `start..end`.
    #[allow(clippy::too_many_arguments)]
    fn run_map_grep_chunk(
        &mut self,
        func: &Option<Value>,
        fatal: bool,
        mode: &MapGrepMode,
        plan: &mut MapGrepPlanSlot,
        items: &MapGrepItems,
        start: usize,
        end: usize,
    ) -> Result<Vec<Value>, RuntimeError> {
        self.in_method_call(|interp| {
            interp.run_map_grep_chunk_body(func, fatal, mode, plan, items, start, end)
        })
    }

    #[allow(clippy::too_many_arguments)]
    fn run_map_grep_chunk_body(
        &mut self,
        func: &Option<Value>,
        fatal: bool,
        mode: &MapGrepMode,
        plan: &mut MapGrepPlanSlot,
        items: &MapGrepItems,
        start: usize,
        end: usize,
    ) -> Result<Vec<Value>, RuntimeError> {
        // Same contract as `force_lazy_list_vm`: this force IS the
        // effective call site for the callbacks it runs, so a
        // captured-outer lexical the callback mutated (`LAST $ran =
        // True`, `$count++`) has to be drained back into the consuming
        // frame's local slots — the reify is not a call op, so nothing
        // else would.
        let caller_code = self.current_code;
        // `use fatal` is lexical to the `.map` call site, not to
        // whoever consumes the Seq — see `SeqSource::MapGrep::fatal`.
        let saved_fatal = std::mem::replace(&mut self.fatal_mode, fatal);
        // Run the callback under its DECLARING package, exactly as
        // `call_compiled_closure_in_unit` does when a Sub value is
        // invoked from a foreign frame. The map loop drives the block
        // through `run_reuse`, which bypasses that guard, and the pull
        // happens wherever the Seq is consumed -- so
        // `class Outer { our sub f(@n) { @n.map({ Inner.new }) } }`
        // consumed from `GLOBAL` could no longer resolve `Inner`
        // (`t/closure-package-nested-class.t`). Eager `map` never hit
        // this because the loop ran inside the declaring routine.
        // Which package that is depends on the callback alone, so a Seq
        // pulled a chunk at a time works it out once (`plan`); only the
        // comparison with the running package is per pull.
        let callback_package = plan.callback_package(|| match func.as_ref().map(Value::view) {
            Some(ValueView::Sub(data))
                if !data.package.as_str().is_empty()
                    && !crate::runtime::utils::has_routine_scope_marker(data.package.as_str()) =>
            {
                Some(data.package)
            }
            _ => None,
        });
        let _pkg_guard = callback_package
            .filter(|&pkg| pkg != self.current_package_sym())
            .map(|pkg| self.enter_package_guarded_sym(pkg));
        let result = match mode {
            // ADR-0058 step 3b: `@a.grep({...})` promotes every matched
            // source slot to a shared element cell and builds its result
            // out of the same cells, so a writeback loop mutates through
            // into `@a`. That whole arm runs here now instead of at the
            // `.grep` call. See `MapGrepMode::GrepArray`.
            MapGrepMode::GrepArray(source) => match source.view() {
                ValueView::Array(source_items, _) => self.grep_over_array_promoting_range(
                    source_items.clone(),
                    func.clone(),
                    &crate::runtime::methods_collection_ops::GrepAdverb::V,
                    start..end,
                    plan,
                ),
                _ => self
                    .eval_grep_over_items_planned(func.clone(), items.slice(start, end), plan)
                    .map(|(result, _, _)| result),
            },
            MapGrepMode::Grep => self
                .eval_grep_over_items_planned(func.clone(), items.slice(start, end), plan)
                .map(|(result, _, _)| result),
            // `@a.map({ $_++ })`: Raku rw-binds `$_` to the source
            // element, so the callback's writes have to reach `@a`.
            // See `MapGrepMode::MapRw`.
            MapGrepMode::MapRw(source) => self.pull_rw_map(
                func.clone(),
                items.slice(start, end),
                source.clone(),
                start,
                plan,
            ),
            MapGrepMode::Map => {
                self.eval_map_over_items_planned(func.clone(), items.slice(start, end), plan)
            }
        };
        self.fatal_mode = saved_fatal;
        self.reconcile_caller_after_lazy_force(caller_code);
        // A `fail` (and `...`, which IS a `fail`) raised by the
        // callback escapes as a `Control::Fail` error, which the next
        // routine boundary would soften into a returned `Failure`.
        // Under the `use fatal` that was lexically in force at the
        // `.map` CALL — most often an enclosing `try`, which implies
        // it — rakudo throws instead, and that boundary is nowhere
        // near here: it is whichever routine encloses the CONSUMER.
        // So decide it here, where the call site's `fatal` is known,
        // by turning the soft failure into a hard throw.
        let result = result.map_err(|mut e| {
            if fatal && e.is_fail() {
                e.control = None;
            }
            e
        })?;
        let items = match result.view() {
            ValueView::Array(items, _) => items.to_vec(),
            _ => crate::runtime::utils::value_to_list(&result),
        };
        Ok(items)
    }

    /// Pull a deferred `.map` whose receiver was a real Array
    /// (`SeqSource::MapGrep::MapRw`): run the rw map loop over `items`, the
    /// source elements from index `start` on, then publish any element the
    /// callback wrote back into that container.
    fn pull_rw_map(
        &mut self,
        func: Option<Value>,
        mut items: Vec<Value>,
        source: Value,
        start: usize,
        plan: &mut MapGrepPlanSlot,
    ) -> Result<Value, RuntimeError> {
        // The narrow native rw loop first: it is the only one that captures a
        // prefix `++$_`/`--$_` or a bare `tr///` (`rw_map_topic_capture`),
        // which the shared loop's `__mutsu_rw_map_topic__` assignment mirror
        // does not see. It declines everything else, including every
        // read-only block, for which it is 4-7.6x slower (see its module doc).
        // Whether the callback's shape can take it at all is a property of
        // the callback alone, so a Seq pulled a chunk at a time asks once:
        // the answer used to cost an AST walk of the body per element.
        let native_candidate = match func.as_ref().map(Value::view) {
            Some(ValueView::Sub(data)) => plan
                .native_rw_candidate(crate::runtime::map_grep_plan::sub_origin(&data), || {
                    super::vm_native_map::native_rw_map_block_shape(&data).is_some()
                }),
            _ => false,
        };
        if native_candidate
            && !self.native_lever_a_user_override_sym(&source, crate::symbol::wk::map())
            && let Some(args) = func.clone().map(|f| vec![f])
            && let Some(native) =
                self.try_native_rw_map_over(&source, &args, start..start + items.len())
        {
            let (result_items, source_after) = native?;
            self.publish_rw_map_writeback(&source, source_after, start);
            return Ok(Value::seq(result_items));
        }
        let (result, wrote_back) = self.eval_map_over_items_rw(func, &mut items, plan)?;
        // A read-only block wrote nothing, so leave the source container
        // ALONE. It used to be rebuilt unconditionally, which silently
        // dropped the per-slot metadata `ArrayData` carries: a `:delete`d
        // slot lost its `initialized` bit, stopped reading as a hole, and a
        // later trailing-element `:delete` could no longer truncate the array
        // (roast/S32-array/delete.t, via a read-only
        // `@a.map({ $_ // "Any()" })` in between).
        if wrote_back {
            self.publish_rw_map_writeback(&source, items, start);
        }
        Ok(result)
    }

    /// Write the mutated elements `items` of a rw `.map` back into the source
    /// container at index `start` on, by mutating its `ArrayData` IN PLACE.
    ///
    /// In place, not by rebuilding and re-binding the name: the pull runs
    /// wherever the Seq is consumed, so the frame whose `env` held `@a` may
    /// be long gone by then and `store_container_preserving_identity` would
    /// have nothing to store into. Writing through the `Gc` (ADR-0013 §7 made
    /// this sound at the primitive) reaches every alias by construction and
    /// does not depend on which frame is running — the same move that made
    /// `grep`'s element promotion frame-independent (ADR-0058 §9.2).
    // Cost: O(c) for a later chunk of a prefix-pulled map, c = elements of
    // the chunk; O(e) otherwise, e = elements of the source container.
    fn publish_rw_map_writeback(&mut self, source: &Value, items: Vec<Value>, start: usize) {
        // A later chunk of a prefix-pulled map (a `for` loop pulls one
        // element per iteration, #9936): write just those slots. Rebuilding
        // the whole container per chunk made the loop quadratic.
        if start > 0 && !crate::runtime::utils::is_shaped_array(source) {
            let ValueView::Array(data, _) = source.view() else {
                return;
            };
            // SAFETY: as below — no other borrow of the `Gc`'s contents is
            // live; the chunk's elements were cloned out before the map ran.
            let slots = unsafe { crate::value::gc_contents_mut(&data) }.items_mut();
            for (i, v) in items.into_iter().enumerate() {
                if let Some(slot) = slots.get_mut(start + i) {
                    *slot = v;
                }
            }
            return;
        }
        // A shaped array keeps its shape/structure — only the leaf values
        // change — so rebuild the rows from the mutated leaves instead of
        // flattening it into an ordinary list, then publish the rows. A
        // shaped source is only ever pulled whole (its `MapGrepItems` is a
        // snapshot of the leaves), so `start` is 0 there.
        let new_items = if crate::runtime::utils::is_shaped_array(source) {
            let rebuilt = crate::runtime::utils::replace_shaped_leaves(source, &items);
            match rebuilt.view() {
                ValueView::Array(rows, _) => rows.to_vec(),
                _ => return,
            }
        } else {
            items
        };
        let ValueView::Array(data, _) = source.view() else {
            return;
        };
        // SAFETY: same contract as `dispatch_grep`'s in-place promotion —
        // a `&mut` to the `Gc`'s contents while no other borrow of it is
        // live (the element vector was cloned out before the map ran).
        let slots = unsafe { crate::value::gc_contents_mut(&data) };
        // The replacement vector is authoritative; an `array[int]` native
        // payload describes the OLD vector and must not decode back over it
        // (the `Value::array_data_like` rebuild this replaces dropped it too
        // -- that helper had no other caller left and is gone).
        slots.clear_native_storage();
        if start == 0 && new_items.len() < slots.len() {
            // The first chunk of a prefix-pulled map covers only a prefix:
            // keep the elements after it.
            let mut all = new_items;
            all.extend(slots.iter().skip(all.len()).cloned());
            *slots.items_mut() = all;
        } else {
            *slots.items_mut() = new_items;
        }
    }
}

/// Whether a deferred `.map`/`.grep` can be run a chunk of its source at a
/// time (see [`Interpreter::pull_map_grep_prefix`]).
// Cost: O(s), s = top-level statements of the callback's body.
fn map_grep_pullable_by_prefix(func: Option<&Value>, mode: &MapGrepMode) -> bool {
    if let MapGrepMode::MapRw(source) = mode
        && crate::runtime::utils::is_shaped_array(source)
    {
        return false;
    }
    let Some(func) = func else {
        return true;
    };
    let ValueView::Sub(data) = func.view() else {
        // A Regex, a type object or another smartmatch target is tested one
        // element at a time and keeps no state between elements.
        return true;
    };
    let positional = data
        .param_defs
        .iter()
        .filter(|pd| !pd.named && !pd.is_invocant)
        .count()
        .max(data.params.len())
        .saturating_sub(data.assumed_positional.len());
    let slurpy = data.param_defs.iter().any(|pd| pd.slurpy);
    let loop_phaser = data.body.iter().any(|stmt| {
        matches!(
            stmt,
            crate::ast::Stmt::Phaser {
                kind: crate::ast::PhaserKind::First | crate::ast::PhaserKind::Last,
                ..
            }
        )
    });
    positional <= 1 && !slurpy && !loop_phaser
}
