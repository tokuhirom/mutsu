use super::*;
use crate::value::{SeqBody, SeqSource, SeqTaken};
use std::sync::Arc;

impl Interpreter {
    /// Pull every element from a not-yet-reified `SeqSource` — the closure
    /// [`SeqBody::reify`]/`take`/`sink` need to actually produce elements
    /// (ADR-0034). Folds together the two flavours of "a Seq that has not
    /// read its elements yet": a user/native `Iterator` (`Seq.new($iter)`,
    /// `Seq.from-loop`) and an `IO::Handle.lines`/`.words` read (formerly the
    /// separate `ValueRepr::LazyIoLines`).
    pub(crate) fn pull_seq_source(
        &mut self,
        source: &SeqSource,
    ) -> Result<Vec<Value>, RuntimeError> {
        match source {
            // `reify`/`take`/`sink` only invoke `pull` for a source that
            // still needs pulling; these two arms exist only so the match is
            // exhaustive and never panics if that invariant ever slips.
            SeqSource::Reified => Ok(Vec::new()),
            SeqSource::Taken => Err(crate::value::seq_consumed_error()),
            SeqSource::Iterator(iterator) => self.pull_iterator_to_vec(iterator.clone()),
            SeqSource::IoLines { handle, words, kv } => {
                self.pull_io_lines_to_vec(handle.clone(), *words, *kv)
            }
        }
    }

    /// Drive a user/native `Iterator`'s `pull-one` until `IterationEnd`.
    fn pull_iterator_to_vec(&mut self, iterator: Value) -> Result<Vec<Value>, RuntimeError> {
        // `Seq.from-loop(&body, :label(...))` with no condition/step
        // (`dispatch_seq_from_loop`'s lazy-infinite branch,
        // `runtime/methods_seq_dispatch.rs`) wraps its body callable in a
        // synthetic `FromLoopIterator` instance instead of a real `Iterator`
        // role — it has no `pull-one` method to call (there is no class body
        // for it), so a genuine consuming touch (`.sink`, `.List`, ...) must
        // drive it the same way the EAGER `Seq.from-loop` loop does, not via
        // the generic `.pull-one` protocol (surfaced by
        // `roast/S04-statements/label.t`'s "nested loop with labeled last
        // (4)": sinking `L7: Seq.from-loop({ loop { last L7 } }, :label(L7))`
        // threw "No such method 'pull-one'" instead of running the body once
        // and stopping on the labeled `last`).
        if let ValueView::Instance { class_name, .. } = iterator.view()
            && class_name == "FromLoopIterator"
        {
            return self.pull_from_loop_iterator_to_vec(&iterator);
        }
        let mut pulled = Vec::new();
        loop {
            let val = self.call_method_with_values(iterator.clone(), "pull-one", vec![])?;
            if matches!(val.view(), ValueView::Str(s) if s.as_str() == "IterationEnd")
                || matches!(val.view(), ValueView::Package(name) if name == crate::symbol::Symbol::intern("IterationEnd"))
            {
                break;
            }
            pulled.push(val);
        }
        Ok(pulled)
    }

    /// Drive a `FromLoopIterator` instance (see `pull_iterator_to_vec`'s doc
    /// comment) by repeatedly invoking its stored `from_loop_body` callable,
    /// mirroring `dispatch_seq_from_loop`'s own eager loop (redo/next/last
    /// with label matching) — this IS the deferred half of that same loop,
    /// just run later, on first consumption, instead of at construction.
    /// Genuinely unbounded if the body never raises a (label-matching)
    /// `last` — same as raku: `.sink`ing an infinite `Seq.from-loop` that
    /// never stops itself hangs there too.
    fn pull_from_loop_iterator_to_vec(
        &mut self,
        iterator: &Value,
    ) -> Result<Vec<Value>, RuntimeError> {
        let ValueView::Instance { attributes, .. } = iterator.view() else {
            return Ok(Vec::new());
        };
        let attrs = attributes.as_map();
        let Some(body_callable) = attrs.get("from_loop_body").cloned() else {
            return Ok(Vec::new());
        };
        let label = match attrs.get("from_loop_label").map(Value::view) {
            Some(ValueView::Str(s)) => Some(s.to_string()),
            _ => None,
        };
        let label_matches = |error_label: &Option<String>| {
            error_label.as_deref() == label.as_deref() || error_label.is_none()
        };
        // Same purpose as `dispatch_seq_from_loop`'s own guard: a loop-control
        // signal raised inside `body_callable` needs somewhere to go instead
        // of surfacing as a bare `X::ControlFlow`.
        let _loop_handler = crate::runtime::loop_handler_depth::LoopHandlerGuard::new();
        let mut items = Vec::new();
        'from_loop: loop {
            'body_redo: loop {
                match self.call_sub_value(body_callable.clone(), vec![], true) {
                    Ok(value) => {
                        if !value.is_nil() {
                            items.push(value);
                        }
                        break 'body_redo;
                    }
                    Err(e) if e.is_redo() && label_matches(&e.label) => continue 'body_redo,
                    Err(e) if e.is_next() && label_matches(&e.label) => break 'body_redo,
                    Err(e) if e.is_last() && label_matches(&e.label) => break 'from_loop,
                    Err(e) => return Err(e),
                }
            }
        }
        Ok(items)
    }

    /// Read every remaining line/word from a file handle (formerly
    /// `force_if_lazy_io_lines`'s body).
    fn pull_io_lines_to_vec(
        &mut self,
        handle: Value,
        words: bool,
        kv: bool,
    ) -> Result<Vec<Value>, RuntimeError> {
        let forced = loan_env!(self, force_lazy_io_lines(&handle, words))?;
        let items = crate::runtime::utils::value_to_list(&forced);
        if kv {
            let mut kv_items = Vec::with_capacity(items.len() * 2);
            for (i, v) in items.iter().enumerate() {
                kv_items.push(Value::int(i as i64));
                kv_items.push(v.clone());
            }
            Ok(kv_items)
        } else {
            Ok(items)
        }
    }

    /// Pull up to `count` additional words/lines from `handle`, reporting
    /// whether EOF was actually reached (see
    /// [`crate::value::SeqBody::pull_io_lines_prefix`]) — the bounded
    /// counterpart of `pull_io_lines_to_vec`'s full drain, used by an indexed
    /// read on a not-yet-reified `IO::Handle.lines`/`.words` Seq
    /// (`vm_var_index_ops.rs`) so a partial slice (`words($fh, :close)[1,
    /// 2]`) does not trigger `:close`'s close-on-exhaust.
    fn pull_io_lines_prefix_to_vec(
        &mut self,
        handle: &Value,
        words: bool,
        count: usize,
    ) -> Result<(Vec<Value>, bool), RuntimeError> {
        let mut pulled = Vec::with_capacity(count);
        let mut exhausted = false;
        for _ in 0..count {
            let next = if words {
                self.read_word_from_handle_value(handle)?
            } else {
                loan_env!(self, read_line_from_handle_value(handle))?
            };
            match next {
                Some(s) => pulled.push(Value::str(s)),
                None => {
                    exhausted = true;
                    break;
                }
            }
        }
        Ok((pulled, exhausted))
    }

    /// Indexed-read special case (ADR-0034, `vm_var_index_ops.rs`): reify
    /// only enough of `body` to serve a subscript up to `needed` — a full
    /// [`Self::reify_seq_body`] for every source EXCEPT a not-yet-exhausted
    /// `IoLines` one, which reads just the missing prefix so `:close`'s
    /// close-on-exhaust only fires when the read genuinely reaches EOF.
    pub(crate) fn reify_seq_body_prefix(
        &mut self,
        body: &Arc<SeqBody>,
        needed: usize,
    ) -> Result<(), RuntimeError> {
        if !body.is_io_lines_source() {
            self.reify_seq_body(body)?;
            return Ok(());
        }
        body.pull_io_lines_prefix(needed, |handle, words, count| {
            self.pull_io_lines_prefix_to_vec(handle, words, count)
        })?;
        // A partial pull that DID hit EOF (exhausted) already left `body` in
        // `SeqSource::Reified`, matching what a full `reify` would produce —
        // no further action needed either way.
        Ok(())
    }

    /// rakudo's `.cache`, and every other non-consuming touch: pull `body`'s
    /// source exactly once (idempotent — a no-op if already reified) and
    /// return the (now-retained) elements.
    pub(crate) fn reify_seq_body(
        &mut self,
        body: &Arc<SeqBody>,
    ) -> Result<Vec<Value>, RuntimeError> {
        Ok(body.reify(|source| self.pull_seq_source(source))?.clone())
    }

    /// rakudo's `.iterator`/`.list`/...: produce the elements, consuming the
    /// source UNLESS the body is already reified or `.cache` was requested.
    pub(crate) fn take_seq_body(
        &mut self,
        body: &Arc<SeqBody>,
    ) -> Result<(Vec<Value>, SeqTaken), RuntimeError> {
        body.take(|source| self.pull_seq_source(source))
    }

    /// rakudo's `sink`: run the source for side effects and discard.
    pub(crate) fn sink_seq_body(&mut self, body: &Arc<SeqBody>) -> Result<(), RuntimeError> {
        body.sink(|source| self.pull_seq_source(source))
    }

    /// Pre-dispatch guard (ADR-0034 §2.3): if `target` is a `Seq` whose body
    /// needs touching before `method` can run — a deferred source
    /// (`Seq.new($iterator)`, `IO::Handle.lines` — formerly the separate
    /// `force_if_lazy_io_lines`/`LazyIoLines` special case) or a body already
    /// taken by an earlier consuming method — reify or consume it as
    /// `method` requires, and return the value dispatch should actually
    /// operate on. Reification fills the SAME body in place, so the common
    /// non-consuming case returns `target` unchanged; a genuinely consuming
    /// method returns a fresh, already-reified `Seq` built from what it just
    /// pulled (the original `target` is left `Taken`). Passes every non-Seq
    /// value, and an already-reified Seq, straight through (`needs_touch` is
    /// a cheap state check, no clone).
    ///
    /// **`.iterator` is deliberately handled by this call, not by the
    /// authoritative one below** — see
    /// [`Self::reify_or_consume_seq_target_authoritative`]'s doc comment for
    /// why `.iterator` needs its own, narrower entry point.
    pub(crate) fn reify_or_consume_seq_target(
        &mut self,
        target: Value,
        method: &str,
    ) -> Result<Value, RuntimeError> {
        self.reify_or_consume_seq_target_inner(target, method, false)
    }

    /// The ONE call site allowed to actually consume `.iterator` — see
    /// [`Self::reify_or_consume_seq_target_inner`]'s `.iterator` arm for the
    /// full reasoning. Only `methods_call_dispatch::call_method_with_values`
    /// calls this; every other `reify_or_consume_seq_target` call site
    /// (`vm_call_method_ops.rs`, `vm_call_method_mut_ops.rs` ×2,
    /// `vm_call_method_compiled_interpret.rs`, `vm_for_loop_dispatch.rs`) is
    /// an OUTER pre-check layer that the VM's own fallback chain may run
    /// MORE THAN ONCE for a single logical method call before reaching the
    /// true dispatch — verified with `rust-gdb` for both the non-mut and the
    /// mut (named-variable-receiver) call chains. `call_method_with_values`
    /// is the one layer every chain funnels through exactly once no matter
    /// which of those outer layers it came from (it has no native `.iterator`
    /// row of its own to short-circuit on — see
    /// `builtins/methods_0arg/mod.rs`'s "Do NOT pre-check... iterator"
    /// comment), so it is the correct place for the one-time,
    /// side-effecting steal to live.
    pub(crate) fn reify_or_consume_seq_target_authoritative(
        &mut self,
        target: Value,
        method: &str,
    ) -> Result<Value, RuntimeError> {
        self.reify_or_consume_seq_target_inner(target, method, true)
    }

    fn reify_or_consume_seq_target_inner(
        &mut self,
        target: Value,
        method: &str,
        handle_iterator: bool,
    ) -> Result<Value, RuntimeError> {
        let ValueView::Seq(body) = target.view() else {
            return Ok(target);
        };
        if !body.needs_touch() || crate::value::seq_method_never_touches(method) {
            return Ok(target);
        }
        let body = Arc::clone(&body);
        // `Seq.new($predictiveIterator)` (`try_native_seq_construct`) builds
        // an EMPTY already-`Reified` body and tracks its iterator out of
        // band, in `self.predictive_seq_iters` keyed by this body's own
        // `Arc` address — so `.tail`/`.Numeric` can use the count-only path
        // instead of eagerly draining. A `seq_method_consumes` touch (e.g.
        // `.tail` is one) must NOT steal this placeholder: `take` returning
        // `SeqTaken::Taken` would rebuild the result as a FRESH `Value::seq`
        // with a NEW `Arc` address, permanently orphaning the
        // `predictive_seq_iters` association (the lookup key no longer
        // matches anything). Leave it completely untouched here; the
        // specific dispatch handlers (`dispatch_tail`, `.Numeric`) do their
        // own `predictive_seq_iter_for` lookup against the UNCHANGED body.
        if body.is_empty()
            && self
                .predictive_seq_iter_for(Arc::as_ptr(&body) as usize)
                .is_some()
        {
            return Ok(target);
        }
        if method == "sink" {
            self.sink_seq_body(&body)?;
            return Ok(target);
        }
        if method == "cache" {
            return Ok(target);
        }
        // `.kv` on a not-yet-touched `IO::Handle.lines`/`.words` Seq still
        // CONSUMES the original (measured against raku: `$s.kv; $s.List`
        // throws `X::Seq::Consumed`, matching every other `seq_method_consumes`
        // entry — this is not an exemption), but must not eagerly PULL
        // anything: the returned replacement is a NEW deferred Seq over the
        // SAME handle with the `IoLines` source's own `kv` flag flipped on,
        // so a following `for $fh.lines.kv -> \k, \v {}` still finds a
        // claimable `IoLines` source on THAT value and streams one line at a
        // time (`vm_for_loop_dispatch.rs`'s `claim_io_lines_for_streaming`,
        // keyed on `$fh.tell` reflecting the current read position —
        // `roast/S16-filehandles/io_in_for_loops.t`). `claim_io_lines_for_streaming`
        // (not the non-destructive peek) is what marks the ORIGINAL body
        // `Taken` here, so `$s.kv` outside a `for`-loop still consumes `$s`
        // correctly (`t/seq-consumption-matrix.t`) even though the value it
        // returns stays lazy. Every other shape (already reified, already
        // taken, cache-requested, or a non-IoLines deferred source) falls
        // through to the ordinary handling below.
        if method == "kv"
            && let Some((handle, words, _kv)) = body.claim_io_lines_for_streaming()?
        {
            return Ok(Value::seq_deferred(crate::value::SeqSource::IoLines {
                handle,
                words,
                kv: true,
            }));
        }
        if matches!(method, "raku" | "perl") {
            // Pulls if the source is still available (needed to render real
            // elements); an already-`Taken` source must NOT throw here — the
            // renderer shows the `Seq.new()` placeholder instead (verified
            // against raku: `.raku` on an already-consumed Seq does not
            // throw, unlike `.Str`/`.gist`).
            let _ = self.reify_seq_body(&body);
            return Ok(target);
        }
        if method == "list" {
            // `"list"` is a genuine ambiguity, not covered by the
            // `seq_method_consumes` table: mutsu's parser desugars the
            // sigil array-context deref `@$s` to the SAME method-name
            // string as an explicit `.list()` call
            // (`src/parser/primary/var/sigil_vars.rs`), but raku treats
            // them differently — `@$s; @$s;` never throws (even over a
            // genuinely deferred `IO::Handle.lines` source), while
            // `$s.list; $s.list;` throws `X::Seq::Consumed` on the second
            // call (both measured directly against raku). Two pinned local
            // tests independently exercise each side and cannot both be
            // satisfied by one policy without the parser telling the calls
            // apart: `t/seq-array-context-reiterate.t` (`@$s` on `.map`/
            // `.grep` results — born `Reified` — must stay re-readable) and
            // `t/io-handle-lines-words-seq.t` (explicit `.list` on
            // `IO::Handle.lines`/`.words` — a genuinely deferred source —
            // must consume). Compromise until the parser can distinguish
            // them: steal a genuinely deferred source (satisfies the
            // `IO::Handle.lines` test, which never touches an already-
            // `Reified` body) but never steal a body already `Reified` at
            // this touch (satisfies the `@$s`-on-`.map`/`.grep` test). A
            // bare `@$s` on a deferred source is thus the one case left
            // over-strict relative to raku (not pinned by any local test).
            if body.has_deferred_source() {
                let (items, outcome) = self.take_seq_body(&body)?;
                return Ok(if matches!(outcome, SeqTaken::Taken) {
                    Value::seq(items)
                } else {
                    target
                });
            }
            self.reify_seq_body(&body)?;
            return Ok(target);
        }
        if method == "iterator" && !handle_iterator {
            // An OUTER pre-check layer (see
            // `reify_or_consume_seq_target_authoritative`'s doc comment):
            // `.iterator` must NOT be consumed here. Pass `target` through
            // completely untouched — not even a non-destructive peek — and
            // let the call chain's single authoritative layer
            // (`call_method_with_values`) do the real work exactly once.
            // Touching it here too (even non-destructively) would race two
            // different notions of "first touch" between this layer and the
            // authoritative one for no benefit, since this layer's own
            // result is discarded once the chain reaches the real one.
            return Ok(target);
        }
        if method == "iterator" {
            // `.iterator` builds its FINAL result (an `Iterator` instance)
            // here, in place of returning a still-`ValueView::Seq` target
            // for a LATER dispatch layer to call `.iterator` on again. Two
            // reasons, both required together:
            //
            // 1. **Redundant dispatch layers.** The VM's own call chain
            //    invokes `reify_or_consume_seq_target` through more than one
            //    fallback layer for a SINGLE logical `.iterator()` call
            //    (`vm_call_method_ops.rs`'s outer native-dispatch guard,
            //    then `vm_call_method_compiled_interpret.rs`'s inner one,
            //    when no native row exists — verified with `rust-gdb`).
            //    Returning a still-`Seq` target from the outer call lets the
            //    inner layer's OWN `reify_or_consume_seq_target` call see the
            //    body already `Taken` by the FIRST call and throw
            //    `X::Seq::Consumed` on what is, from the user's code, the
            //    FIRST and only `.iterator()` call. Building the final
            //    Instance here means the redundant inner call sees a
            //    non-`Seq` `target` and no-ops (this function's very first
            //    line), so it is naturally idempotent no matter how many
            //    layers re-run it. This is why the outer layers must not
            //    even PEEK at `.iterator` (the arm just above this one) —
            //    only the ONE authoritative call
            //    (`reify_or_consume_seq_target_authoritative`, `handle_iterator
            //    == true`) reaches here.
            // 2. **Arc identity for side-table lookups.** `take_seq_body`'s
            //    `Taken` outcome would otherwise get wrapped in a FRESH
            //    `Value::seq(items)` (a new Arc) by the generic branch
            //    below. `dispatch_iterator_method` looks up
            //    `squish_iterator_meta` (a lazy-replay `.squish(:as, :with)`
            //    iterator) keyed by the body's ORIGINAL `Arc::as_ptr`
            //    address — a fresh Arc orphans that lookup, and `.iterator`
            //    silently falls back to a plain, already-fully-drained array
            //    iterator (surfaced by `roast/S32-list/squish.t`'s
            //    ":as + :with, iterator use" subtest: every callback fired
            //    upfront instead of one per `.pull-one`).
            //
            // `take_seq_body` still runs first (unchanged): it fills `gens`
            // for a genuinely deferred source, or marks an already-`Reified`
            // body `Taken` in place — `dispatch_iterator_method` then reads
            // the SAME body (identity preserved either way) to build the
            // instance, so its `squish_iterator_meta`/`Deref`-based fallback
            // both see the correct, final data.
            self.take_seq_body(&body)?;
            return self.dispatch_iterator_method(target);
        }
        if crate::value::seq_method_consumes(method) {
            let (items, outcome) = self.take_seq_body(&body)?;
            return Ok(if matches!(outcome, SeqTaken::Taken) {
                Value::seq(items)
            } else {
                target
            });
        }
        self.reify_seq_body(&body)?;
        Ok(target)
    }

    pub(super) fn thread_right_first(
        left: &crate::value::JunctionKind,
        right: &crate::value::JunctionKind,
    ) -> bool {
        use crate::value::JunctionKind::{All, Any, None, One};
        matches!(left, Any | One) && matches!(right, All | None)
    }

    pub(super) fn label_matches(error_label: &Option<String>, loop_label: &Option<String>) -> bool {
        error_label.as_deref() == loop_label.as_deref() || error_label.is_none()
    }

    /// Check if a method on LazyList requires forcing the list first.
    /// Methods that coerce a lazy `Seq` to another lazy view and so must
    /// preserve the laziness of a map/grep pipeline (return it unchanged)
    /// instead of forcing it. `.eager`/`.elems`/`.sort`/… are NOT here — those
    /// genuinely need the whole list and correctly raise X::Cannot::Lazy.
    pub(crate) fn lazy_pipe_preserving_coercion(method: &str) -> bool {
        matches!(
            method,
            "Seq" | "List" | "list" | "Array" | "cache" | "values" | "lazy"
        )
    }

    /// If the variable `name` holds a lazy `@`-array backed by a cache-bearing
    /// spec (infinite sequence/closure/scan), reify enough of its prefix into a
    /// temporary real Array and write it back, so a subsequent element mutation
    /// (`@a[i] = v`, `:delete`) can run the existing (LazyList-unaware)
    /// element-assign/delete machinery unchanged. `touched_index`, when the
    /// caller can cheaply determine it (a plain non-negative Int subscript on
    /// top of the stack), bounds the reify to exactly the elements that
    /// mutation needs -- matching raku, which reifies only up to the touched
    /// index and keeps the rest of an infinite source live. Without a concrete
    /// index (a slice assign, a WhateverCode delete index, ...) this falls back
    /// to the historical capped prefix so a genuinely infinite source cannot
    /// hang or blow memory.
    ///
    /// Returns the original `LazyList`'s `Gc` handle when it reified one, so
    /// the caller can hand it to [`Self::restore_lazy_array_slot`] afterwards
    /// and rebuild a still-lazy value around the mutated prefix and the SAME
    /// live source -- an element write/delete on a lazy array must not
    /// collapse it into a finite Array (raku: `@a.is-lazy` stays `True`,
    /// `@a.elems` keeps throwing `X::Cannot::Lazy`, and a later out-of-range
    /// read keeps pulling from the live source). (L2, bounded reify follow-up)
    pub(super) fn reify_lazy_array_slot(
        &mut self,
        name: &str,
        touched_index: Option<i64>,
    ) -> Result<Option<crate::gc::Gc<LazyList>>, RuntimeError> {
        let lazy = match self.env().get(name).map(Value::view) {
            // Any `@`-array-context lazy list must materialize before an element
            // mutation, not just the cache-backed infinite specs: a finite
            // `(1..10).lazy` / `lazy gather {...}` is also a `LazyList` value with
            // no backing Array, so a bare elem-assign would autovivify a fresh
            // empty Array (losing the elements) instead of writing through.
            Some(ValueView::LazyList(ll)) if ll.in_array_context() => Some(ll.clone()),
            _ => None,
        };
        let Some(ll) = lazy else {
            return Ok(None);
        };
        // A `.lazy`-on-a-finite-list is a pure cache-only `LazyList` (no source
        // that can extend it). Its cache IS the complete data, so use it
        // directly: `force_lazy_list_vm_n(cap)` would return an empty prefix
        // (its cache-hit path needs `cache.len() >= needed`, which a short
        // finite cache never satisfies) and drop the elements.
        let has_extendable_source = ll.sequence_spec.is_some()
            || ll.closure_seq.is_some()
            || ll.scan_spec.is_some()
            || ll.lazy_pipe.is_some()
            || ll.coroutine.is_some()
            || ll.walk_pending.is_some()
            || ll.cat_pull.is_some()
            || ll.compiled_code.is_some();
        let items = if has_extendable_source {
            const MAX_ARRAY_EXPAND: usize = 100_000;
            let needed = match touched_index {
                Some(i) if i >= 0 => (i as usize).saturating_add(1),
                _ => MAX_ARRAY_EXPAND,
            };
            self.force_lazy_list_vm_n(&ll, needed)?
        } else {
            ll.cache.lock().unwrap().clone().unwrap_or_default()
        };
        self.env_mut()
            .insert(name.to_string(), Value::real_array(items));
        Ok(Some(ll))
    }

    /// Counterpart to [`Self::reify_lazy_array_slot`]: once the generic
    /// element-assign/delete machinery has finished mutating the temporary
    /// real Array that function installed, rebuild a still-lazy `LazyList`
    /// around the mutated prefix and `source` (the SAME live sequence
    /// spec/closure/gather coroutine/... the array was reified from), and
    /// write it back over the temporary Array. No-op if the slot no longer
    /// holds a plain Array (e.g. the mutation deleted the variable, or wrapped
    /// it in a container the reify step did not anticipate) -- in that rare
    /// case the array simply stays the finite prefix, no worse than before
    /// this bounded-reify change.
    ///
    /// Also refreshes the caller's local slot (`code`/`slot`), not just env:
    /// the element-assign/delete machinery this wraps writes its result
    /// through the dual store's local-slot fast paths, so leaving the slot
    /// holding the stale temporary Array would let a later per-statement
    /// `locals`->`env` reconcile clobber the LazyList this function just
    /// installed (surfaced by a `@a.is-lazy`/`.gist` touch between the
    /// mutation and a later out-of-range read silently losing laziness).
    pub(super) fn restore_lazy_array_slot(
        &mut self,
        code: &CompiledCode,
        name: &str,
        source: crate::gc::Gc<LazyList>,
    ) {
        let Some(current) = self.env().get(name).cloned() else {
            return;
        };
        let ValueView::Array(items, _) = current.view() else {
            return;
        };
        // A plain Array applies its `is default(...)` value to a hole
        // transparently at READ time, via the embedded `ArrayData::default`
        // field (`typed_container_default`) -- `LazyList` has no equivalent
        // field, and the general lazy-array index-read path
        // (`vm_var_index_ops.rs`) rebuilds a bare `Vec<Value>` from `cache`
        // with no default metadata at all. So bake the default into a hole
        // HERE, once, while the mutated prefix is still a real Array that
        // knows its own default. Only a genuine hole (untracked by the
        // array's `initialized` set) is substituted -- an explicit
        // `@a[i] = Any` write stays `Any`, matching raku
        // (`my @a is default(99) = 1,2,3; @a[1] = Any; @a[1]` is `Any`, not
        // `99`, while `@a[1]:delete; @a[1]` IS `99`).
        let initialized = items.initialized.clone();
        // `typed_container_default` only reads the embedded `container_default`
        // (plus type metadata); fall back to the name-keyed `var_default`
        // table too, matching `exec_delete_index_named_op_inner`'s own
        // `saved_default` computation -- a container that arrived here
        // without ever having its embedded default re-tagged (e.g. an
        // `is default(...)` declared before the array was ever reified)
        // must still find it by name.
        let default = self
            .container_default(&current)
            .or_else(|| self.var_default(name).cloned())
            .unwrap_or_else(|| self.typed_container_default(&current));
        let items: Vec<Value> = items
            .iter()
            .enumerate()
            .map(|(i, v)| {
                let is_hole = match v.view() {
                    ValueView::Nil => true,
                    ValueView::Package(_) => !initialized.as_ref().is_some_and(|s| s.contains(&i)),
                    _ => false,
                };
                if is_hole { default.clone() } else { v.clone() }
            })
            .collect();
        {
            let mut cache = source.cache.lock().unwrap();
            let cached = cache.get_or_insert_with(Vec::new);
            // Overwrite only the mutated prefix (`items`). A longer tail the
            // cache already held -- from an earlier out-of-range read, or an
            // earlier `@a[j] = v` override at some `j` beyond this prefix --
            // must survive untouched; replacing the whole cache would both
            // discard that already-pulled tail (forcing a wasted re-pull)
            // and silently revert any such earlier override.
            if cached.len() < items.len() {
                cached.resize(items.len(), Value::NIL);
            }
            for (i, v) in items.iter().enumerate() {
                cached[i] = v.clone();
            }
        }
        let restored = Value::lazy_list(source);
        self.env_mut().insert(name.to_string(), restored.clone());
        self.locals_set_by_name(code, name, restored);
    }

    pub(super) fn lazy_list_needs_forcing(method: &str) -> bool {
        matches!(
            method,
            "list"
                | "Array"
                | "Numeric"
                | "Int"
                | "elems"
                | "end"
                | "hyper"
                | "race"
                | "first"
                | "grep"
                | "map"
                | "sort"
                | "reverse"
                | "join"
                | "head"
                | "tail"
                | "min"
                | "max"
                | "minmax"
                | "sum"
                | "flat"
                | "unique"
                | "repeated"
                | "squish"
                | "classify"
                | "categorize"
                | "produce"
                | "rotor"
                | "batch"
                | "reduce"
                | "Supply"
                | "combinations"
                | "permutations"
                | "values"
                | "List"
                | "Str"
                | "Stringy"
                | "gist"
                | "raku"
                | "perl"
                | "Seq"
                | "item"
                | "cache"
                | "pick"
                | "roll"
                | "keys"
                | "kv"
                | "pairs"
                | "antipairs"
        )
    }

    /// For `.head` on a gather-sourced `LazyList`, return how many leading
    /// elements need to be produced (so an infinite gather is pulled lazily via
    /// [`Self::force_lazy_list_vm_n`] instead of forced to completion).
    /// Returns `None` for forms that need the whole list (e.g. `.head(*-3)`).
    pub(super) fn gather_head_bound(method: &str, args: &[Value]) -> Option<usize> {
        if method != "head" {
            return None;
        }
        match args {
            [] => Some(1),
            [v] => match v.view() {
                ValueView::Int(n) => Some(n.max(0) as usize),
                ValueView::Num(f) => Some((f as i64).max(0) as usize),
                _ => None,
            },
            _ => None,
        }
    }

    /// Force a LazyList by running its compiled bytecode in the Interpreter.
    /// Falls back to interpreter if no compiled code is available.
    pub(crate) fn force_lazy_list_vm(
        &mut self,
        list: &LazyList,
    ) -> Result<Vec<Value>, RuntimeError> {
        // GC safepoint (§9.2a `lazy_force`): the strict-force entry boundary.
        crate::gc::gc_safepoint(crate::gc::SafepointKind::LazyForce);
        let caller_code = self.current_code;
        // The body runs under its OWN readonly context, not the consumer
        // frame's (see take_readonly_state).
        let saved_readonly = self.take_readonly_state();
        // A `samewith` in the body means the routine the gather was WRITTEN in,
        // not whichever routine is forcing it now — re-push the context the
        // gather captured at creation.
        let pushed_samewith = self.push_captured_samewith_context(&list.env);
        let mut r = self.force_lazy_list_vm_inner(list);
        // A `return` inside the gather body (`gather { ...; return }`) is
        // lexically inside whatever routine WROTE the gather, and its target
        // must be resolved from THAT env — not left untargeted — the exact
        // same rule `call_compiled_closure_with_topic` applies to an
        // ordinary non-routine closure's own captured `return`
        // (`vm_closure_dispatch.rs`). Without this, forcing a gather whose
        // routine has already exited produced an UNTARGETED `CX::Return`,
        // which the first enclosing routine call frame unconditionally
        // "catches" as if it were ITS OWN return (raku: the return targets
        // the routine that wrote the gather, not whoever is forcing it) —
        // silently truncating that caller instead of surfacing
        // `X::ControlFlow::Return` to the nearest real `CATCH`.
        if let Err(e) = &mut r
            && e.is_return()
            && e.return_target_callable_id().is_none()
            && let Some(ValueView::Int(id)) = list.env.get("__mutsu_callable_id").map(Value::view)
        {
            e.set_return_target_callable_id(Some(id as u64));
        }
        self.pop_captured_samewith_context(pushed_samewith);
        self.restore_readonly_state(saved_readonly);
        self.reconcile_caller_after_lazy_force(caller_code);
        // An array-context lazy list IS the array's element store: a Nil the
        // pipe produced resets its fresh element container to Any, like any
        // other store into an untyped array element (ADR-0049 slice 3:
        // `decay_nil_vec_elements`, sharing the same store-time decay
        // authority as every other construction/assignment site instead of
        // hardcoding `Any` here independently).
        if list.in_array_context() {
            return match r {
                Ok(items) => Ok(self.decay_nil_vec_elements(items)),
                err => err,
            };
        }
        r
    }

    /// If `val` is a lazy `.map`/`.grep` pipe whose source chain bottoms out in
    /// a provably-finite source (a `gather`, or a finite Array/Seq/Range), force
    /// it to a reified `Seq` and return that; otherwise return `val` unchanged.
    ///
    /// Used when `map`/`grep` collects a callback result that is itself such a
    /// pipe as an element of the result array. Those elements would otherwise
    /// reach a container whose *static* readers (`flat_val`/`value_to_list`,
    /// used by `.flat`/`for`) cannot run the VM to force a pipe, so they pull the
    /// still-empty pipe cache and yield `()`. Reifying here keeps the element as
    /// a single `Seq` (raku `(1,).map({(10,20)})` == `((10 20),)`, never
    /// flattened). An infinite pipe (`(1,).map({1..Inf})`) bottoms out `false`
    /// and stays lazy, so this can never turn an infinite pipe into a hang.
    pub(crate) fn reify_finite_pipe_value(&mut self, val: Value) -> Result<Value, RuntimeError> {
        if let ValueView::LazyList(ll) = val.view()
            && ll.lazy_pipe.is_some()
            && ll.pipe_bottoms_out_finite()
        {
            let items = self.force_lazy_list_vm(&ll)?;
            return Ok(Value::seq(items));
        }
        Ok(val)
    }

    fn force_lazy_list_vm_inner(&mut self, list: &LazyList) -> Result<Vec<Value>, RuntimeError> {
        // A lazy `WALK(method)()` list is finite (one element per MRO-level
        // candidate): force them all by invoking every remaining candidate.
        if let Some(ref wp) = list.walk_pending {
            let total = wp.lock().unwrap().targets.len();
            return self.force_walk_pending(list, total);
        }
        // Handle scan-based lazy lists: compute elements on demand
        if list.scan_spec.is_some() {
            return self.force_scan_lazy_list(list, 200_000);
        }

        // A lazy map/grep pipeline is rooted at an infinite source. It can still
        // be forced when the callback runs `last` (which ends the sequence) —
        // e.g. `(^Inf).grep({ last if $_ > 5; True }).eager`. So attempt a
        // bounded force: if the pipe terminates within the cap (via `last` or a
        // finite source), return it; otherwise it is genuinely infinite and we
        // throw X::Cannot::Lazy, matching raku. Methods that know their own name
        // (e.g. `.elems`/`.sort`) raise a more specific message at the dispatch
        // site before reaching here.
        if list.lazy_pipe.is_some() {
            const EAGER_FORCE_CAP: usize = 1_000_000;
            let forced = self.force_lazy_pipe(list, EAGER_FORCE_CAP)?;
            let done = list
                .lazy_pipe
                .as_ref()
                .map(|p| p.lock().unwrap().done)
                .unwrap_or(true);
            if done {
                return Ok(forced);
            }
            return Err(RuntimeError::typed_msg(
                "X::Cannot::Lazy",
                "Cannot coerce an infinite lazy list to a strict list",
            ));
        }

        // For sequence-spec lazy lists, a strict force cannot materialize the
        // infinite tail, so return a bounded prefix (the historical 100k cap).
        // With the L2b `[start]` seed the cache is O(1), so extend it to the cap
        // here — every strict-force caller (front mutation reify, eager coerce,
        // ...) expects the prefix, not the seed. Lazy *read* paths (index, head,
        // first, map/grep pipes) use `force_lazy_list_vm_n` / pull and stay O(1).
        if let Some(ref spec) = list.sequence_spec {
            const MAX_ARRAY_EXPAND: usize = 100_000;
            return Self::extend_sequence_cache(list, spec, MAX_ARRAY_EXPAND);
        }

        // A lazy `IO::CatHandle.lines` / `.handles` list is finite (it reads to
        // the end of the cat's handles): force it fully by pulling until the cat
        // is exhausted.
        if list.cat_pull.is_some() {
            return self.force_cat_pull(list, usize::MAX);
        }

        // A closure sequence with a concrete endpoint is finite, even though
        // we deferred its tail to avoid speculating about recurrence.  Strict
        // consumers must drive it to that endpoint; unbounded closure
        // sequences are rejected by their callers before reaching this path.
        if list
            .closure_seq
            .as_ref()
            .is_some_and(|state| state.lock().unwrap().endpoint.is_some())
        {
            return self.extend_closure_sequence(list, usize::MAX);
        }

        // Check cache first
        if let Some(cached) = list.cache.lock().unwrap().clone() {
            return Ok(cached);
        }

        // If no compiled code, fall back to interpreter
        let (cc, fns) = match (&list.compiled_code, &list.compiled_fns) {
            (Some(cc), Some(fns)) => (cc.clone(), fns.clone()),
            _ => return self.force_lazy_list_bridge(list),
        };

        // RAII (`MarkContextGuard`,
        // `todo/deep/mark-context-flags-leak-across-live-call-boundary.md`):
        // this inline exec is another call boundary that runs a callee's
        // compiled body without going through `vm_run_loop.rs`'s nested-run
        // save/restore -- isolate the "mark context" one-shot flag family so
        // a caller's pending `:=` mark (e.g. `@x := $lazy-list.eager`) does
        // not leak into the gather body's own vardecl/store opcodes.
        let _mark_context_guard = crate::vm::vm_call_state_guard::MarkContextGuard::new(self);
        // Save current Interpreter state. Locals are kept coherent with env by
        // write-through (`flush_local_to_env`), so no explicit flush is needed
        // here; we restore locals directly on return.
        crate::vm::vm_stats::record_clone_env();
        let saved_env = self.clone_env();
        let saved_locals = std::mem::take(&mut self.locals);
        let saved_stack = std::mem::take(&mut self.stack);
        // `LazyList` has no upvalue array of its own (its captures live in
        // `list.env`, installed as the scoped env below) -- this inline exec
        // bypasses closure dispatch, so without resetting `self.upvalues` a
        // `GetUpvalue` in the gather body would index whatever array the
        // ENCLOSING frame installed, silently reading an unrelated capture
        // on an index collision. Empty is always safe here: `exec_get_upvalue_op`
        // falls back to a by-name env read on any out-of-range index, which is
        // correct since the gather body's captures are reachable through the
        // scoped env installed just below. See
        // todo/tickets/inline-closure-exec-sites-skip-upvalue-array-install.md.
        let saved_upvalues = std::mem::take(&mut self.upvalues);

        // Each `gather` expression evaluation creates a fresh block clone, so
        // its `state` variables need the coroutine's per-instance scope even
        // when a finite gather is forced to completion in this strict path.
        // The bounded pull/resume sibling installs the same scope around every
        // coroutine run; without it, strict `my @a = gather { state ... }`
        // forces use the raw, compile-position-only key and sibling gathers
        // share a state cell.
        let saved_state_scope = self.state_scope_id.get();
        let gather_scope_id = list
            .coroutine
            .as_ref()
            .map(|m| m.lock().unwrap().state_scope_id)
            .unwrap_or(0);
        if gather_scope_id != 0 {
            self.state_scope_id.set(Some(gather_scope_id));
        }

        // Set up the lazy list's environment as a scoped overlay's parent: the
        // gather body reads its captured lexicals through to `list.env` and its
        // own writes land in a fresh born-owned overlay (no fork of `list.env`).
        // The merge below iterates the overlay (overlay-only) = the body's writes.
        // See docs/vm-dual-store.md (Slice 6).
        *self.env_mut() = crate::env::Env::scoped_child(list.env.flattened());

        // Push gather items collector
        let saved_gather_len = self.gather_items_len();
        self.push_gather_items(Vec::new());
        self.push_gather_take_limit(None);

        // Initialize locals for the compiled code
        self.locals = vec![Value::NIL; cc.locals.len()];
        for (i, name) in cc.locals.iter().enumerate() {
            if let Some(val) = self.env().get(name) {
                self.locals[i] = val.clone();
            }
        }
        self.stack = Vec::new();

        // Run the compiled code using the lazy list's own compiled_fns.
        // Outer scope subs are available via the env as Sub values.
        let run_fns = fns.as_ref();

        let mut ip = 0;
        let mut run_result = Ok(());
        while ip < cc.ops.len() {
            match self.exec_one(&cc, &mut ip, run_fns) {
                Ok(()) => {}
                Err(e) if e.is_warn() => {
                    if !self.warning_suppressed() {
                        self.write_warn_to_stderr(&e.message);
                    }
                    if let Some(v) = e.return_value {
                        self.stack.push(v);
                    }
                    ip += 1;
                    continue;
                }
                Err(e) => {
                    run_result = Err(e);
                    break;
                }
            }
            if self.is_halted() {
                break;
            }
        }

        // Collect gather items
        let items = self.pop_gather_items().unwrap_or_default();
        self.pop_gather_take_limit();

        // Clean up extra gather items if needed
        while self.gather_items_len() > saved_gather_len {
            self.pop_gather_items();
            self.pop_gather_take_limit();
        }

        // Sync locals back to env before reading the result environment.
        // During Interpreter execution, variable assignments go to self.locals, not
        // to the interpreter env. We must flush them so the merge logic below
        // can see the changes made by the gather body.
        for (i, name) in cc.locals.iter().enumerate() {
            {
                let __v = self.locals[i].clone();
                self.env_mut().insert(name.clone(), __v);
            }
        }

        // Restore the outer environment, selectively merging changes from
        // the gather body. Only propagate variables that:
        // 1. Existed in the outer scope, AND
        // 2. Were actually modified during gather body execution
        //    (i.e., their value changed from the gather body's initial env).
        // This prevents nested gather closures from corrupting each other's
        // captured variables (e.g., `$n` in nested grep-div calls), while
        // still propagating genuine side effects (e.g., `$x += 1`).
        let gather_result_env = self.env().clone();
        let mut merged_env = saved_env.clone();
        // The body's OWN declarations (`my`, `for`-loop params) never merge
        // back: a body loop var shadowing a same-named consumer lexical would
        // otherwise clobber it (see CompiledCode::self_declared_names).
        let body_declared = cc.self_declared_names();
        for (k, v) in gather_result_env.iter() {
            if !saved_env.contains_key_sym(*k) {
                continue;
            }
            if body_declared.contains(k) {
                continue;
            }
            if let Some(initial) = list.env.get_sym(*k) {
                // Variable existed in both outer and gather env.
                // Only propagate if the value actually changed during execution.
                // Compare string representations as a proxy for value equality,
                // except a newly-promoted scalar cell: it deliberately has the
                // same visible value as its source while changing the storage
                // identity (`take-rw $x` must not lose that cell on gather exit).
                if v.to_string_value() != initial.to_string_value()
                    || (v.is_container_ref() && !initial.is_container_ref())
                {
                    merged_env.insert_sym(*k, v.clone());
                }
            } else {
                // Variable existed in outer scope but not in gather's initial env;
                // always propagate changes.
                merged_env.insert_sym(*k, v.clone());
            }
        }
        *self.env_mut() = merged_env;
        // Precise (env_dirty-independent) writeback: record the gather body's
        // captured-outer writes so `apply_pending_rw_writeback` (called from
        // `reconcile_caller_after_lazy_force`) drains them into the caller's local
        // slots, exactly like a `map`/`grep` callback. Without this the gather
        // case relied solely on the blanket `env_dirty` reconcile (the surface
        // docs/captured-outer-cell-sharing.md is retiring).
        self.record_eager_block_free_var_writeback(cc.as_ref(), &[]);

        // Restore Interpreter state
        self.state_scope_id.set(saved_state_scope);
        self.locals = saved_locals;
        self.stack = saved_stack;
        self.upvalues = saved_upvalues;

        // Check for errors
        run_result?;

        // Cache the result
        *list.cache.lock().unwrap() = Some(items.clone());
        Ok(items)
    }

    /// Force a gather-based LazyList to produce at least `needed` elements.
    /// Uses coroutine-style suspend/resume: the gather body pauses at each
    /// `take` once enough elements are available, and can be resumed later.
    /// Side effects (e.g. `$count++`) are correctly scoped because we pause
    /// mid-execution rather than re-running from scratch.
    pub(crate) fn force_lazy_list_vm_n(
        &mut self,
        list: &LazyList,
        needed: usize,
    ) -> Result<Vec<Value>, RuntimeError> {
        // GC safepoint (§9.2a `lazy_force`): the bounded pull/resume boundary.
        crate::gc::gc_safepoint(crate::gc::SafepointKind::LazyForce);
        let caller_code = self.current_code;
        // The body runs under its OWN readonly context, not the consumer
        // frame's (see take_readonly_state).
        let saved_readonly = self.take_readonly_state();
        // See `force_lazy_list_vm`: the body's `samewith` is lexical.
        let pushed_samewith = self.push_captured_samewith_context(&list.env);
        let r = self.force_lazy_list_vm_n_inner(list, needed);
        self.pop_captured_samewith_context(pushed_samewith);
        self.restore_readonly_state(saved_readonly);
        self.reconcile_caller_after_lazy_force(caller_code);
        // See force_lazy_list_vm: array-context elements store Any, not Nil.
        if list.in_array_context() {
            return match r {
                Ok(items) => Ok(self.decay_nil_vec_elements(items)),
                err => err,
            };
        }
        r
    }

    /// Reconcile the caller frame's local slots after a lazy force, so a
    /// captured-outer lexical mutated at reify time (e.g. `map({$c++})`,
    /// `gather`) is visible in the caller's slots even when the blanket reverse
    /// pull (`sync_locals_from_env`) is disabled (Slice F campaign). Two reify
    /// shapes record their writes differently and both must be drained here —
    /// the force machinery is the effective "call site" for the callbacks it
    /// runs, but unlike a real call op it never drains them:
    ///
    /// - a `map`/`grep` callback runs via `call_compiled_closure`, which logs
    ///   the changed caller free-vars into `pending_rw_writeback_sources`
    ///   (drained by `apply_pending_rw_writeback`, as every call op does);
    /// - a `gather` body merges its env changes and sets `env_dirty`, mirrored
    ///   by `reconcile_locals_from_env_at_site` (the #3331 helper).
    ///
    /// Both are byte-identical to the work reverse-sync ON would do (the call-op
    /// drain + the barrier pull), so ON behavior is unchanged.
    ///
    /// Also used by op handlers that run user code without a `code` parameter in
    /// hand (`say`/`note`, which dispatch a `.gist`/`.Str` closure that can
    /// mutate a captured-outer lexical): they capture `self.current_code` before
    /// the dispatch and pass it here afterwards. `caller_code` is the address of
    /// the caller frame's `CompiledCode`, captured before the inner `exec_one`
    /// runs clobbered `current_code`.
    pub(super) fn reconcile_caller_after_lazy_force(&mut self, caller_code: usize) {
        // The force body's own `exec_one` runs reset `current_code` to the lazy
        // body; restore it to the caller so a subsequent force in the same op
        // handler reconciles the right frame.
        self.current_code = caller_code;
        if caller_code == 0 {
            return;
        }
        if !self.pending_rw_writeback_sources.is_empty() {
            // SAFETY: `caller_code` is the address of the `CompiledCode` of the
            // bytecode frame that invoked this force. That frame is an ancestor
            // on the call stack (the op handler driving the force) and is alive
            // for the whole synchronous duration of the force, so the pointer is
            // valid here.
            let code = unsafe { &*(caller_code as *const CompiledCode) };
            self.apply_pending_rw_writeback(code);
        }
    }

    /// Reconcile after an *internal redispatch* (a user coercion/render method run
    /// mid-opcode without a surrounding CallMethod op: `+$obj`/`~$obj`/`if $obj`,
    /// string interpolation, `put`/`print`, …). Like
    /// [`Self::reconcile_caller_after_lazy_force`] it drains the captured-outer
    /// writeback into the caller frame's local slots, BUT it **retains** any
    /// `pending_rw_writeback_sources` entry whose name is not a slot of
    /// `caller_code` instead of dropping it.
    ///
    /// The retain matters because an internal redispatch can fire *inside another
    /// method body that has not yet returned* — e.g. a `submethod BUILD`'s
    /// `$gather ~= "($a)"` interpolation runs while a *sibling* `BUILD`'s
    /// captured-outer write (`$parent-counter++`) is still sitting in
    /// `pending_rw_writeback_sources`, queued for the outer `.new` call site to
    /// drain. The drop-on-miss `apply_pending_rw_writeback` would consume and
    /// discard that sibling write here (its slot lives in the outer frame, not the
    /// BUILD frame), so the outer `.new` drain finds nothing and the caller's slot
    /// stays stale. Retaining the miss leaves it for the frame that actually owns
    /// the slot.
    pub(crate) fn reconcile_caller_after_internal_dispatch(&mut self, caller_code: usize) {
        self.current_code = caller_code;
        if caller_code == 0 {
            return;
        }
        // SAFETY: see `reconcile_caller_after_lazy_force` — `caller_code` is the
        // live ancestor frame's `CompiledCode` address.
        let code = unsafe { &*(caller_code as *const CompiledCode) };
        if !self.pending_rw_writeback_sources.is_empty() {
            let sources = std::mem::take(&mut self.pending_rw_writeback_sources);
            let mut retained = Vec::new();
            for source in sources {
                if let Some(slot) = self.find_local_slot(code, &source) {
                    if !matches!(self.locals[slot].view(), ValueView::HashEntryRef { .. })
                        && let Some(val) = self.env().get(&source).cloned()
                    {
                        self.locals[slot] = val;
                    }
                    // matched (slot in this frame) → applied, do not retain
                } else {
                    retained.push(source);
                }
            }
            self.pending_rw_writeback_sources = retained;
        }
        self.apply_pending_caller_var_writeback(code);
    }
}
