use super::vm_control_ops::ForLoopSpec;
use super::*;

impl Interpreter {
    pub(super) fn exec_for_loop_op(
        &mut self,
        code: &CompiledCode,
        spec: &ForLoopSpec,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        // Routine declarations inside a loop body are lexically scoped to that
        // body (Raku): snapshot the routine registry before the loop and restore
        // it after so those `sub`s do not leak past the loop (or into a later
        // sibling loop). Gated on the compile-time flag so the overwhelmingly
        // common no-`sub` loop body pays zero cost. Snapshot happens once per
        // loop entry, not per iteration.
        if !spec.body_declares_routines {
            return self.exec_for_loop_op_inner(code, spec, ip, compiled_fns);
        }
        let snapshot = self.snapshot_routine_registry();
        let result = self.exec_for_loop_op_inner(code, spec, ip, compiled_fns);
        self.restore_routine_registry(snapshot);
        result
    }

    fn exec_for_loop_op_inner(
        &mut self,
        code: &CompiledCode,
        spec: &ForLoopSpec,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        // Check for gather coroutine resume state. A `CStyleLoop` marker belongs
        // to a `loop`/`while` opcode, not this for-loop, so leave it in place.
        if !matches!(
            self.gather_for_loop_resume,
            Some(crate::value::ForLoopResumeState::CStyleLoop)
        ) && let Some(resume) = self.gather_for_loop_resume.take()
        {
            let body_start = *ip + 1;
            let loop_end = spec.body_end as usize;
            match resume {
                crate::value::ForLoopResumeState::IntRange {
                    current,
                    end_val,
                    inclusive,
                } => {
                    self.exec_for_loop_int_range(
                        code,
                        spec,
                        current,
                        end_val,
                        inclusive,
                        body_start,
                        loop_end,
                        compiled_fns,
                    )?;
                    *ip = loop_end;
                    return Ok(());
                }
                crate::value::ForLoopResumeState::List { items, next_index } => {
                    let _ = self.exec_for_loop_body(
                        code,
                        spec,
                        &items,
                        body_start,
                        loop_end,
                        compiled_fns,
                        next_index,
                    )?;
                    *ip = loop_end;
                    return Ok(());
                }
                crate::value::ForLoopResumeState::LazyGather {
                    lazy_list,
                    next_index,
                } => {
                    self.exec_for_loop_lazy_gather_from(
                        code,
                        spec,
                        &lazy_list,
                        next_index,
                        body_start,
                        loop_end,
                        compiled_fns,
                    )?;
                    *ip = loop_end;
                    return Ok(());
                }
                // Guarded out above: a CStyleLoop marker is never taken here.
                crate::value::ForLoopResumeState::CStyleLoop => unreachable!(),
            }
        }

        // Fresh (non-resume) entry to the loop statement: state variables
        // declared inside the body re-initialize (fresh block clone per
        // statement execution — see `reset_state_locals_in_range`). The
        // resume paths above returned early, so this never fires on a
        // gather-coroutine re-entry.
        if !code.state_locals.is_empty() {
            self.reset_state_locals_in_range(code, *ip + 1, spec.body_end as usize);
        }

        let iterable = self.stack.pop().unwrap();

        // Handle lazy IO lines: iterate by pulling one line at a time
        // so that $fh.tell reflects the current read position.
        if let ValueView::LazyIoLines { handle, kv, words } = iterable.view() {
            let body_start = *ip + 1;
            let loop_end = spec.body_end as usize;
            self.exec_for_loop_lazy_io_lines(
                code,
                spec,
                handle,
                kv,
                words,
                body_start,
                loop_end,
                compiled_fns,
            )?;
            *ip = loop_end;
            return Ok(());
        }

        // Fast path for integer range iteration: avoid materializing the entire range.
        // Applies when the range is simple (arity 1, not threaded, no writeback, no collect).
        if !spec.threaded && !spec.collect && !spec.do_writeback && spec.arity <= 1 && !spec.kv_mode
        {
            let int_range = match iterable.view() {
                ValueView::Range(a, b) => Some((a, b, true)), // a..b (inclusive)
                ValueView::RangeExcl(a, b) => Some((a, b, false)), // a..^b (exclusive end)
                ValueView::RangeExclStart(a, b) => Some((a + 1, b, true)),
                ValueView::RangeExclBoth(a, b) => Some((a + 1, b, false)),
                _ => None,
            };
            if let Some((start, end_val, inclusive)) = int_range {
                let body_start = *ip + 1;
                let loop_end = spec.body_end as usize;
                self.exec_for_loop_int_range(
                    code,
                    spec,
                    start,
                    end_val,
                    inclusive,
                    body_start,
                    loop_end,
                    compiled_fns,
                )?;
                *ip = loop_end;
                return Ok(());
            }
        }

        // For gather-based, sequence-spec, or lazy map/grep pipeline LazyList
        // iterables, iterate lazily by pulling items one at a time. This avoids
        // materializing infinite sequences.
        if let ValueView::LazyList(ll) = iterable.view()
            && (ll.coroutine.is_some()
                || ll.sequence_spec.is_some()
                // An endpoint-less closure sequence (`for 2, 3, *+2 ... * { }`) must be
                // pulled on demand too. Without this it fell through to the eager path,
                // which sees only the 32-element prefix `eval_sequence` generated up
                // front and ends the loop there.
                || ll.closure_seq.is_some()
                || ll.lazy_pipe.is_some()
                || ll.walk_pending.is_some()
                // `for $cat.lines` / `for $cat.handles`: pull one element per
                // iteration so `on-switch` fires and `.path` tracks the current
                // handle inside the loop body (Rakudo's lazy semantics).
                || ll.cat_pull.is_some())
        {
            let body_start = *ip + 1;
            let loop_end = spec.body_end as usize;
            self.exec_for_loop_lazy_gather(code, spec, &ll, body_start, loop_end, compiled_fns)?;
            *ip = loop_end;
            return Ok(());
        }

        // A bare `Seq` is single-shot: a `for` loop consumes its iterator, so a
        // second iteration of the SAME Seq throws X::Seq::Consumed (Rakudo). A Seq
        // that was array-contextualized (`@$s`, which marks it cached) stays
        // re-iterable, so a `for @$s` loop must NOT consume it — otherwise a
        // second `for @$s` would spuriously throw (surfaced by Zef::Pluggable
        // iterating `@$backend` across two calls).
        if let ValueView::Seq(arc) = iterable.view()
            && !crate::value::seq_is_cached(&arc)
        {
            crate::value::seq_consume(&arc)?;
        }
        let raw_items = if let ValueView::LazyList(ll) = iterable.view() {
            self.force_lazy_list_vm(&ll)?
        } else if let ValueView::Channel(ch) = iterable.view() {
            // Drain the channel synchronously, blocking on receive until the
            // channel is closed. Propagate any failure as an exception so the
            // surrounding `start { }` / `try` can observe it.
            let mut items = Vec::new();
            loop {
                match ch.receive_result() {
                    Ok(v) if v.is_nil() => break,
                    Ok(v) => items.push(v),
                    Err(cause) => {
                        let ex = crate::runtime::Interpreter::as_exception_value(cause);
                        let mut err = RuntimeError::new(ex.to_string_value());
                        err.exception = Some(Box::new(ex));
                        return Err(err);
                    }
                }
            }
            items
        } else if let Some(bytes) = Self::for_blob_byte_items(&iterable) {
            // A non-itemized Blob/Buf iterates its bytes (matches raku: a `Blob`
            // value or `Blob:D`-typed param yields its bytes — e.g. the
            // `for $data -> $b1, $b2?, $b3?` encoder loop in MIME::Base64).
            // mutsu has no itemization marker for Blobs, so `for $itemized_blob`
            // also expands here — a corner not exercised by any roast Buf/Blob
            // test.
            bytes
        } else if let ValueView::Hash(gc) = iterable.view() {
            // `for %h` / `for %h -> $p`: bind each pair's value to a live
            // `HashEntryRef` so an in-loop `%h{$p.key} = X` is observable through
            // a later `$p.value` read (raku's live hash-iteration pairs).
            Self::hash_live_pairs(&gc, iterable.hash_is_itemized())
        } else {
            runtime::value_to_list(&iterable)
        };
        // When autothread_junctions is set (parameter typed as Any or more
        // specific), expand Junction items into their eigenstates so each
        // eigenstate is iterated separately.
        let items = if spec.autothread_junctions {
            let mut expanded = Vec::new();
            for item in raw_items {
                if let ValueView::Junction { values, .. } = item.view() {
                    for v in values.iter() {
                        expanded.push(v.clone());
                    }
                } else {
                    expanded.push(item);
                }
            }
            expanded
        } else {
            raw_items
        };
        // When `-> {}` was used (explicit zero params), throw if any items would be passed.
        if spec.explicit_zero_params && !items.is_empty() {
            return Err(RuntimeError::new(format!(
                "Too many positionals passed; expected 0 arguments but got {}",
                items.len()
            )));
        }
        let body_start = *ip + 1;
        let loop_end = spec.body_end as usize;

        if spec.threaded {
            // race for / hyper for: run the loop body in a spawned thread
            // so that $*THREAD.id returns a different value.
            let result = std::thread::scope(|s| {
                s.spawn(|| {
                    self.exec_for_loop_body(
                        code,
                        spec,
                        &items,
                        body_start,
                        loop_end,
                        compiled_fns,
                        0,
                    )
                })
                .join()
                .unwrap_or_else(|_| Err(RuntimeError::new("thread panicked in race/hyper for")))
            });
            *ip = loop_end;
            return result.map(|_| ());
        }

        // Live-array iteration (RT113026): `for @a -> $n { @a.push: ... }` must
        // iterate the *live* array — raku keeps yielding elements pushed during the
        // loop. mutsu materialized `items` once via `value_to_list`, so it stopped
        // at the original length. After each pass, if the single plain source array
        // grew AND the loop ran to completion (no `last`/`return`), pick up the new
        // tail (`resume_index = old_len`) and continue. Gated narrowly so only a
        // bare `for @array` (no multi-arity / kv / values / pairs / non-array
        // source) takes this path; everything else runs exactly as before.
        let live_src: Option<&String> = spec.single_array_source.as_ref().filter(|_| {
            spec.arity <= 1
                && !spec.kv_mode
                && !spec.values_mode
                && !spec.loop_var_wraps_element
                // A collecting loop (`my @r = (for @a {...})`) pushes its result
                // array at the END of each `exec_for_loop_body` pass, so a
                // continuation would emit one collected array per pass — keep
                // those on the single-pass path (live-array growth in a collect
                // context is not exercised by roast and needs cross-pass accum.).
                && !spec.collect
                && matches!(iterable.view(), ValueView::Array(..))
        });
        let mut all_items = items;
        let mut resume = 0usize;
        loop {
            let completed = self.exec_for_loop_body(
                code,
                spec,
                &all_items,
                body_start,
                loop_end,
                compiled_fns,
                resume,
            )?;
            let Some(name) = live_src else { break };
            if !completed {
                break;
            }
            // Re-read the live source array. In the single-store model `@a` is
            // authoritative in its local slot (the env copy can be stale), so try
            // the local slot first (the compiler-baked slot §1.5, else by name —
            // bare and `@`-sigiled), then env.
            let live = self
                .resolve_local_slot(code, spec.single_array_source_local, name)
                .or_else(|| self.find_local_slot(code, &format!("@{name}")))
                .map(|s| self.locals[s].clone())
                .or_else(|| self.env().get(name).cloned());
            let grown = match live.as_ref().map(Value::view) {
                Some(ValueView::Array(arc, _)) if arc.len() > all_items.len() => {
                    arc.as_slice().to_vec()
                }
                _ => break,
            };
            resume = all_items.len();
            all_items = grown;
        }
        *ip = loop_end;
        Ok(())
    }
}
