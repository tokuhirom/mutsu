//! Native `.first` in the Interpreter (no-adverb forms).
//!
//! `@a.first`, `@a.first({ .foo })`, `@a.first(* > 3)`, `@a.first(/rx/)`,
//! `%h.first({ .value > 1 })` run entirely in the Interpreter. The `Sub` matcher block
//! (the genuine Category-B fork) is invoked through `vm_call_on_value`; non-block
//! patterns use the interpreter's single shared `smart_match`. The scan itself
//! (iteration, `pair_as_positional`) is the shared
//! [`crate::runtime::resolution_map_grep::find_first_match_generic`] — the same one the
//! interpreter's `find_first_match_over_items` uses.
//!
//! Anything with an adverb (`:k`/`:kv`/`:p`/`:v`/`:end`), a `Bool` matcher (which
//! must raise `X::Match::Bool`), or a non-list target (`Supply`/`Instance`/…)
//! falls back to the interpreter's `builtin_first` / method dispatch unchanged.

use super::*;
use crate::runtime::resolution_map_grep::{FirstMatcher, find_first_match_generic};
use crate::runtime::utils::pair_as_positional;
use crate::value::Value;

/// [`FirstMatcher`] backed by the bytecode Interpreter.
struct VmFirstMatcher<'a>(&'a mut Interpreter);

impl FirstMatcher for VmFirstMatcher<'_> {
    fn item_matches(&mut self, pattern: &Value, item: &Value) -> Result<bool, RuntimeError> {
        if matches!(pattern.view(), ValueView::Sub(_)) {
            let call_item = pair_as_positional(item);
            Ok(self
                .0
                .vm_call_on_value(pattern.clone(), vec![call_item], None)?
                .truthy())
        } else {
            self.0.try_smart_match(item, pattern)
        }
    }
}

/// The adverb a lazy `.first` answers with: `:k`, `:kv` or `:p` reshape the
/// match like the eager scan does (`format_first_result`).
#[derive(Clone, Copy, Default)]
struct FirstAdverb {
    k: bool,
    kv: bool,
    p: bool,
}

impl FirstAdverb {
    // Cost: O(1).
    fn answer(self, idx: usize, value: Value) -> Value {
        crate::runtime::builtins_collection::format_first_result(
            idx, value, self.k, self.kv, self.p,
        )
    }
}

/// The matcher and adverb of a `.first` call a lazy scan can answer: at most
/// one positional (not a `Bool`, which must raise `X::Match::Bool`), and at
/// most one of `:k`/`:kv`/`:p` (`:v` is the default). `None` for `:end`, an
/// unknown or conflicting adverb or `:!v`: the eager scan reports those.
// Cost: O(a), a = arguments.
fn lazy_first_args(args: &[Value]) -> Option<(Option<Value>, FirstAdverb)> {
    let mut func = None;
    let mut adverb = FirstAdverb::default();
    for arg in args {
        match arg.view() {
            ValueView::Pair(key, value) => {
                let on = value.truthy();
                match key.as_str() {
                    "k" => adverb.k = on,
                    "kv" => adverb.kv = on,
                    "p" => adverb.p = on,
                    "v" | "end" if on == (key == "v") => {}
                    _ => return None,
                }
            }
            ValueView::Bool(_) => return None,
            _ if func.is_none() => func = Some(arg.clone()),
            _ => return None,
        }
    }
    ((adverb.k as u8 + adverb.kv as u8 + adverb.p as u8) <= 1).then_some((func, adverb))
}

impl Interpreter {
    /// Test a `grep`/`first`-style matcher against a single `item` using
    /// Interpreter-native dispatch (a `Sub` block via `vm_call_on_value`, any other
    /// pattern via the shared `smart_match`). Used by the lazy map/grep
    /// pipeline (`force_lazy_pipe`) so the matcher runs in this Interpreter and keeps
    /// locals/env coherent (the interpreter's `eval_grep_over_items` spins up a
    /// nested Interpreter via `mem::take`, which would discard the surrounding frame).
    pub(super) fn vm_grep_item_matches(
        &mut self,
        pattern: &Value,
        item: &Value,
    ) -> Result<bool, RuntimeError> {
        VmFirstMatcher(self).item_matches(pattern, item)
    }

    /// Try to run `target.first(...)` natively. Returns `Some(result)` when
    /// handled in the Interpreter, `None` to fall back unchanged.
    // Cost: O(i), i = index of the first match, on an Array/List/Seq or an
    // unbounded Range (scanned in doubling chunks); O(e + i), e = elements, on
    // any other receiver.
    pub(super) fn try_native_first(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if method != "first" {
            return None;
        }

        // Plain list-like targets plus `Hash` (`.first` iterates its pairs);
        // `Supply`/`Instance`/… keep their own semantics -> fall back.
        if !Self::is_plain_eager_list(target) && !matches!(target.view(), ValueView::Hash(_)) {
            return None;
        }

        // At most one positional matcher and no adverbs. Any `Pair` arg is an
        // adverb (`:k`/`:v`/`:end`/…) handled only by the interpreter.
        let mut func: Option<Value> = None;
        for arg in args {
            match arg.view() {
                ValueView::Pair(..) => return None,
                // `Bool` matcher must raise X::Match::Bool -> let the interpreter
                // produce the typed exception.
                ValueView::Bool(_) => return None,
                _ if func.is_none() => func = Some(arg.clone()),
                _ => return None,
            }
        }
        // Look through a role-mixed callable (`&foo but R1`) so both legs
        // below (the batched Sub fast path and `VmFirstMatcher`'s own Sub
        // check) see a bare Sub, instead of falling through to smartmatch
        // (which never truthily matches a Mixin, silently returning Nil) --
        // see `todo/tickets/map-rejects-role-mixed-sub-as-callable.md`.
        let func = func.map(Self::unwrap_callable_mixin);

        // `.first` answers the element's VALUE; a container above is the
        // matcher's binding, not the result.
        let answer = |found: Result<Option<(usize, Value)>, RuntimeError>| match found {
            Ok(Some((_, value))) => Some(Ok(value.deref_container())),
            Ok(None) => Some(Ok(Value::NIL)),
            Err(e) => Some(Err(e)),
        };
        // Setup-once batched scan for a plain `Sub` matcher (one compile +
        // env setup, bare `run_reuse` per element, early exit) — ~25x cheaper
        // per element than the per-element closure call below. Falls through
        // for matchers it cannot handle (sub-signature, composed, .assuming).
        let scan = |interp: &mut Self, items: &[Value], from_end: bool| {
            if let Some(func_ref) = func.as_ref()
                && let Some(res) = interp.try_first_match_batched(func_ref, items, from_end)
            {
                return res;
            }
            let mut matcher = VmFirstMatcher(interp);
            find_first_match_generic(&mut matcher, func.as_ref(), items, from_end)
        };
        // An Array/List/Seq is scanned in doubling chunks, so a hit at index i
        // decomposes O(i) elements, not the whole receiver. A mutable array is
        // scanned through its element CONTAINERS, so the matcher's topic
        // aliases the element rather than a copy of its value: `@a.first({ $_
        // = 5 })` writes `@a`, as `.grep`/`.map` and `@a.values.first(...)`
        // already do. A `Hash` is decomposed whole into its pairs.
        if Self::promotable_array_len(target).is_some() || Self::first_borrows(target) {
            let found = self.find_first_match_chunked_with(target, false, scan);
            // A mutable `Array` answers the element's own container (rakudo:
            // `@a.first(*>1).VAR` is a `Scalar`; `my $x := @a.first(...)` binds
            // into `@a`), as `.head` does.
            if let Ok(Some((idx, _))) = &found
                && let ValueView::Array(_, kind) = target.view()
                && !kind.is_immutable_list()
                && let Some(cell) = target.array_slot_ref(*idx, true)
            {
                return Some(Ok(cell));
            }
            // A `.map`/`.grep` Seq holds the cells its callback returned
            // (`.map({ %h<a> })`); `.first` hands out that cell (#17094).
            if let Ok(Some((_, cell))) = &found
                && matches!(target.view(), ValueView::Seq(_))
                && cell.is_container_ref()
            {
                return Some(Ok(cell.clone()));
            }
            return answer(found);
        }
        // An unbounded range of any element type is stepped by `.succ` in
        // doubling chunks: O(i) time, nothing reified past the match.
        if let Some(mut steps) = crate::runtime::unbounded_range::Steps::new(target) {
            let mut chunk_len = 64usize;
            loop {
                let chunk = steps.take(chunk_len);
                if chunk.is_empty() {
                    return Some(Ok(Value::NIL));
                }
                match scan(self, &chunk, false) {
                    Ok(None) => {}
                    found => return answer(found),
                }
                chunk_len = (chunk_len * 2).min(1 << 16);
            }
        }
        let items = crate::runtime::utils::value_to_list_for_receiver(target);
        answer(scan(self, &items, false))
    }

    /// `.first(matcher)` (with `:k`/`:kv`/`:p` too) on a Seq nobody has read whose source is a not-yet-run
    /// `.map`/`.grep`: pull it one element at a time and stop at the match, so
    /// the callback runs only as far as the match and a callback that slips an
    /// infinite list does not make the scan read the whole Seq (#14312). The Seq
    /// is consumed, as Rakudo's `.first` consumes it. `Ok(None)` for any other
    /// call: a different method, an adverb or `Bool` matcher, or another source.
    // Cost: O(i) pulls and matcher calls, i = index of the first match.
    pub(crate) fn first_over_map_grep_seq(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        if method != "first" {
            return Ok(None);
        }
        let Some((Some(matcher), adverb)) = lazy_first_args(args) else {
            return Ok(None);
        };
        let ValueView::Seq(body) = target.view() else {
            return Ok(None);
        };
        let matcher = Self::unwrap_callable_mixin(matcher);
        let mut idx = 0usize;
        // A `Seq.new($iterator)` (a `does Sequence` class's own iterator
        // included): one `pull-one` at a time up to the match, so an endless
        // iterator answers (#16133).
        if body.unpulled_iterator().is_some() {
            let Some(crate::value::PrefixSource::Iterator(iterator)) = body.take_prefix_source()
            else {
                return Ok(None);
            };
            loop {
                let Some(item) = self.pull_iterator_prefix_to_vec(&iterator, 1)?.pop() else {
                    return Ok(Some(Value::NIL));
                };
                if VmFirstMatcher(self).item_matches(&matcher, &item)? {
                    return Ok(Some(adverb.answer(idx, item.deref_container())));
                }
                idx += 1;
            }
        }
        if !body.is_map_grep_source() {
            return Ok(None);
        }
        let Some(crate::value::PrefixSource::MapGrep(mut source)) = body.take_prefix_source()
        else {
            return Ok(None);
        };
        loop {
            let (items, exhausted) = self.pull_map_grep_prefix(&mut source, 1)?;
            for item in items {
                if VmFirstMatcher(self).item_matches(&matcher, &item)? {
                    // The callback's own cell (`.map({ %h<a> })`) is the answer
                    // (#17094); a plain value is returned bare. The `:k`/`:kv`/
                    // `:p` forms answer values.
                    return Ok(Some(if adverb.k || adverb.kv || adverb.p {
                        adverb.answer(idx, item.deref_container())
                    } else if item.is_container_ref() {
                        item
                    } else {
                        item.deref_container()
                    }));
                }
                idx += 1;
            }
            if exhausted {
                return Ok(Some(Value::NIL));
            }
        }
    }

    /// Lazy `.first` over a gather-sourced `LazyList`: pull elements
    /// incrementally via the coroutine and test the matcher, instead of forcing
    /// the whole (possibly infinite) list. Returns `Some` when handled, `None`
    /// to fall back to the eager full-force path (adverbs / `Bool` matcher).
    // Cost: O(i) pulls and matcher calls, i = index of the first match, plus
    // running the gather body that far.
    pub(crate) fn try_lazy_gather_first(
        &mut self,
        list: &crate::value::LazyList,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        // At most one positional matcher, no `Bool` matcher and no `:end`
        // (those keep interpreter semantics). `:k`/`:kv`/`:p` are answered
        // while pulling, without materializing the lazy list (List::Divvy
        // finds the stopping index of an infinite prime pipeline with `:k`).
        let (func, adverb) = lazy_first_args(args)?;
        let func = func.map(Self::unwrap_callable_mixin);

        // No matcher: `.first` is just the first element.
        let Some(func) = func else {
            return match self.force_lazy_list_vm_n(list, 1) {
                Ok(items) => Some(Ok(match items.into_iter().next() {
                    Some(item) => adverb.answer(0, item),
                    None => Value::NIL,
                })),
                Err(e) => Some(Err(e)),
            };
        };

        // Pull one element at a time and test it. Pulling singly (rather than in
        // chunks) keeps the gather body's side effects in step with the match
        // position, matching Rakudo (`.first(* >= 3)` runs the body exactly as
        // far as the first match, no further).
        //
        // Slice F (gather coroutine coherence): the matcher closure (`* >= 3`)
        // runs via `exec` and leaves `self.current_code` pointing at *its* frame.
        // `force_lazy_list_vm_n` captures `self.current_code` as the frame to
        // reconcile a captured-outer write into (`my $c; gather { loop { $c++ } }`),
        // so without restoring it each iteration the second-and-later forces
        // reconcile the matcher's frame instead of the caller's, leaving the
        // caller's `$c` slot stale under reverse-sync OFF. Pin the caller frame
        // captured at entry and restore it before every force.
        let caller_code = self.current_code;
        let mut idx = 0usize;
        loop {
            self.current_code = caller_code;
            // Only element `idx` is copied out: copying the whole prefix on
            // every pull made a long scan quadratic.
            let items = match self.force_lazy_list_vm_window(list, idx, idx + 1) {
                Ok(v) => v,
                Err(e) => return Some(Err(e)),
            };
            // No element => the gather body finished (finite source) without
            // a match.
            let Some(item) = items.into_iter().next() else {
                return Some(Ok(Value::NIL));
            };
            let matched = {
                let mut matcher = VmFirstMatcher(self);
                match matcher.item_matches(&func, &item) {
                    Ok(b) => b,
                    Err(e) => return Some(Err(e)),
                }
            };
            if matched {
                return Some(Ok(adverb.answer(idx, item)));
            }
            idx += 1;
        }
    }
}
