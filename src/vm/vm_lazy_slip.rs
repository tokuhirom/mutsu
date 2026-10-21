//! A method called on a lazy-tail Slip (#13117, `builtins::lazy_slip`) reads
//! the Slip's elements as the lazy concatenation of its items, the same lazy
//! list the list constructor builds when the Slip lands in a list literal.

use super::*;

impl Interpreter {
    /// The receiver a method call on `target` reads: the lazy concatenation of
    /// a lazy-tail Slip's items, or `target` unchanged. A method about the
    /// Slip itself rather than its elements (its type, identity, `.Slip`,
    /// `.raku`) keeps the Slip; `.gist` is the lazy list's `(...)`.
    // Cost: O(k) to build the concatenation, k = items in the Slip; O(1) for
    // any other receiver.
    pub(crate) fn lazy_slip_receiver(target: Value, method: &str) -> Value {
        if matches!(
            method,
            "WHAT"
                | "WHICH"
                | "WHO"
                | "HOW"
                | "VAR"
                | "Slip"
                | "raku"
                | "Str"
                | "item"
                | "defined"
                | "isa"
                | "does"
                | "can"
        ) || method.starts_with('^')
            || !crate::builtins::lazy_slip::has_lazy_tail(&target)
        {
            return target;
        }
        let ValueView::Slip(items) = target.view() else {
            return target;
        };
        let mut elems = Vec::with_capacity(items.len());
        let mut lazy_slots = Vec::new();
        Self::extend_with_slip_items(&mut elems, &mut lazy_slots, &items);
        Self::lazy_literal_with_slipped_tail(elems, &lazy_slots, false)
    }

    /// The Slip an eager consumer flattens into its result (a `.map`
    /// callback's value, `take`, a `flat` argument): each lazy item of a
    /// lazy-tail Slip that ends is reified in place, so
    /// `(1, 2).map({ slip((gather { take $_ }).lazy) })` is `(1 2)` as in
    /// Rakudo, whose Slip reifies from the list as its consumer iterates. An
    /// item that may not end (an infinite Range or sequence, an infinite
    /// pipe, a gather still running after the bounded pull) stays one lazy
    /// item, its pulled prefix cached. Every other value is returned
    /// unchanged. This is the one interpreter-side place a lazy-tail Slip is
    /// flattened (ADR-11678 §8.6).
    // Cost: O(1) for a value that is not a lazy-tail Slip; otherwise O(k) for
    // k items plus reifying each ending lazy item (a gather's pull is bounded
    // by `MAP_RESULT_GATHER_FORCE_CAP`).
    pub(crate) fn reify_lazy_slip_tail(&mut self, val: Value) -> Result<Value, RuntimeError> {
        if !val.is_lazy_tail_slip() {
            return Ok(val);
        }
        let ValueView::Slip(items) = val.view() else {
            return Ok(val);
        };
        let mut out = Vec::with_capacity(items.len());
        for item in items.iter() {
            match item.view() {
                ValueView::LazyList(ll) if ll.renders_lazy_placeholder() => {
                    match self.reify_ending_lazy_list(&ll)? {
                        Some(elems) => out.extend(elems),
                        None => out.push(item.clone()),
                    }
                }
                _ => out.push(item.clone()),
            }
        }
        Ok(Value::slip(out))
    }

    /// Settle the lazy-tail Slips a `.map` loop left in `produced[from..]`.
    /// A callback result that is a lazy-tail Slip whose tail cannot be seen to
    /// end reaches `produced` whole, as a Slip element (a Slip is otherwise
    /// always flattened there), so the loop can stay a plain `Vec` loop
    /// (#14295). With `stream` the first such Slip and everything the chunk
    /// produced after it become the lazy list answered: the consumer reads it
    /// before the callback runs again, as Rakudo's map iterator pulls a
    /// slipped iterator. Otherwise every such Slip is read to its end in
    /// place, which does not return for a Slip that has none (an eager
    /// consumer of an infinite list does not either).
    // Cost: O(p) to scan the p elements produced; O(r) to build or read the
    // r elements from the first Slip on.
    pub(crate) fn settle_map_slip_markers(
        &mut self,
        produced: &mut Vec<Value>,
        from: usize,
        stream: bool,
    ) -> Result<Option<Value>, RuntimeError> {
        let Some(first) = produced[from..]
            .iter()
            .position(|v| matches!(v.view(), ValueView::Slip(_)))
        else {
            return Ok(None);
        };
        let rest = produced.split_off(from + first);
        if stream {
            return Ok(Some(Self::lazy_map_parts(rest)));
        }
        for v in rest {
            if !matches!(v.view(), ValueView::Slip(_)) {
                produced.push(v);
                continue;
            }
            let slipped = Self::lazy_slip_receiver(v, "");
            match slipped.view() {
                ValueView::LazyList(ll) => produced.extend(self.force_lazy_list_vm(&ll)?),
                _ => produced.extend(crate::runtime::value_to_list(&slipped)),
            }
        }
        Ok(None)
    }

    /// The elements a `.map` loop produced as one value: a plain list, or the
    /// lazy concatenation of its runs and the tails of the lazy-tail Slips
    /// among them ([`Self::settle_map_slip_markers`]).
    // Cost: O(p), p = elements produced.
    pub(crate) fn map_produced_value(produced: Vec<Value>) -> Value {
        if produced
            .iter()
            .any(|v| matches!(v.view(), ValueView::Slip(_)))
        {
            Self::lazy_map_parts(produced)
        } else {
            Value::array(produced)
        }
    }

    /// `items` with each Slip element read as its lazy tail and each run of
    /// the others as one finite part, concatenated lazily.
    // Cost: O(p), p = elements.
    fn lazy_map_parts(items: Vec<Value>) -> Value {
        let mut parts = Vec::new();
        let mut run = Vec::new();
        for v in items {
            if matches!(v.view(), ValueView::Slip(_)) {
                if !run.is_empty() {
                    parts.push(Value::array(std::mem::take(&mut run)));
                }
                parts.push(Self::lazy_slip_receiver(v, ""));
            } else {
                run.push(v);
            }
        }
        if !run.is_empty() {
            parts.push(Value::array(run));
        }
        if parts.len() == 1 {
            return parts.remove(0);
        }
        Self::lazy_concat_pipe(parts, false)
    }

    /// `args` with each lazy-tail Slip passed through
    /// [`Self::reify_lazy_slip_tail`], or `None` when no argument is one.
    // Cost: O(n), n = arguments, plus the reifications.
    pub(crate) fn reify_lazy_slip_args(
        &mut self,
        args: &[Value],
    ) -> Result<Option<Vec<Value>>, RuntimeError> {
        if !args.iter().any(Value::is_lazy_tail_slip) {
            return Ok(None);
        }
        args.iter()
            .map(|a| self.reify_lazy_slip_tail(a.clone()))
            .collect::<Result<_, _>>()
            .map(Some)
    }

    /// The elements of `ll` when it can be seen to end: a pipe whose source
    /// chain bottoms out in a finite source is forced, and a gather is pulled
    /// up to `MAP_RESULT_GATHER_FORCE_CAP` elements and answered when its body
    /// finished. `None` for anything else.
    // Cost: O(n) for n elements produced, n bounded by the cap for a gather.
    fn reify_ending_lazy_list(
        &mut self,
        ll: &crate::value::LazyList,
    ) -> Result<Option<Vec<Value>>, RuntimeError> {
        // A gather streamed through a pipe (a gather's own lazy-tail Slip, a
        // `.lazy` gather) may never end: it gets the bounded pull below.
        if Self::pipe_over_gather(ll) {
            let items = self.force_lazy_list_vm_n(ll, GATHER_SLIP_FORCE_CAP + 1)?;
            return Ok((items.len() <= GATHER_SLIP_FORCE_CAP).then_some(items));
        }
        if ll.lazy_pipe.is_some() && ll.pipe_bottoms_out_finite() {
            return self.force_lazy_list_vm(ll).map(Some);
        }
        if ll.coroutine.is_some()
            && ll.sequence_spec.is_none()
            && ll.scan_spec.is_none()
            && ll.lazy_pipe.is_none()
        {
            let items = self.force_lazy_list_vm_n(ll, GATHER_SLIP_FORCE_CAP)?;
            let finished = ll
                .coroutine
                .as_ref()
                .is_some_and(|coro| coro.lock().unwrap().finished);
            if finished {
                return Ok(Some(items));
            }
        }
        Ok(None)
    }

    /// Whether `ll` is a pipe that reads a plain gather `Seq`.
    // Cost: O(1), one lock read.
    fn pipe_over_gather(ll: &crate::value::LazyList) -> bool {
        ll.lazy_pipe.as_ref().is_some_and(|spec| {
            let spec = spec.lock().unwrap_or_else(|e| e.into_inner());
            Self::is_gather_seq(&spec.source)
        })
    }

    /// The Slip of a plain gather `Seq` (`slip(gather {...})`, `.Slip` on
    /// one). Rakudo's Slip reifies from the gather's iterator on demand, so
    /// an infinite gather slips without hanging. Whether a gather ends is
    /// undecidable, so it gets a bounded pull of `GATHER_SLIP_FORCE_CAP`
    /// elements, as a gather a `.map` callback returns does
    /// (`reify_finite_pipe_value`):
    ///
    /// - A gather that finishes within the pull gives the eager Slip of its
    ///   elements. Every consumer flattens such a Slip as before.
    /// - Otherwise the answer is a lazy-tail Slip. Its one item is a
    ///   `lazy`-marked pipe that streams the gather, with the pulled prefix
    ///   cached. Like any genuinely lazy Slip item, it reifies on demand.
    ///
    /// `consume` is `.Slip`, which iterates the Seq and so consumes it.
    /// `slip(...)` binds `+args` and leaves the Seq cached. `None` for a
    /// value that is not a plain gather.
    // Cost: running the gather body to its `GATHER_SLIP_FORCE_CAP`-th take at
    // most; O(e) to copy e elements of a gather that finished.
    pub(crate) fn gather_slip(
        &mut self,
        value: &Value,
        consume: bool,
    ) -> Result<Option<Value>, RuntimeError> {
        if !Self::is_gather_seq(value) {
            return Ok(None);
        }
        // `.Slip` takes the gather over into a fresh body, consuming the Seq;
        // `slip(...)` reads the Seq itself and leaves it cached.
        let source = if consume {
            match Self::gather_stream_source(value) {
                Some(source) => source,
                None => return Ok(None),
            }
        } else {
            if let ValueView::Seq(body) = value.view() {
                body.mark_cache_requested();
            }
            value.clone()
        };
        let ValueView::Seq(body) = source.view() else {
            return Ok(None);
        };
        let body = std::sync::Arc::clone(&body);
        // TODO: slip the gather without pulling it (ADR-11678 §8.6): the
        // bounded pull runs the body ahead of what is read, where Rakudo reads
        // on demand. It keeps the pure Slip flatten sites, which cannot run a
        // gather, seeing only the elements of a gather that ends.
        self.reify_seq_body_prefix(&body, GATHER_SLIP_FORCE_CAP + 1)?;
        if body.len() <= GATHER_SLIP_FORCE_CAP {
            return Ok(Some(Value::slip(self.reify_seq_body(&body)?)));
        }
        let mut pipe = crate::value::LazyList::new_stream_pipe(source);
        pipe.mark_lazy();
        Ok(Some(Value::slip(vec![Value::lazy_list(
            crate::gc::Gc::new(pipe),
        )])))
    }

    /// `.Slip` on a genuinely lazy receiver or a plain gather: the lazy-tail
    /// Slip (`lazy_slip::lazy_slip_tail`, [`Self::gather_slip`]), answered
    /// before any dispatch path that would reify the receiver to copy its
    /// elements.
    // Cost: O(1) for a lazy receiver; `gather_slip`'s bounded pull for a
    // gather.
    pub(super) fn lazy_slip_coercion(
        &mut self,
        target: &Value,
        method: &str,
        argc: usize,
    ) -> Result<Option<Value>, RuntimeError> {
        if method != "Slip" || argc != 0 {
            return Ok(None);
        }
        if let Some(tail) = crate::builtins::lazy_slip::lazy_slip_tail(target) {
            return Ok(Some(tail));
        }
        self.gather_slip(target, true)
    }
}

/// How many elements a gather's Slip pulls before it is left lazy (see
/// [`Interpreter::gather_slip`]): the cap a gather a `.map` callback returns
/// gets.
const GATHER_SLIP_FORCE_CAP: usize = super::vm_helpers_lazy_reify::MAP_RESULT_GATHER_FORCE_CAP;
