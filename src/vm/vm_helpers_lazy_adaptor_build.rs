//! Building lazy adaptor stages (#9159): the `Z`/`X`/`roundrobin` pipes and
//! the list-method intercept that turns `.skip`/`.rotor`/`.unique`/... on a
//! lazy invocant into a stage. The pull side lives in
//! `vm_helpers_lazy_adaptor.rs`.

use super::vm_helpers_lazy_adaptor::{is_infinite_operand, is_unbounded_operand, pull_operand_of};
use super::*;
use crate::value::{
    DistinctMode, DistinctState, IndexTransform, PipeAdaptor, PullOperand, RowCombine,
};

impl Interpreter {
    /// A lazy `Z`/`zip` over `columns` when every column is unbounded (so the
    /// result is only as long as the shortest one, which nothing can measure
    /// up front). `None` when some column is a plain finite list: the caller
    /// zips eagerly, bounded by that column.
    // Cost: O(n), n = columns.
    pub(crate) fn lazy_zip_pipe(columns: &[Value], combine: RowCombine) -> Option<Value> {
        if columns.is_empty() || !columns.iter().all(is_unbounded_operand) {
            return None;
        }
        let operands = columns.iter().map(pull_operand_of).collect();
        Some(Self::adaptor_pipe_value(
            columns[0].clone(),
            Value::NIL,
            PipeAdaptor::Zip { operands, combine },
        ))
    }

    /// A lazy `X`/`cross` over `columns` when any column is infinite.
    // Cost: O(n), n = columns.
    pub(crate) fn lazy_cross_pipe(columns: &[Value], combine: RowCombine) -> Option<Value> {
        if columns.is_empty() || !columns.iter().any(is_infinite_operand) {
            return None;
        }
        let operands: Vec<PullOperand> = columns.iter().map(pull_operand_of).collect();
        let pos = vec![0; operands.len()];
        Some(Self::adaptor_pipe_value(
            columns[0].clone(),
            Value::NIL,
            PipeAdaptor::Cross {
                operands,
                combine,
                pos,
                started: false,
            },
        ))
    }

    /// A lazy `roundrobin` over `streams` (already split per Rakudo's
    /// single-argument rule; a finite stream is passed as its element list).
    // Cost: O(n), n = streams.
    pub(crate) fn lazy_roundrobin_pipe(streams: Vec<Value>, slip: bool) -> Value {
        let alive = vec![true; streams.len()];
        let first = streams.first().cloned().unwrap_or(Value::NIL);
        let operands = streams.into_iter().map(PullOperand::Source).collect();
        Self::adaptor_pipe_value(
            first,
            Value::NIL,
            PipeAdaptor::Roundrobin {
                operands,
                alive,
                slip,
            },
        )
    }

    /// A lazy list literal whose `parts` (runs of plain elements as `List`s,
    /// slipped lazy lists as themselves) are read one after another.
    // Cost: O(p), p = parts.
    /// A slipped `lazy`-marked part keeps the whole list `.is-lazy` even when
    /// every part is finite (`(1, |(lazy 2, 3)).is-lazy` is True).
    // Cost: O(p), p = parts.
    pub(crate) fn lazy_concat_pipe(parts: Vec<Value>, array_context: bool) -> Value {
        let lazy_marked = parts
            .iter()
            .any(|p| matches!(p.view(), ValueView::LazyList(ll) if ll.is_lazy_marked()));
        let first = parts.first().cloned().unwrap_or(Value::NIL);
        let mut ll = LazyList::new_adaptor_pipe(
            first,
            Value::NIL,
            PipeAdaptor::Concat {
                parts,
                part: 0,
                base: 0,
            },
        );
        if lazy_marked {
            ll.mark_lazy();
        }
        if array_context {
            ll = ll.with_array_context();
        }
        Value::lazy_list(crate::gc::Gc::new(ll))
    }

    /// Splice a Slip's items into a list literal under construction, noting
    /// where a genuinely lazy list (one `|` kept whole rather than reifying)
    /// lands so the literal can stay lazy.
    // Cost: O(k), k = items in the Slip.
    pub(super) fn extend_with_slip_items(
        elems: &mut Vec<Value>,
        lazy_slots: &mut Vec<usize>,
        items: &[Value],
    ) {
        for item in items {
            if let ValueView::LazyList(ll) = item.view()
                && ll.renders_lazy_placeholder()
            {
                lazy_slots.push(elems.len());
            }
            elems.push(item.clone());
        }
    }

    /// A list literal with slipped lazy lists (`(1, |[\*] 1..*)`,
    /// `[0, |(1...*)]`) as a lazy concatenation: each run of plain elements is
    /// one finite part, each slipped lazy list its own part, read in order.
    /// Rakudo keeps such a literal lazy (`.is-lazy` is True) and reifies only
    /// what is read.
    // Cost: O(e), e = elements of the literal.
    pub(super) fn lazy_literal_with_slipped_tail(
        elems: Vec<Value>,
        lazy_slots: &[usize],
        is_real_array: bool,
    ) -> Value {
        let finite_part = |run: Vec<Value>| {
            if is_real_array {
                crate::runtime::utils::itemize_real_array_elements(Value::real_array(run))
            } else {
                Value::array(run)
            }
        };
        let mut parts = Vec::with_capacity(lazy_slots.len() * 2 + 1);
        let mut run = Vec::new();
        let mut slots = lazy_slots.iter().peekable();
        for (i, elem) in elems.into_iter().enumerate() {
            if slots.peek() == Some(&&i) {
                slots.next();
                if !run.is_empty() {
                    parts.push(finite_part(std::mem::take(&mut run)));
                }
                parts.push(elem);
            } else {
                run.push(elem);
            }
        }
        if !run.is_empty() {
            parts.push(finite_part(run));
        }
        Self::lazy_concat_pipe(parts, is_real_array)
    }

    fn adaptor_pipe_value(source: Value, func: Value, adaptor: PipeAdaptor) -> Value {
        Value::lazy_list(crate::gc::Gc::new(LazyList::new_adaptor_pipe(
            source, func, adaptor,
        )))
    }

    /// Whether a method call on `target` should stream through an adaptor
    /// stage instead of forcing it: an infinite Range, or a lazy list that
    /// is genuinely lazy.
    fn streams_through_adaptor(target: &Value) -> bool {
        match target.view() {
            ValueView::LazyList(ll) => {
                (ll.lazy_pipe.is_some() || ll.needs_vm_lazy_dispatch()) && ll.is_genuinely_lazy()
            }
            _ => Self::is_lazy_pipe_source(target),
        }
    }

    /// `.skip` / `.rotor` / `.batch` / `.unique` / `.repeated` / `.squish` /
    /// `.produce` / `.flat` on a lazy invocant: build the lazy stage that
    /// streams it (#9159). `Ok(None)` when the call is not one of those, the
    /// invocant is not lazy, or the arguments take a form only the eager
    /// implementation understands (which then reports on it as before).
    // Cost: O(a), a = arguments.
    pub(crate) fn try_lazy_adaptor_method(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        if !matches!(
            method,
            "skip" | "rotor" | "batch" | "unique" | "repeated" | "squish" | "produce" | "flat"
        ) || !Self::streams_through_adaptor(target)
        {
            return Ok(None);
        }
        let adaptor = match method {
            "skip" => match args {
                [] => PipeAdaptor::Skip { remaining: 1 },
                [n] => match n.view() {
                    ValueView::Int(n) => PipeAdaptor::Skip {
                        remaining: n.max(0) as usize,
                    },
                    _ => return Ok(None),
                },
                _ => return Ok(None),
            },
            "rotor" => match Self::lazy_rotor_specs(args) {
                Some((specs, partial)) => PipeAdaptor::Chunk {
                    specs,
                    next_spec: 0,
                    partial,
                    emitted: false,
                },
                None => return Ok(None),
            },
            "batch" => {
                let n = match args {
                    [n] => match n.view() {
                        ValueView::Int(n) => n,
                        ValueView::Pair(k, v) if k == "elems" => crate::runtime::to_int(v),
                        _ => return Ok(None),
                    },
                    _ => return Ok(None),
                };
                if n < 1 {
                    return Ok(None);
                }
                PipeAdaptor::Chunk {
                    specs: vec![(n as usize, 0)],
                    next_spec: 0,
                    partial: true,
                    emitted: false,
                }
            }
            "unique" | "repeated" | "squish" => {
                let mode = match method {
                    "unique" => DistinctMode::Unique,
                    "repeated" => DistinctMode::Repeated,
                    _ => DistinctMode::Squish,
                };
                let (as_fn, with_fn) =
                    crate::runtime::methods_collection_ops::distinct_adverbs(args);
                PipeAdaptor::Distinct(Box::new(DistinctState::new(mode, as_fn, with_fn)))
            }
            "produce" => match args {
                [op] if self.produce_streams(op) => PipeAdaptor::Produce {
                    op: op.clone(),
                    acc: None,
                },
                _ => return Ok(None),
            },
            // "flat"
            _ => {
                if !args.is_empty() || !matches!(target.view(), ValueView::LazyList(_)) {
                    return Ok(None);
                }
                return Ok(Some(Value::lazy_list(crate::gc::Gc::new(
                    LazyList::new_index_pipe(target.clone(), IndexTransform::Flat),
                ))));
            }
        };
        Ok(Some(Self::adaptor_pipe_value(
            target.clone(),
            Value::NIL,
            adaptor,
        )))
    }

    /// The `(count, gap)` cycle of a `.rotor` call a lazy stage can run: Int
    /// counts and `count => gap` Pairs, with an optional `:partial`. `None`
    /// for anything else (`*`, `Inf`, Range counts, a bad count), which the
    /// eager implementation handles or reports.
    fn lazy_rotor_specs(args: &[Value]) -> Option<(Vec<(usize, i64)>, bool)> {
        let mut partial = false;
        let mut raw: Vec<Value> = Vec::new();
        for arg in args {
            match arg.view() {
                ValueView::Pair(k, v) if k == "partial" => partial = v.truthy(),
                ValueView::Array(items, kind) if !kind.is_itemized() => {
                    raw.extend(items.iter().cloned())
                }
                ValueView::Seq(items) => raw.extend(items.iter().cloned()),
                _ => raw.push(arg.clone()),
            }
        }
        let count_of = |v: &Value| match v.view() {
            ValueView::Int(n) if n >= 0 => Some(n as usize),
            _ => None,
        };
        let mut specs = Vec::with_capacity(raw.len());
        for spec in &raw {
            specs.push(match spec.view() {
                ValueView::Int(_) => (count_of(spec)?, 0),
                ValueView::ValuePair(k, v) => match v.view() {
                    ValueView::Int(gap) => (count_of(k)?, gap),
                    _ => return None,
                },
                _ => return None,
            });
        }
        // A cycle that never advances would emit empty chunks forever; the
        // eager implementation stops such a cycle, so leave it to that.
        if specs.is_empty() || specs.iter().all(|&(c, g)| c as i64 + g <= 0) {
            return None;
        }
        Some((specs, partial))
    }
}
