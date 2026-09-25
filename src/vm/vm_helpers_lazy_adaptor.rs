//! Stateful lazy list adaptors (#9159).
//!
//! A [`PipeAdaptor`] rides a lazy `map`/`grep` pipe stage ([`MapGrepSpec`])
//! and turns one list method or list operator into a pull-driven iterator:
//! every step pulls only what it needs from its source(s) and emits zero or
//! more elements into the pipe's cache. The same code serves a finite reified
//! source and an infinite one, so no prefix cap is ever needed:
//! `(1..*).map(* + 1).rotor(2)`, `(1..*) Z (1..*)`, `((1..*) X (1, 2))[600]`
//! and `roundrobin(1..*, (5, 6))` all stream exactly as they do in Rakudo.

use super::*;
use crate::value::{PipeAdaptor, PullOperand, RowCombine};

/// What one adaptor step produced.
struct AdaptorStep {
    out: Vec<Value>,
    /// The spec's `source_idx` after the step.
    next_idx: usize,
    /// Nothing more will ever be produced.
    done: bool,
}

impl AdaptorStep {
    fn finished(next_idx: usize) -> Self {
        Self {
            out: Vec::new(),
            next_idx,
            done: true,
        }
    }
}

/// Whether `v` is an operand a zip/cross/roundrobin cannot measure up front:
/// a lazy list, a Range with an infinite end, or a list extended by a
/// trailing `*`.
pub(crate) fn is_unbounded_operand(v: &Value) -> bool {
    match v.view() {
        ValueView::LazyList(_) => true,
        ValueView::Range(_, b)
        | ValueView::RangeExcl(_, b)
        | ValueView::RangeExclStart(_, b)
        | ValueView::RangeExclBoth(_, b) => b == i64::MAX,
        ValueView::GenericRange { end, .. } => {
            let f = end.to_f64();
            f.is_infinite() && f.is_sign_positive()
        }
        _ => extended_list(v).is_some() || lazy_segments(v).is_some(),
    }
}

/// A list literal carrying a slipped lazy list as a direct element
/// (`(1, |map {...}, 0..*)`), which zips as one flattened sequence.
fn lazy_segments(v: &Value) -> Option<Vec<Value>> {
    match v.view() {
        ValueView::Array(items, kind)
            if !kind.is_itemized()
                && items
                    .iter()
                    .any(|item| matches!(item.view(), ValueView::LazyList(_))) =>
        {
            Some(items.to_vec())
        }
        _ => None,
    }
}

/// A list whose last element is `*` (`(1, 2, *)`): Rakudo's zip extends it by
/// repeating its last real element.
fn extended_list(v: &Value) -> Option<(Vec<Value>, Value)> {
    let items = match v.view() {
        ValueView::Array(items, kind) if !kind.is_itemized() => items.to_vec(),
        ValueView::Seq(items) => items.to_vec(),
        _ => return None,
    };
    if items.len() >= 2 && matches!(items.last().map(Value::view), Some(ValueView::Whatever)) {
        let items = items[..items.len() - 1].to_vec();
        let fill = items.last().cloned().unwrap_or(Value::NIL);
        Some((items, fill))
    } else {
        None
    }
}

pub(super) fn pull_operand_of(v: &Value) -> PullOperand {
    if let Some((items, fill)) = extended_list(v) {
        return PullOperand::Extended { items, fill };
    }
    match lazy_segments(v) {
        Some(items) => PullOperand::Segments(items),
        None => PullOperand::Source(v.clone()),
    }
}

impl Interpreter {
    /// The first `rows` rows of a zip whose shortest column is the finite one
    /// (at least one column is a plain list): finite columns are read whole,
    /// unbounded ones pulled only as far as the rows need. Fewer rows come
    /// back when a lazy column turns out shorter.
    // Cost: O(sum f_i + n * r) pulls, f_i = elements of each finite column,
    // n = columns, r = rows.
    pub(crate) fn zip_rows_bounded(
        &mut self,
        columns: &[Value],
    ) -> Result<Vec<Vec<Value>>, RuntimeError> {
        let finite: Vec<Option<Vec<Value>>> = columns
            .iter()
            .map(|c| (!is_unbounded_operand(c)).then(|| Self::zip_operand_list(c)))
            .collect();
        let rows = finite.iter().flatten().map(Vec::len).min().unwrap_or(0);
        let mut out: Vec<Vec<Value>> = Vec::with_capacity(rows);
        'rows: for i in 0..rows {
            let mut row = Vec::with_capacity(columns.len());
            for (col, fin) in columns.iter().zip(&finite) {
                match fin {
                    Some(items) => row.push(items[i].clone()),
                    None => match self.pull_operand(&pull_operand_of(col), i)? {
                        Some(v) => row.push(v),
                        None => break 'rows,
                    },
                }
            }
            out.push(row);
        }
        Ok(out)
    }

    /// The elements of a finite zip operand. `Nil` in zip context is a
    /// one-element list (not empty): `Nil Z+ 2` is `(2)`, Nil coercing to 0.
    pub(crate) fn zip_operand_list(v: &Value) -> Vec<Value> {
        match v.view() {
            ValueView::Nil => vec![Value::NIL],
            _ => crate::runtime::value_to_list(v),
        }
    }

    /// Pull element `idx` of one adaptor operand (`None` once exhausted).
    // Cost: O(1) for a Range or reified list; a lazy list pulls up to `idx`.
    fn pull_operand(
        &mut self,
        op: &PullOperand,
        idx: usize,
    ) -> Result<Option<Value>, RuntimeError> {
        match op {
            PullOperand::Source(v) => self.pull_source_element(v, idx),
            PullOperand::Extended { items, fill } => Ok(Some(
                items.get(idx).cloned().unwrap_or_else(|| fill.clone()),
            )),
            PullOperand::Segments(items) => {
                // Element `idx` of the flattened sequence: a plain element is
                // one position; a lazy child spans as many as it produces,
                // which is only known once it is exhausted.
                let mut base = 0usize;
                for item in items {
                    if let ValueView::LazyList(ll) = item.view() {
                        if let Some(v) = self.pull_source_element(item, idx - base)? {
                            return Ok(Some(v));
                        }
                        base += ll
                            .cache
                            .lock()
                            .unwrap_or_else(|e| e.into_inner())
                            .as_ref()
                            .map_or(0, Vec::len);
                    } else if base == idx {
                        return Ok(Some(item.clone()));
                    } else {
                        base += 1;
                    }
                }
                Ok(None)
            }
        }
    }

    /// Turn one zip/cross row into its output element.
    // Cost: O(n) operator calls, n = elements in the row.
    pub(super) fn combine_row(
        &mut self,
        combine: &RowCombine,
        row: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match combine {
            RowCombine::List => Ok(Value::array(row)),
            // ADR-0021 I2: data-minted pairs default positional, and the key
            // keeps its own value/type.
            RowCombine::Pair => match <[Value; 2]>::try_from(row) {
                Ok([k, v]) => Ok(Value::value_pair(k, v)),
                Err(row) => Ok(Value::array(row)),
            },
            RowCombine::SmartMatch => {
                let mut acc = row.first().cloned().unwrap_or(Value::NIL);
                for r in row.iter().skip(1) {
                    acc = Value::truth(self.vm_smart_match(&acc, r));
                }
                Ok(acc)
            }
            RowCombine::Infix(op) => {
                let shape = crate::compiled_operator::InfixShape::lower(op.as_str());
                let mut it = row.into_iter();
                let mut acc = it.next().unwrap_or(Value::NIL);
                for r in it {
                    acc = self.eval_infix_shape(shape.as_ref(), &acc, &r)?;
                }
                Ok(acc)
            }
            RowCombine::With { func, fold: true } => self.zip_with_combine_row(func, row),
            RowCombine::With { func, fold: false } => self.call_sub_value(func.clone(), row, false),
        }
    }

    /// Run one step of the adaptor stage of `list` (a pipe carrying
    /// `MapGrepSpec::adaptor`), appending what it produced to the cache.
    ///
    /// The adaptor is moved out of the spec for the duration of the step (its
    /// state can be large -- a `unique` seen-set -- so it is never cloned per
    /// step); a re-entrant pull of the same list meanwhile finds
    /// `PipeAdaptor::Busy` and is refused rather than corrupting the state.
    // Cost: O(1) amortized pulls per emitted element for every adaptor except
    // `rotor` with overlap (re-reads `count` elements per chunk) and
    // `unique`/`repeated` with `:with` (O(u) comparator calls).
    pub(super) fn step_lazy_adaptor(&mut self, list: &LazyList) -> Result<(), RuntimeError> {
        let (source, func, idx, adaptor) = {
            let Some(pipe) = list.lazy_pipe.as_ref() else {
                return Ok(());
            };
            let mut spec = pipe.lock().unwrap_or_else(|e| e.into_inner());
            (
                spec.source.clone(),
                spec.func.clone(),
                spec.source_idx,
                spec.adaptor
                    .as_mut()
                    .map(|a| std::mem::replace(a.as_mut(), PipeAdaptor::Busy)),
            )
        };
        let Some(mut adaptor) = adaptor.filter(|a| !matches!(a, PipeAdaptor::Busy)) else {
            return Err(RuntimeError::new(
                "Cannot pull from a lazy list while it is producing its next element",
            ));
        };
        let step = self.run_adaptor_step(&source, &func, idx, &mut adaptor);
        let Some(pipe) = list.lazy_pipe.as_ref() else {
            return Ok(());
        };
        let mut spec = pipe.lock().unwrap_or_else(|e| e.into_inner());
        spec.adaptor = Some(Box::new(adaptor));
        let step = match step {
            Ok(step) => step,
            Err(e) => {
                // A callback may end the sequence (`last`) like it does a map.
                if e.is_last() {
                    spec.done = true;
                    return Ok(());
                }
                return Err(e);
            }
        };
        spec.source_idx = step.next_idx;
        if step.done {
            spec.done = true;
        }
        drop(spec);
        if !step.out.is_empty() {
            list.cache
                .lock()
                .unwrap_or_else(|e| e.into_inner())
                .get_or_insert_with(Vec::new)
                .extend(step.out);
        }
        Ok(())
    }

    fn run_adaptor_step(
        &mut self,
        source: &Value,
        func: &Value,
        idx: usize,
        adaptor: &mut PipeAdaptor,
    ) -> Result<AdaptorStep, RuntimeError> {
        let one = |out: Vec<Value>, next_idx: usize| AdaptorStep {
            out,
            next_idx,
            done: false,
        };
        match adaptor {
            PipeAdaptor::Skip { remaining } => {
                let Some(elem) = self.pull_source_element(source, idx)? else {
                    return Ok(AdaptorStep::finished(idx));
                };
                if *remaining > 0 {
                    *remaining -= 1;
                    return Ok(one(Vec::new(), idx + 1));
                }
                Ok(one(vec![elem], idx + 1))
            }
            PipeAdaptor::MultiMap { arity } => {
                let mut args = Vec::with_capacity(*arity);
                for i in 0..*arity {
                    match self.pull_source_element(source, idx + i)? {
                        Some(v) => args.push(v),
                        None if i == 0 => return Ok(AdaptorStep::finished(idx)),
                        None => {
                            return Err(RuntimeError::new(
                                "Not enough elements for map block arity",
                            ));
                        }
                    }
                }
                let next = idx + *arity;
                match self.vm_call_on_value(func.clone(), args, None) {
                    Ok(v) => Ok(one(
                        match v.view() {
                            ValueView::Slip(items) => items.as_ref().clone(),
                            _ => vec![v],
                        },
                        next,
                    )),
                    Err(e) if e.is_next() => Ok(one(Vec::new(), next)),
                    Err(e) => Err(e),
                }
            }
            PipeAdaptor::Chunk {
                specs,
                next_spec,
                partial,
                emitted,
            } => {
                let (count, gap) = specs[*next_spec % specs.len()];
                let mut chunk = Vec::with_capacity(count);
                for i in 0..count {
                    match self.pull_source_element(source, idx + i)? {
                        Some(v) => chunk.push(v),
                        None => break,
                    }
                }
                if chunk.len() < count {
                    // A trailing short chunk is kept only under `:partial`,
                    // and not when an overlapping spec already covered it
                    // (same rule as the eager `dispatch_rotor`).
                    let covered = gap < 0 && *emitted && (chunk.len() as i64) < -gap;
                    let out = if *partial && !chunk.is_empty() && !covered {
                        vec![Value::array(chunk)]
                    } else {
                        Vec::new()
                    };
                    return Ok(AdaptorStep {
                        out,
                        next_idx: idx,
                        done: true,
                    });
                }
                let next = idx as i64 + count as i64 + gap;
                if next < 0 {
                    return Err(RuntimeError::new(
                        "X::OutOfRange: Rotoring gap is too large",
                    ));
                }
                *next_spec += 1;
                *emitted = true;
                Ok(one(vec![Value::array(chunk)], next as usize))
            }
            PipeAdaptor::Distinct(state) => {
                let Some(elem) = self.pull_source_element(source, idx)? else {
                    return Ok(AdaptorStep::finished(idx));
                };
                let keep = self.distinct_admit(state, &elem)?;
                Ok(one(if keep { vec![elem] } else { Vec::new() }, idx + 1))
            }
            PipeAdaptor::Produce { op, acc } => {
                // Rakudo's one-behind emit order (see `eval_produce_over_items`):
                // a running value is emitted only once the NEXT reducer call
                // has succeeded, so `last` drops the pending value too.
                let Some(elem) = self.pull_source_element(source, idx)? else {
                    let out = acc.take().into_iter().collect();
                    return Ok(AdaptorStep {
                        out,
                        next_idx: idx,
                        done: true,
                    });
                };
                let Some(prev) = acc.take() else {
                    *acc = Some(elem);
                    return Ok(one(Vec::new(), idx + 1));
                };
                match self.produce_step(op, prev.clone(), elem) {
                    Ok(new_acc) => {
                        *acc = Some(new_acc);
                        Ok(one(vec![prev], idx + 1))
                    }
                    Err(e) if e.is_last() => Ok(AdaptorStep::finished(idx + 1)),
                    Err(e) if e.is_next() => {
                        *acc = Some(prev);
                        Ok(one(Vec::new(), idx + 1))
                    }
                    Err(e) => Err(e),
                }
            }
            PipeAdaptor::Zip { operands, combine } => {
                let mut row = Vec::with_capacity(operands.len());
                for op in operands.iter() {
                    match self.pull_operand(op, idx)? {
                        Some(v) => row.push(v),
                        None => return Ok(AdaptorStep::finished(idx)),
                    }
                }
                let combine = combine.clone();
                Ok(one(vec![self.combine_row(&combine, row)?], idx + 1))
            }
            PipeAdaptor::Cross {
                operands,
                combine,
                pos,
                started,
            } => {
                if *started {
                    // Advance the odometer: the last operand fastest; an
                    // exhausted operand wraps to 0 and carries into the one
                    // before it.
                    let mut j = operands.len();
                    loop {
                        if j == 0 {
                            return Ok(AdaptorStep::finished(idx));
                        }
                        j -= 1;
                        pos[j] += 1;
                        if self.pull_operand(&operands[j], pos[j])?.is_some() {
                            break;
                        }
                        pos[j] = 0;
                    }
                }
                *started = true;
                let mut row = Vec::with_capacity(operands.len());
                for (op, &p) in operands.iter().zip(pos.iter()) {
                    match self.pull_operand(op, p)? {
                        Some(v) => row.push(v),
                        // Only reachable on the first row: an empty operand
                        // makes the whole product empty.
                        None => return Ok(AdaptorStep::finished(idx)),
                    }
                }
                let combine = combine.clone();
                Ok(one(vec![self.combine_row(&combine, row)?], idx + 1))
            }
            PipeAdaptor::Repeat { remaining } => {
                if *remaining == Some(0) {
                    return Ok(AdaptorStep::finished(idx));
                }
                if let Some(r) = remaining.as_mut() {
                    *r -= 1;
                }
                let mut out = Vec::new();
                self.repeat_lhs_into(func, &mut out)?;
                // A repetition that is an empty Slip contributes nothing, and
                // so would every later one: end the list instead of pulling
                // forever (`(Slip.new xx *).head(2)` is `()` in Rakudo).
                if out.is_empty() {
                    return Ok(AdaptorStep::finished(idx + 1));
                }
                Ok(one(out, idx + 1))
            }
            // `step_lazy_adaptor` refuses a Busy adaptor before getting here.
            PipeAdaptor::Busy => Ok(AdaptorStep::finished(idx)),
            PipeAdaptor::Roundrobin {
                operands,
                alive,
                slip,
            } => {
                let mut row = Vec::new();
                for (op, live) in operands.iter().zip(alive.iter_mut()) {
                    if !*live {
                        continue;
                    }
                    match self.pull_operand(op, idx)? {
                        Some(v) => row.push(v),
                        None => *live = false,
                    }
                }
                if row.is_empty() {
                    return Ok(AdaptorStep::finished(idx));
                }
                let out = if *slip { row } else { vec![Value::array(row)] };
                Ok(one(out, idx + 1))
            }
        }
    }
}
