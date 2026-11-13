//! Running a `...` sequence's term generator ([`SeqGen`]): the eager
//! prefix `eval_sequence` computes, and the resumption a finite sequence's
//! lazy list performs past that prefix.

use super::*;
use crate::value::seq_gen::{EndpointKind, SeqGen, SeqGenOutcome, SeqMode};

impl Interpreter {
    /// Append up to `max_gen` generation steps' terms to `result`, which
    /// holds every term so far (the generator and a closure endpoint read its
    /// tail). The outcome says whether the sequence ended within the budget.
    // Cost: O(max_gen) steps, each one term computation (a generator or
    // endpoint closure call for a closure sequence).
    pub(crate) fn run_seq_gen(
        &mut self,
        g: &SeqGen,
        result: &mut Vec<Value>,
        max_gen: usize,
    ) -> Result<SeqGenOutcome, RuntimeError> {
        let mut outcome = SeqGenOutcome::default();
        // Set at the start of each step and cleared at its end: a step that
        // leaves the loop early ended the sequence.
        let mut exited_early = false;
        'seq_gen: for _ in 0..max_gen {
            exited_early = true;
            let next = match &g.mode {
                SeqMode::Closure => {
                    let suppress_generator_error =
                        matches!(g.endpoint_kind, Some(EndpointKind::Value(_)));
                    let genfn = g.generator.as_ref().unwrap();
                    match self.sequence_closure_step(
                        genfn,
                        &result,
                        g.generator_shape,
                        suppress_generator_error,
                    ) {
                        Ok(Some(v)) => {
                            // A closure generator step's result becomes BOTH
                            // the publicly exposed `$seq[N]` element AND
                            // (aliasing the same `Arc<SeqBody>`) the
                            // `history` input fed to compute a LATER step —
                            // e.g. `*.reverse ... *` calls `.reverse` on the
                            // previous element to derive the next one.
                            // `.reverse` (and most other methods) are
                            // single-use consumers under ADR-0034
                            // (`seq_method_consumes`): letting the
                            // generator's OWN internal use silently steal
                            // the same Seq the user already holds would
                            // corrupt it (`$seq[1]` would read back as the
                            // `Seq.new()` placeholder once `$seq[2]` is
                            // generated from it). Reifying here, before
                            // `next` is stored, marks the body `retained` so
                            // a later internal consuming touch serves it
                            // instead of stealing it.
                            if let ValueView::Seq(body) = v.view() {
                                let body = std::sync::Arc::clone(&body);
                                self.reify_seq_body(&body)?;
                            }
                            // A generator that returns a container it keeps mutating
                            // (`state @row`) hands back the SAME array every step. The
                            // eager prefix runs ahead of its consumer, so snapshot it:
                            // a lazily consuming `flat` would have read each row
                            // before the next step changed it.
                            let v = match v.view() {
                                ValueView::Array(items, kind) => Value::array_with_kind(
                                    crate::gc::Gc::new(crate::value::ArrayData::new(
                                        items.to_vec(),
                                    )),
                                    kind,
                                ),
                                _ => v,
                            };
                            // The eager prefix is only a warm-up: cut it off once
                            // the elements get expensive (`2, { $_**2 - $_ + 1 }
                            // ... *` doubles its digit count each step, a
                            // look-and-say string grows ~30% per step) and let the
                            // lazy continuation compute the rest on demand.
                            // TODO: drop the prefix altogether once `Array.clone`
                            // shares the generator (roast S32-array/create.t
                            // "clone and original array share reifier" leans on
                            // the first elements being cached).
                            let expensive = match v.view() {
                                ValueView::BigInt(b) => b.bits() > super::sequence::SEQ_EAGER_PREFIX_MAX_SIZE,
                                // A sequence of ever-growing lists (the generator keeps a
                                // `state @row` it doubles each step).
                                ValueView::Array(items, ..) => {
                                    items.len() as u64 > super::sequence::SEQ_EAGER_PREFIX_MAX_SIZE
                                }
                                ValueView::BigRat(n, d) => {
                                    n.bits().max(d.bits()) > super::sequence::SEQ_EAGER_PREFIX_MAX_SIZE
                                }
                                ValueView::Str(s) => s.len() as u64 > super::sequence::SEQ_EAGER_PREFIX_MAX_SIZE,
                                _ => false,
                            };
                            // Only a sequence that hands back a lazy continuation may
                            // stop early; one with a closure or regex endpoint ends as
                            // a plain array of this prefix (Math::Root's `newton`).
                            if expensive
                                && matches!(
                                    g.endpoint_kind,
                                    None | Some(EndpointKind::Value(_))
                                )
                            {
                                result.push(v);
                                break 'seq_gen;
                            }
                            v
                        }
                        Ok(None) => {
                            outcome.generation_finished = true;
                            break 'seq_gen;
                        }
                        // Re-entrant (or otherwise erroring) generator on an
                        // infinite `… *` sequence: defer to lazy instead of
                        // failing eagerly (see `deferred_after_generator_error`).
                        Err(_e) if g.endpoint_kind.is_none() => {
                            outcome.deferred_after_generator_error = true;
                            break 'seq_gen;
                        }
                        Err(e) => return Err(e),
                    }
                }
                SeqMode::Arithmetic(step) => {
                    if g.is_string_seq {
                        if *step == 0.0 {
                            // String constant sequence
                            result.last().unwrap().clone()
                        } else if g.use_codepoint {
                            // Single-char: use codepoint increment/decrement
                            let last = result.last().unwrap();
                            if let ValueView::Str(s) = last.view() {
                                let ch = s.chars().next().unwrap_or('\0');
                                let delta = if *step > 0.0 { 1i32 } else { -1i32 };
                                if let Some(next_ch) =
                                    char::from_u32((ch as i64 + delta as i64) as u32)
                                {
                                    Value::str(next_ch.to_string())
                                } else {
                                    break;
                                }
                            } else {
                                break;
                            }
                        } else if *step < 0.0 {
                            // String predecessor sequence: use .pred
                            let last = result.last().unwrap();
                            if let ValueView::Str(s) = last.view() {
                                if let Some(alpha) = g.uniform_string_alphabet.as_ref() {
                                    let mut idxs: Vec<usize> =
                                        Vec::with_capacity(s.chars().count());
                                    for ch in s.chars() {
                                        let Some(pos) = alpha.iter().position(|a| *a == ch) else {
                                            idxs.clear();
                                            break;
                                        };
                                        idxs.push(pos);
                                    }
                                    if idxs.is_empty() {
                                        Value::str(Self::string_pred(&s)?)
                                    } else {
                                        let mut borrow = true;
                                        for idx in idxs.iter_mut().rev() {
                                            if !borrow {
                                                break;
                                            }
                                            if *idx == 0 {
                                                *idx = alpha.len() - 1;
                                            } else {
                                                *idx -= 1;
                                                borrow = false;
                                            }
                                        }
                                        if borrow {
                                            return Err(RuntimeError::new(
                                                "Decrement out of range",
                                            ));
                                        }
                                        Value::str(
                                            idxs.into_iter().map(|i| alpha[i]).collect::<String>(),
                                        )
                                    }
                                } else if let Some(radix) = g.digit_string_radix {
                                    Value::str(Self::digit_string_pred_radix(&s, radix)?)
                                } else {
                                    Value::str(Self::string_pred(&s)?)
                                }
                            } else {
                                break;
                            }
                        } else {
                            // String succession sequence: use .succ
                            let last = result.last().unwrap();
                            if let ValueView::Str(s) = last.view() {
                                if let Some(alpha) = g.uniform_string_alphabet.as_ref() {
                                    let mut idxs: Vec<usize> =
                                        Vec::with_capacity(s.chars().count());
                                    for ch in s.chars() {
                                        let Some(pos) = alpha.iter().position(|a| *a == ch) else {
                                            idxs.clear();
                                            break;
                                        };
                                        idxs.push(pos);
                                    }
                                    if idxs.is_empty() {
                                        Value::str(Self::string_succ(&s))
                                    } else {
                                        let mut carry = true;
                                        for idx in idxs.iter_mut().rev() {
                                            if !carry {
                                                break;
                                            }
                                            if *idx + 1 >= alpha.len() {
                                                *idx = 0;
                                            } else {
                                                *idx += 1;
                                                carry = false;
                                            }
                                        }
                                        if carry {
                                            return Err(RuntimeError::new(
                                                "Increment out of range",
                                            ));
                                        }
                                        Value::str(
                                            idxs.into_iter().map(|i| alpha[i]).collect::<String>(),
                                        )
                                    }
                                } else if let Some(radix) = g.digit_string_radix {
                                    Value::str(Self::digit_string_succ_radix(&s, radix))
                                } else {
                                    Value::str(Self::string_succ(&s))
                                }
                            } else {
                                break;
                            }
                        }
                    } else {
                        let last = result.last().unwrap();
                        if let ValueView::Package(name) = last.view() {
                            let method = if *step >= 0.0 { "succ" } else { "pred" };
                            // A type object cannot be numified into the next element.
                            // Dispatch a user method through compiled bytecode when
                            // present, and report the missing method otherwise.
                            match self.try_dispatch_compiled_method_direct(last, method, &[]) {
                                Some(result) => result?,
                                None => {
                                    return Err(crate::runtime::did_you_mean::method_not_found(
                                        method,
                                        &name.resolve(),
                                    ));
                                }
                            }
                        } else {
                            // For non-numeric values (e.g. custom objects), try .succ/.pred
                            if matches!(last.view(), ValueView::Instance { .. }) {
                                let method = if *step >= 0.0 { "succ" } else { "pred" };
                                let saved = self.env.clone();
                                let saved_vb = self.lexicals.var_bindings.clone();
                                let v = match self.call_method_with_values(
                                    last.clone(),
                                    method,
                                    vec![],
                                ) {
                                    Ok(v) => v,
                                    Err(_) => Self::seq_add(last, *step),
                                };
                                self.env = saved;
                                self.lexicals.var_bindings = saved_vb;
                                v
                            } else if let Some((sn, sd)) = g.rat_step {
                                Self::seq_add_rat(last, sn, sd)
                            } else {
                                Self::seq_add(last, *step)
                            }
                        }
                    }
                }
                SeqMode::Geometric(ratio) => {
                    let last = result.last().unwrap();
                    Self::seq_mul(last, *ratio)
                }
                SeqMode::GeometricRat(num, den) => {
                    let last = result.last().unwrap();
                    Self::seq_mul_rat(last, *num, *den)
                }
            };

            // Flatten Slip into individual items for processing
            let items_to_add: Vec<Value> = match next.view() {
                ValueView::Slip(items) => items.iter().cloned().collect(),
                _ => vec![next.clone()],
            };

            if g.endpoint_kind.is_none() {
                result.extend(items_to_add);
                exited_early = false;
                continue;
            }

            // Check endpoint for each item (important for Slip returns)
            let mut should_break = false;
            for item in items_to_add {
                if should_break {
                    break;
                }
                if let Some(ref epk) = g.endpoint_kind {
                    match epk {
                        EndpointKind::Closure(closure_val) => {
                            if self.sequence_endpoint_matches(
                                closure_val,
                                g.endpoint_count,
                                &result,
                                &item,
                            )? {
                                if !g.exclusive {
                                    result.push(item);
                                }
                                should_break = true;
                                continue;
                            }
                        }
                        EndpointKind::Regex(pat) => {
                            let s = item.to_string_value();
                            if self.regex_find_first(pat, &s).is_some() {
                                if !g.exclusive {
                                    result.push(item);
                                }
                                should_break = true;
                                continue;
                            }
                        }
                        EndpointKind::Value(ep) => {
                            if let ValueView::Package(type_name) = ep.view()
                                && Self::seq_type_matches(&item, &type_name.resolve())
                            {
                                if !g.exclusive {
                                    result.push(item);
                                }
                                outcome.endpoint_reached = true;
                                should_break = true;
                                continue;
                            }
                            if Self::seq_values_equal(&item, ep) {
                                if !g.exclusive {
                                    result.push(item);
                                }
                                outcome.endpoint_reached = true;
                                should_break = true;
                                continue;
                            }
                            // Check if we went past the endpoint (string comparison)
                            if let (ValueView::Str(ns), ValueView::Str(es)) =
                                (item.view(), ep.view())
                                && let SeqMode::Arithmetic(step) = &g.mode
                            {
                                if *step > 0.0 && *ns > *es {
                                    should_break = true;
                                    continue;
                                }
                                if *step < 0.0 && *ns < *es {
                                    should_break = true;
                                    continue;
                                }
                            }
                            // An exact geometric sequence can grow past i64
                            // while its endpoint is also a BigInt. Compare the
                            // values directly before the f64-only cases below;
                            // converting either side to Num loses the bound.
                            if let SeqMode::GeometricRat(num, den) = &g.mode
                                && *num > 0
                                && *den > 0
                                && let ValueView::Int(_)
                                | ValueView::BigInt(_)
                                | ValueView::Rat(..)
                                | ValueView::BigRat(..) = item.view()
                                && let ValueView::Int(_)
                                | ValueView::BigInt(_)
                                | ValueView::Rat(..)
                                | ValueView::BigRat(..) = ep.view()
                            {
                                let cmp = crate::runtime::compare_values(&item, ep);
                                if (*num > *den && cmp > 0) || (*num < *den && cmp < 0) {
                                    should_break = true;
                                    continue;
                                }
                            }
                            // Check if we went past the endpoint (numeric)
                            if let (Some(nf), Some(ef)) =
                                (Self::seq_value_to_f64(&item), Self::seq_value_to_f64(ep))
                            {
                                match &g.mode {
                                    SeqMode::Arithmetic(step) => {
                                        if *step > 0.0 && nf > ef {
                                            should_break = true;
                                            continue;
                                        }
                                        if *step < 0.0 && nf < ef {
                                            should_break = true;
                                            continue;
                                        }
                                    }
                                    SeqMode::Geometric(_) | SeqMode::GeometricRat(..) => {
                                        let ratio = match &g.mode {
                                            SeqMode::Geometric(r) => *r,
                                            SeqMode::GeometricRat(n, d) => {
                                                crate::value::rat_to_f64(*n, *d)
                                            }
                                            _ => unreachable!(),
                                        };
                                        if ratio > 0.0 {
                                            if ratio > 1.0 && nf > ef {
                                                should_break = true;
                                                continue;
                                            }
                                            if ratio > 0.0 && ratio < 1.0 && nf < ef {
                                                should_break = true;
                                                continue;
                                            }
                                        } else {
                                            let first_f = g
                                                .first_seed
                                                .as_ref()
                                                .and_then(Self::seq_value_to_f64)
                                                .map_or(1.0, f64::abs);
                                            let reachable = if first_f > 1e-15 && ef.abs() > 1e-15 {
                                                let log_val =
                                                    (ef.abs() / first_f).ln() / ratio.abs().ln();
                                                (log_val - log_val.round()).abs() < 1e-9
                                                    && log_val >= -1e-9
                                            } else {
                                                ef.abs() < 1e-15
                                            };
                                            if !reachable {
                                                if ratio.abs() > 1.0 && nf.abs() > ef.abs() {
                                                    should_break = true;
                                                    continue;
                                                }
                                                if ratio.abs() < 1.0
                                                    && ratio.abs() > 0.0
                                                    && nf.abs() < ef.abs()
                                                {
                                                    should_break = true;
                                                    continue;
                                                }
                                            }
                                        }
                                    }
                                    SeqMode::Closure => {}
                                }
                            }
                        }
                    }
                }

                result.push(item);
            }
            if should_break {
                break;
            }
            exited_early = false;
        }
        outcome.ended = exited_early;
        Ok(outcome)
    }
}
