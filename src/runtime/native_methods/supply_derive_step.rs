//! The per-value Supply combinators `skip`, `min`, `max`, `minmax`, `rotor`,
//! `encode` and `decode`: one state machine per tap (ADR-18443, #18579).
//!
//! Rakudo defines each as `supply { whenever self -> \val { ... } }` with its
//! state (the count still to skip, the running extreme, the pending batch, the
//! partial grapheme) local to the supply block, so every tap of the derived
//! supply has its own. A live derived supply keeps the state in a buffer
//! stage of [`super::supply_derive_buffer`]; a materialized one folds its
//! values through the same state ([`Interpreter::step_fold`]), so the two
//! cannot drift.

use crate::runtime::*;
use crate::value::ValueView;

/// Whether `mode` is one of this module's combinators.
// Cost: O(1).
pub(in crate::runtime) fn is_step_mode(mode: &str) -> bool {
    matches!(
        mode,
        "skip" | "min" | "max" | "minmax" | "rotor" | "encode" | "decode"
    )
}

/// The batch sizes of a `rotor`: its cycle of `elems => gap` specs, or the
/// counts `n, n + 1, ...` of a lone endless range (`rotor(1..*)`).
pub(super) enum RotorCycle {
    Specs { specs: Vec<(i64, i64)>, next: usize },
    Counting(i64),
}

impl RotorCycle {
    // Cost: O(1).
    fn next_batch(&mut self) -> (i64, i64) {
        match self {
            RotorCycle::Specs { specs, next } => {
                let spec = specs[*next % specs.len()];
                *next += 1;
                spec
            }
            RotorCycle::Counting(n) => {
                let elems = *n;
                *n += 1;
                (elems, 0)
            }
        }
    }
}

/// One tap's state.
pub(super) enum StepState {
    /// `skip(N)`: how many values are still to be dropped.
    Skip { remaining: i64 },
    /// `min(&by)` / `max(&by)`: the extreme so far.
    Extreme {
        max: bool,
        by: Value,
        cur: Option<Value>,
    },
    /// `minmax(&by)`: the extremes so far.
    MinMax {
        by: Value,
        min: Option<Value>,
        max: Option<Value>,
    },
    /// `rotor(*@cycle, :partial)`: rakudo's `@batched`, the current batch
    /// size and gap, and the values the gap still drops.
    Rotor {
        cycle: RotorCycle,
        elems: i64,
        gap: i64,
        to_skip: i64,
        skip: i64,
        batched: Vec<Value>,
        partial: bool,
    },
    /// `encode(...)`: the arguments of each value's `.encode`.
    Encode { args: Vec<Value> },
    /// `decode($encoding)`: the trailing grapheme, held back until the next
    /// chunk shows it is complete.
    Decode { encoding: String, carry: String },
}

/// The arguments `params` (as [`Interpreter::make_on_demand_buffer_supply`]
/// packed them) as a list.
// Cost: O(a), a = arguments.
fn param_list(params: &Value) -> Vec<Value> {
    match params.view() {
        ValueView::Array(items, ..) => items.to_vec(),
        _ => Vec::new(),
    }
}

/// A count or gap given as a number (or a numeric string).
// Cost: O(1).
fn spec_number(value: &Value) -> Option<i64> {
    match value.view() {
        ValueView::Int(n) => Some(n),
        ValueView::Num(n) if n.is_finite() => Some(n as i64),
        ValueView::Rat(n, d) if d != 0 => Some(n / d),
        ValueView::Str(s) => s.trim().parse::<i64>().ok(),
        _ => None,
    }
}

/// `rotor`'s positional arguments as its cycle and its `:partial`; `Err` with
/// rakudo's message for a spec that has no count (`rotor(*)`).
// Cost: O(s), s = specs, plus the elements of a finite range among them.
fn rotor_cycle(args: &[Value]) -> Result<(Vec<Value>, bool), String> {
    let mut partial = false;
    let mut specs = Vec::new();
    let mut pending: std::collections::VecDeque<Value> = args.iter().cloned().collect();
    while let Some(arg) = pending.pop_front() {
        match arg.view() {
            ValueView::Pair(key, value) if key == "partial" => partial = value.truthy(),
            ValueView::Array(items, ..) => {
                for item in items.iter().rev() {
                    pending.push_front(item.clone());
                }
            }
            ValueView::Seq(items) => {
                for item in items.iter().rev() {
                    pending.push_front(item.clone());
                }
            }
            ValueView::Range(start, end) | ValueView::RangeExcl(start, end) => {
                let end = if matches!(arg.view(), ValueView::RangeExcl(..)) && end != i64::MAX {
                    end - 1
                } else {
                    end
                };
                if end == i64::MAX {
                    // An endless range: its counts, one after another.
                    specs.push(Value::array(vec![Value::int(start), Value::str("*".to_string())]));
                    break;
                }
                for n in start..=end {
                    specs.push(Value::int(n));
                }
            }
            ValueView::Pair(..) | ValueView::ValuePair(..) => specs.push(arg.clone()),
            _ if spec_number(&arg).is_some() => specs.push(arg.clone()),
            _ => {
                return Err(format!(
                    "Cannot resolve caller Numeric({}); none of these signatures matches",
                    crate::value::type_name::value_type_name(&arg)
                ));
            }
        }
    }
    Ok((specs, partial))
}

/// The arguments of the `mode` combinator as its stage parameters: `rotor`'s
/// are its `:partial` followed by its cycle (see [`rotor_cycle`]); the others
/// are passed as given.
// Cost: O(a), a = arguments (plus a finite range's elements for `rotor`).
pub(in crate::runtime) fn step_params(mode: &str, args: Vec<Value>) -> Result<Value, RuntimeError> {
    if mode != "rotor" {
        return Ok(Value::array(args));
    }
    let (specs, partial) = rotor_cycle(&args).map_err(RuntimeError::new)?;
    let mut params = Vec::with_capacity(specs.len() + 1);
    params.push(Value::truth(partial));
    params.extend(specs);
    Ok(Value::array(params))
}

/// The state of a new tap of the `mode` combinator; `None` for a mode this
/// module does not own.
// Cost: O(a), a = the stage's arguments.
pub(super) fn step_state(mode: &str, params: &Value) -> Option<StepState> {
    let args = param_list(params);
    Some(match mode {
        "skip" => StepState::Skip {
            remaining: args.first().and_then(spec_number).unwrap_or(1),
        },
        "min" | "max" => StepState::Extreme {
            max: mode == "max",
            by: args.first().cloned().unwrap_or(Value::NIL),
            cur: None,
        },
        "minmax" => StepState::MinMax {
            by: args.first().cloned().unwrap_or(Value::NIL),
            min: None,
            max: None,
        },
        "rotor" => {
            let partial = args.first().is_some_and(Value::truthy);
            let specs: Vec<Value> = args.into_iter().skip(1).collect();
            let mut cycle = match specs.as_slice() {
                [only] if matches!(only.view(), ValueView::Array(..)) => {
                    RotorCycle::Counting(param_list(only).first().and_then(spec_number)?)
                }
                _ => RotorCycle::Specs {
                    specs: specs.iter().map(rotor_spec).collect(),
                    next: 0,
                },
            };
            if matches!(&cycle, RotorCycle::Specs { specs, .. } if specs.is_empty()) {
                return None;
            }
            let (elems, gap) = cycle.next_batch();
            StepState::Rotor {
                cycle,
                elems,
                gap,
                to_skip: gap.max(0),
                skip: 0,
                batched: Vec::new(),
                partial,
            }
        }
        "encode" => StepState::Encode { args },
        "decode" => StepState::Decode {
            encoding: args
                .first()
                .map(Value::to_string_value)
                .unwrap_or_else(|| "utf-8".to_string()),
            carry: String::new(),
        },
        _ => return None,
    })
}

/// One `rotor` spec as `(elems, gap)`.
// Cost: O(1).
fn rotor_spec(spec: &Value) -> (i64, i64) {
    match spec.view() {
        ValueView::Pair(key, value) => (
            key.trim().parse::<i64>().unwrap_or(0),
            spec_number(value).unwrap_or(0),
        ),
        ValueView::ValuePair(key, value) => (
            spec_number(key).unwrap_or(0),
            spec_number(value).unwrap_or(0),
        ),
        _ => (spec_number(spec).unwrap_or(0), 0),
    }
}

/// The source is done: what the state still holds back (a `rotor(:partial)`
/// batch, the trailing grapheme of a `decode`).
// Cost: O(b), b = values held back.
pub(super) fn step_finish(state: StepState) -> Vec<Value> {
    match state {
        StepState::Rotor {
            batched, partial, ..
        } if partial && !batched.is_empty() => vec![Value::real_array(batched)],
        StepState::Decode { carry, .. } if !carry.is_empty() => vec![Value::str(carry)],
        _ => Vec::new(),
    }
}

/// Keep every value the GC must see in a tap's state.
// Cost: O(v), v = values held.
pub(super) fn visit_step_roots(state: &StepState, visitor: &mut dyn crate::gc::RootVisitor) {
    match state {
        StepState::Extreme { by, cur, .. } => {
            visitor.visit_value(by);
            cur.iter().for_each(|v| visitor.visit_value(v));
        }
        StepState::MinMax { by, min, max } => {
            visitor.visit_value(by);
            min.iter().chain(max.iter()).for_each(|v| visitor.visit_value(v));
        }
        StepState::Rotor { batched, .. } => batched.iter().for_each(|v| visitor.visit_value(v)),
        StepState::Encode { args } => args.iter().for_each(|v| visitor.visit_value(v)),
        StepState::Skip { .. } | StepState::Decode { .. } => {}
    }
}

impl Interpreter {
    /// The per-tap recipe of the per-value combinator `name` (or of
    /// `repeated`, `stable`, `migrate`) over the live `source`; `None` for any
    /// other combinator.
    // Cost: O(a), a = the arguments.
    pub(in crate::runtime) fn per_value_recipe(
        &mut self,
        source: &Value,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if name == "repeated" {
            return Some(Ok(Self::make_on_demand_repeated_supply(
                source.clone(),
                Self::named_value(args, "as"),
                Self::named_value(args, "with"),
            )));
        }
        // `stable` (a positive window) and `migrate` over a recipe: the
        // existing live stage, built once per tap (`supply_derive_eager`).
        let positive_window = args.first().is_some_and(|s| s.to_f64() > 0.0);
        let recipe = matches!(source.view(), ValueView::Instance { attributes, .. }
            if attributes.as_map().contains_key(Self::LIVE_RECIPE_KEY));
        let live_throttle =
            name == "throttle" && crate::runtime::supply_throttle_live::throttle_is_live_stage(args);
        if (name == "stable" && positive_window) || (name == "migrate" && recipe) || live_throttle {
            return Some(Ok(Self::make_eager_stage_supply(source.clone(), name, args.to_vec())));
        }
        if !is_step_mode(name) {
            return None;
        }
        // `min` / `max` / `minmax` take only a code block, as on a finite
        // supply.
        if matches!(name, "min" | "max" | "minmax")
            && args.first().is_some_and(|by| {
                !matches!(
                    by.view(),
                    ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
                )
            })
        {
            return Some(Err(RuntimeError::new(format!(
                "Supply.{name}: mapper must be code if specified"
            ))));
        }
        Some(
            step_params(name, args.to_vec())
                .map(|params| Self::make_on_demand_buffer_supply(source.clone(), name, params)),
        )
    }

    /// `cmp(a, b)` of a `min` / `max` / `minmax` stage: `&by` itself when it
    /// takes two arguments, else `by(a) cmp by(b)`, else plain `cmp`.
    // Cost: up to two calls of `&by`.
    fn step_cmp(&mut self, by: &Value, a: &Value, b: &Value) -> Result<std::cmp::Ordering, RuntimeError> {
        if by.is_nil() {
            return Ok(compare_values(a, b).cmp(&0));
        }
        if self.extrema_callable_arity(by) >= 2 {
            let order = self.call_supply_callback(by.clone(), vec![a.clone(), b.clone()])?;
            return Ok(crate::runtime::methods_collection_ops::sort::sort_result_to_ordering(&order));
        }
        let ka = self.call_supply_callback(by.clone(), vec![a.clone()])?;
        let kb = self.call_supply_callback(by.clone(), vec![b.clone()])?;
        Ok(compare_values(&ka, &kb).cmp(&0))
    }

    /// One value through a tap's state: what the stage emits for it.
    // Cost: O(1) amortized (a `rotor` flush is O(b), b = the batch), plus
    // the stage's callables (`&by`, `.encode`) and an O(c) `decode` of a
    // c-byte chunk.
    pub(super) fn step_emit(&mut self, state: &mut StepState, value: Value) -> Result<Vec<Value>, RuntimeError> {
        match state {
            StepState::Skip { remaining } => {
                if *remaining > 0 {
                    *remaining -= 1;
                    return Ok(Vec::new());
                }
                Ok(vec![value])
            }
            StepState::Extreme { max, by, cur } => {
                if !crate::runtime::types::value_is_defined(&value) {
                    return Ok(Vec::new());
                }
                let better = match cur {
                    None => true,
                    Some(cur) => {
                        let order = self.step_cmp(&by.clone(), &value, &cur.clone())?;
                        if *max {
                            order == std::cmp::Ordering::Greater
                        } else {
                            order == std::cmp::Ordering::Less
                        }
                    }
                };
                if !better {
                    return Ok(Vec::new());
                }
                *cur = Some(value.clone());
                Ok(vec![value])
            }
            StepState::MinMax { by, min, max } => {
                // rakudo: `val.throw` for a Failure.
                if let Some(err) = Self::failure_exception_from_value(&value) {
                    return Err(RuntimeError::from_exception_value(err));
                }
                if !crate::runtime::types::value_is_defined(&value) {
                    return Ok(Vec::new());
                }
                let by = by.clone();
                match (min.clone(), max.clone()) {
                    (Some(lo), Some(hi)) => {
                        if self.step_cmp(&by, &value, &lo)? == std::cmp::Ordering::Less {
                            *min = Some(value);
                        } else if self.step_cmp(&by, &value, &hi)? == std::cmp::Ordering::Greater {
                            *max = Some(value);
                        } else {
                            return Ok(Vec::new());
                        }
                    }
                    _ => {
                        *min = Some(value.clone());
                        *max = Some(value);
                    }
                }
                let (lo, hi) = (min.clone().unwrap_or(Value::NIL), max.clone().unwrap_or(Value::NIL));
                Ok(vec![match (lo.view(), hi.view()) {
                    (ValueView::Int(a), ValueView::Int(b)) => {
                        crate::builtins::arith::range::int_range(a, b, false, false)
                    }
                    _ => Value::generic_range(lo, hi, false, false),
                }])
            }
            StepState::Rotor {
                cycle,
                elems,
                gap,
                to_skip,
                skip,
                batched,
                ..
            } => {
                if *skip > 0 {
                    *skip -= 1;
                } else {
                    batched.push(value);
                }
                if batched.len() as i64 != *elems {
                    return Ok(Vec::new());
                }
                // rakudo: `emit @batched.splice(0, +@batched,
                // @batched[* + $gap .. *])` -- a negative gap keeps the
                // overlap for the next batch.
                let keep_from = batched.len() as i64 + *gap;
                if keep_from < 0 {
                    return Err(RuntimeError::new(format!(
                        "Effective index out of range. Is: {keep_from}, should be in 0..^Inf"
                    )));
                }
                let kept = batched.get(keep_from as usize..).map(<[Value]>::to_vec).unwrap_or_default();
                let out = std::mem::replace(batched, kept);
                *skip = *to_skip;
                let (next_elems, next_gap) = cycle.next_batch();
                *elems = next_elems;
                *gap = next_gap;
                *to_skip = next_gap.max(0);
                Ok(vec![Value::real_array(out)])
            }
            StepState::Encode { args } => {
                let args = args.clone();
                Ok(vec![self.call_method_with_values(value, "encode", args)?])
            }
            StepState::Decode { encoding, carry } => {
                let bytes = self.supply_chunk_to_bytes(&value, encoding);
                let decoded = self.decode_bytes_for_supply(&bytes, encoding)?;
                let normalized = self.translate_newlines_for_decode_native(&decoded);
                let mut merged = std::mem::take(carry);
                merged.push_str(&normalized);
                // The last grapheme may still combine with the next chunk.
                use crate::grapheme_break::UnicodeSegmentation;
                let Some((last_start, _)) = merged.grapheme_indices(true).last() else {
                    return Ok(Vec::new());
                };
                *carry = merged[last_start..].to_string();
                merged.truncate(last_start);
                Ok(if merged.is_empty() {
                    Vec::new()
                } else {
                    vec![Value::str(merged)]
                })
            }
        }
    }

    /// The `mode` combinator over a materialized supply's `values`: the
    /// values fold through one tap's state, so a finite supply and a live
    /// one answer alike.
    // Cost: O(v) steps, v = values (each as in [`Self::step_emit`]).
    pub(in crate::runtime) fn step_fold(
        &mut self,
        mode: &str,
        args: Vec<Value>,
        values: Vec<Value>,
    ) -> Result<Vec<Value>, RuntimeError> {
        let Some(mut state) = step_state(mode, &step_params(mode, args)?) else {
            return Ok(Vec::new());
        };
        let mut out = Vec::new();
        for value in values {
            out.extend(self.step_emit(&mut state, value)?);
        }
        out.extend(step_finish(state));
        Ok(out)
    }

    /// A stage's callable (or its own check) died: quit the stage, as
    /// `map`'s does (#18473), and report it stopped.
    // Cost: O(1) plus the downstream quit.
    pub(super) fn buffer_stage_quit(&mut self, id: u64, err: RuntimeError, emitter: &Value) -> Result<bool, RuntimeError> {
        super::supply_derive_buffer::buffer_retire(id);
        let reason = Self::supply_stage_quit_reason(err)?;
        self.call_method_with_values(emitter.clone(), "quit", vec![reason])?;
        Ok(true)
    }
}
