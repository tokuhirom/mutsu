use super::*;

impl Interpreter {
    pub(in crate::runtime) fn dispatch_encoding_registry_find(
        &self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let name = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        if let Some(entry) = self.find_encoding(&name) {
            if let Some(ref user_type) = entry.user_type {
                // User-registered encoding: return an instance of the user's type
                return Ok(user_type.clone());
            }
            // Built-in encoding: create an Encoding::Builtin instance
            let mut attrs = HashMap::new();
            attrs.insert("name".to_string(), Value::str(entry.name.clone()));
            let alt_names: Vec<Value> = entry
                .alternative_names
                .iter()
                .map(|s| Value::str(s.clone()))
                .collect();
            attrs.insert("alternative-names".to_string(), Value::array(alt_names));
            Ok(Value::make_instance(
                Symbol::intern("Encoding::Builtin"),
                attrs,
            ))
        } else {
            // Throw X::Encoding::Unknown
            let mut ex_attrs = HashMap::new();
            ex_attrs.insert("name".to_string(), Value::str(name.clone()));
            let ex = Value::make_instance(Symbol::intern("X::Encoding::Unknown"), ex_attrs);
            let mut err = RuntimeError::new(format!("Unknown encoding '{}'", name));
            err.exception = Some(Box::new(ex));
            Err(err)
        }
    }

    pub(in crate::runtime) fn dispatch_encoding_registry_register(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let encoding_val = args.first().cloned().unwrap_or(Value::NIL);
        // The encoding is a type object (Package) or class instance.
        // We need to call .name and .alternative-names on it to get registration info.
        let enc_name = self.call_method_with_values(encoding_val.clone(), "name", vec![])?;
        let enc_name_str = enc_name.to_string_value();

        let alt_names_val = self
            .call_method_with_values(encoding_val.clone(), "alternative-names", vec![])
            .unwrap_or(Value::array(Vec::new()));
        let alt_names: Vec<String> = match alt_names_val.view() {
            ValueView::Array(items, ..) => items.iter().map(|v| v.to_string_value()).collect(),
            ValueView::Slip(items) => items.iter().map(|v| v.to_string_value()).collect(),
            _ => Vec::new(),
        };

        let entry = super::super::EncodingEntry {
            name: enc_name_str,
            alternative_names: alt_names,
            user_type: Some(encoding_val),
        };

        match self.register_encoding(entry) {
            Ok(()) => Ok(Value::NIL),
            Err(conflicting_name) => {
                let mut ex_attrs = HashMap::new();
                ex_attrs.insert("name".to_string(), Value::str(conflicting_name.clone()));
                let ex = Value::make_instance(
                    Symbol::intern("X::Encoding::AlreadyRegistered"),
                    ex_attrs,
                );
                let mut err = RuntimeError::new(format!(
                    "Encoding '{}' is already registered",
                    conflicting_name
                ));
                err.exception = Some(Box::new(ex));
                Err(err)
            }
        }
    }

    /// The `X::OutOfRange` raku throws for a `rotor`/`batch` sublist length
    /// outside `1..^Inf`. A length of 0 (or negative) is rejected eagerly because
    /// a zero-length batch never advances the cursor and would loop forever.
    fn rotor_count_out_of_range(count: i64) -> RuntimeError {
        let msg =
            format!("Batching sublist length is out of range. Is: {count}, should be in 1..^Inf");
        let mut attrs = HashMap::new();
        attrs.insert("got".to_string(), Value::int(count));
        attrs.insert("range".to_string(), Value::str("1..^Inf".to_string()));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        let ex = Value::make_instance(Symbol::intern("X::OutOfRange"), attrs);
        let mut err = RuntimeError::new(format!("X::OutOfRange: {msg}"));
        err.exception = Some(Box::new(ex));
        err
    }

    /// Cost: O(1) per call on a non-shaped Array whose specs cannot step before
    /// the start (a lazy Seq, `ListGen::Rotor`), then O(n) per sublist of n
    /// pulled; otherwise O(e + s), e = elements of the invocant (decomposed
    /// eagerly), s = total elements of the produced sublists. A lazy invocant
    /// throws X::Cannot::Lazy before reaching here.
    pub(in crate::runtime) fn dispatch_rotor(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        use crate::runtime::utils::to_float_value;

        // Extract :partial named arg
        let mut partial = false;
        let mut positional_args: Vec<Value> = Vec::new();
        for arg in args {
            match arg.view() {
                ValueView::Pair(key, val) if key == "partial" => {
                    partial = val.truthy();
                }
                _ => positional_args.push(arg.clone()),
            }
        }

        // Build spec list from positional args
        // If single arg is a list/array, use its elements as specs
        let specs = if positional_args.len() == 1 {
            match positional_args[0].view() {
                ValueView::Array(items, ..) => items.to_vec(),
                ValueView::Seq(items) => items.to_vec(),
                ValueView::LazyList(_) => Self::value_to_list(&positional_args[0]),
                _ => vec![positional_args[0].clone()],
            }
        } else {
            positional_args
        };

        // Parse each spec into (count, gap) pairs
        // count can be: Int, Whatever (*), Inf, Range
        // gap is from Pair's value
        use crate::value::list_gen_rotor::{RotorCount, RotorSpec, RotorState};

        // Flatten any nested Seq/Array specs into a flat list
        let mut flat_specs: Vec<Value> = Vec::new();
        let mut to_process: std::collections::VecDeque<Value> = specs.into();
        while let Some(spec) = to_process.pop_front() {
            let nested = match spec.view() {
                ValueView::Seq(items) => Some(items.to_vec()),
                ValueView::Array(items, ..) => Some(items.to_vec()),
                _ => None,
            };
            match nested {
                Some(items) => {
                    for item in items {
                        to_process.push_back(item);
                    }
                }
                None => flat_specs.push(spec),
            }
        }

        let mut rotor_specs: Vec<RotorSpec> = Vec::new();
        for spec in &flat_specs {
            match spec.view() {
                ValueView::Int(n) => {
                    let count = n;
                    // A negative batching sublist length is out of range. A length
                    // of 0 is allowed: it yields an empty sublist `()` (roast
                    // S32-list/rotor.t exercises `(0..5).rotor(0, 1, *)`). The
                    // infinite-loop case (a full spec cycle that never advances the
                    // cursor, e.g. a lone `rotor(0)`) is caught in the loop below.
                    if count < 0 {
                        return Err(Self::rotor_count_out_of_range(count));
                    }
                    rotor_specs.push(RotorSpec {
                        count: RotorCount::Fixed(count as usize),
                        gap: 0,
                    });
                }
                ValueView::Whatever => {
                    rotor_specs.push(RotorSpec {
                        count: RotorCount::Rest,
                        gap: 0,
                    });
                }
                ValueView::Num(n) if n.is_infinite() && n.is_sign_positive() => {
                    rotor_specs.push(RotorSpec {
                        count: RotorCount::Rest,
                        gap: 0,
                    });
                }
                ValueView::Num(n) => {
                    rotor_specs.push(RotorSpec {
                        count: RotorCount::Fixed(n as usize),
                        gap: 0,
                    });
                }
                ValueView::Rat(n, d) => {
                    let count = if d != 0 { n / d } else { 0 };
                    rotor_specs.push(RotorSpec {
                        count: RotorCount::Fixed(count as usize),
                        gap: 0,
                    });
                }
                ValueView::Pair(..) | ValueView::ValuePair(..) => {
                    let (count_val, gap_val) = match spec.view() {
                        ValueView::Pair(k, v) => (Value::str(k.clone()), v.clone()),
                        ValueView::ValuePair(k, v) => (k.clone(), v.clone()),
                        _ => unreachable!(),
                    };
                    let count = match count_val.view() {
                        ValueView::Int(n) => RotorCount::Fixed(n as usize),
                        ValueView::Num(n) => RotorCount::Fixed(n as usize),
                        ValueView::Rat(n, d) => {
                            RotorCount::Fixed(if d != 0 { (n / d) as usize } else { 0 })
                        }
                        ValueView::Str(s) => {
                            RotorCount::Fixed(s.parse::<i64>().unwrap_or(0) as usize)
                        }
                        _ => RotorCount::Fixed(0),
                    };
                    let gap = match gap_val.view() {
                        ValueView::Int(n) => n,
                        ValueView::Num(n) => n as i64,
                        ValueView::Rat(n, d) if d != 0 => n / d,
                        ValueView::Rat(..) => 0,
                        _ => 0,
                    };
                    rotor_specs.push(RotorSpec { count, gap });
                }
                ValueView::HyperWhatever => {
                    rotor_specs.push(RotorSpec {
                        count: RotorCount::Rest,
                        gap: 0,
                    });
                }
                ValueView::Range(start, end) | ValueView::RangeExcl(start, end) => {
                    let is_excl = matches!(spec.view(), ValueView::RangeExcl(..));
                    let end_val = if is_excl { end - 1 } else { end };
                    // `1..*` counts up forever; a finite range cycles its
                    // counts. Either way the next count is computed, never
                    // pre-expanded.
                    let len = if end_val == i64::MAX || end == i64::MAX {
                        None
                    } else {
                        Some(if end_val >= start {
                            (end_val - start) as usize + 1
                        } else {
                            0
                        })
                    };
                    rotor_specs.push(RotorSpec {
                        count: RotorCount::Range { start, len },
                        gap: 0,
                    });
                }
                _ => {
                    // Try to coerce to int
                    if let Some(n) = to_float_value(spec) {
                        if n.is_infinite() && n.is_sign_positive() {
                            rotor_specs.push(RotorSpec {
                                count: RotorCount::Rest,
                                gap: 0,
                            });
                        } else {
                            rotor_specs.push(RotorSpec {
                                count: RotorCount::Fixed(n as usize),
                                gap: 0,
                            });
                        }
                    }
                }
            }
        }

        if rotor_specs.is_empty() {
            return Ok(Value::seq(Vec::new()));
        }

        // Get the items to rotor over (force LazyList if needed)
        let lazy_forced = if let ValueView::LazyList(ll) = target.view() {
            Some(self.force_lazy_list_bridge(&ll)?)
        } else {
            None
        };
        let target = match lazy_forced {
            Some(items) => Value::array(items),
            None => target,
        };
        // A (non-shaped) Array rotors lazily over its live elements, as
        // Rakudo's does -- unless a negative gap could step before the start
        // of the list, which throws mid-iteration and a pure iterator cannot.
        if !RotorState::can_underflow(&rotor_specs) {
            let items = crate::value::MapGrepItems::of(&target, Vec::new);
            if matches!(items, crate::value::MapGrepItems::Live(_)) {
                return Ok(Value::seq_list_gen(
                    crate::value::ListGen::rotor(items, RotorState::new(rotor_specs, partial)),
                    false,
                ));
            }
        }
        let items = if crate::runtime::utils::is_shaped_array(&target) {
            crate::runtime::utils::shaped_array_leaves(&target)
        } else {
            Self::value_to_list(&target)
        };
        let mut state = RotorState::new(rotor_specs, partial);
        let mut result: Vec<Value> = Vec::new();
        loop {
            match state.step(&items) {
                Ok(Some(chunk)) => result.push(chunk),
                Ok(None) => break,
                Err(underflow) => {
                    // Negative gap past start of list
                    let mut attrs = HashMap::new();
                    attrs.insert("got".to_string(), Value::int(underflow.new_pos));
                    attrs.insert(
                        "message".to_string(),
                        Value::str(
                            "Rotoring gap is too large and causes an index below zero".to_string(),
                        ),
                    );
                    let ex = Value::make_instance(Symbol::intern("X::OutOfRange"), attrs);
                    let mut err =
                        RuntimeError::new("X::OutOfRange: Rotoring gap is too large".to_string());
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                }
            }
        }
        Ok(Value::seq(result))
    }

    /// Implements the `.toggle` method.
    ///
    /// method toggle(*@conditions, Bool :$off --> Seq)
    ///
    /// Iterates over the invocant, toggling whether values are emitted based
    /// on Callable conditions. The switch starts "on" (unless :off is given).
    /// Each value is tested by the current condition; the switch is set to the
    /// result. When the switch toggles (changes state), the next condition is
    /// used. Values are emitted when the switch is "on".
    pub(in crate::runtime) fn dispatch_toggle(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Extract :off named arg and collect positional callable args
        let mut start_off = false;
        let mut conditions: Vec<Value> = Vec::new();

        for arg in args {
            match arg.view() {
                ValueView::Pair(key, value) if key == "off" => {
                    start_off = value.truthy();
                }
                _ => {
                    conditions.push(arg.clone());
                }
            }
        }

        // Get items to iterate over
        let items = match target.view() {
            // Non-iterable scalar: treat as a single-element list
            ValueView::Int(_)
            | ValueView::Num(_)
            | ValueView::Str(_)
            | ValueView::Bool(_)
            | ValueView::Rat(..)
            | ValueView::FatRat(..)
            | ValueView::BigInt(_)
            | ValueView::BigRat(..)
            | ValueView::Complex(..)
            | ValueView::Nil => vec![target.clone()],
            // ADR-0040 slices 1-2: `target` is `.toggle`'s own RECEIVER, so it
            // is decomposed into ITS elements — a question the itemization it
            // may carry as an element of some other container has no say in.
            // (`my @t = %(),; for @t -> \v { v.toggle }` must yield the empty
            // Seq, not a one-element Seq holding the itemized empty hash.)
            _ => crate::runtime::utils::value_to_list_for_receiver(&target),
        };

        let mut result: Vec<Value> = Vec::new();
        let mut switch_on = !start_off;
        let mut cond_idx: usize = 0;

        for item in &items {
            if cond_idx < conditions.len() {
                let tester = &conditions[cond_idx];
                let test_result = self
                    .call_sub_value(tester.clone(), vec![item.clone()], true)?
                    .truthy();

                let old_on = switch_on;
                switch_on = test_result;

                // If the switch toggled, advance to the next condition
                if switch_on != old_on {
                    cond_idx += 1;
                }
            }
            // No more conditions: switch stays in its current state

            if switch_on {
                result.push(item.clone());
            }
        }

        Ok(Value::seq(result))
    }
}
