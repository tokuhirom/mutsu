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
        let encoding_val = args.first().cloned().unwrap_or(Value::Nil);
        // The encoding is a type object (Package) or class instance.
        // We need to call .name and .alternative-names on it to get registration info.
        let enc_name = self.call_method_with_values(encoding_val.clone(), "name", vec![])?;
        let enc_name_str = enc_name.to_string_value();

        let alt_names_val = self
            .call_method_with_values(encoding_val.clone(), "alternative-names", vec![])
            .unwrap_or(Value::array(Vec::new()));
        let alt_names: Vec<String> = match alt_names_val {
            Value::Array(items, ..) => items.iter().map(|v| v.to_string_value()).collect(),
            Value::Slip(items) => items.iter().map(|v| v.to_string_value()).collect(),
            _ => Vec::new(),
        };

        let entry = super::super::EncodingEntry {
            name: enc_name_str.clone(),
            alternative_names: alt_names,
            user_type: Some(encoding_val),
        };

        match self.register_encoding(entry) {
            Ok(()) => Ok(Value::Nil),
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
            match arg {
                Value::Pair(key, val) if key == "partial" => {
                    partial = val.truthy();
                }
                _ => positional_args.push(arg.clone()),
            }
        }

        // Build spec list from positional args
        // If single arg is a list/array, use its elements as specs
        let specs = if positional_args.len() == 1 {
            match &positional_args[0] {
                Value::Array(items, ..) => items.to_vec(),
                Value::Seq(items) => items.to_vec(),
                Value::LazyList(_) => Self::value_to_list(&positional_args[0]),
                other => vec![other.clone()],
            }
        } else {
            positional_args
        };

        // Parse each spec into (count, gap) pairs
        // count can be: Int, Whatever (*), Inf, Range
        // gap is from Pair's value
        struct RotorSpec {
            count: RotorCount,
            gap: i64,
        }
        #[derive(Clone)]
        enum RotorCount {
            Fixed(usize),
            Whatever,        // * — take everything remaining
            Inf,             // Inf — take everything remaining
            Range(Vec<i64>), // 1..* or similar — cycling counts
        }

        // Flatten any nested Seq/Array specs into a flat list
        let mut flat_specs: Vec<Value> = Vec::new();
        let mut to_process: std::collections::VecDeque<Value> = specs.into();
        while let Some(spec) = to_process.pop_front() {
            match &spec {
                Value::Seq(items) => {
                    for item in items.iter() {
                        to_process.push_back(item.clone());
                    }
                }
                Value::Array(items, ..) => {
                    for item in items.iter() {
                        to_process.push_back(item.clone());
                    }
                }
                _ => flat_specs.push(spec),
            }
        }

        let mut rotor_specs: Vec<RotorSpec> = Vec::new();
        for spec in &flat_specs {
            match spec {
                Value::Int(n) => {
                    let count = *n;
                    if count < 0 {
                        let mut attrs = HashMap::new();
                        attrs.insert("got".to_string(), Value::Int(count));
                        attrs.insert(
                            "message".to_string(),
                            Value::str(format!(
                                "Expected a non-negative integer for rotor count, got {}",
                                count
                            )),
                        );
                        let ex = Value::make_instance(Symbol::intern("X::OutOfRange"), attrs);
                        let mut err = RuntimeError::new(format!(
                            "X::OutOfRange: Expected non-negative count, got {}",
                            count
                        ));
                        err.exception = Some(Box::new(ex));
                        return Err(err);
                    }
                    rotor_specs.push(RotorSpec {
                        count: RotorCount::Fixed(count as usize),
                        gap: 0,
                    });
                }
                Value::Whatever => {
                    rotor_specs.push(RotorSpec {
                        count: RotorCount::Inf,
                        gap: 0,
                    });
                }
                Value::Num(n) if n.is_infinite() && n.is_sign_positive() => {
                    rotor_specs.push(RotorSpec {
                        count: RotorCount::Inf,
                        gap: 0,
                    });
                }
                Value::Num(n) => {
                    rotor_specs.push(RotorSpec {
                        count: RotorCount::Fixed(*n as usize),
                        gap: 0,
                    });
                }
                Value::Rat(n, d) => {
                    let count = if *d != 0 { n / d } else { 0 };
                    rotor_specs.push(RotorSpec {
                        count: RotorCount::Fixed(count as usize),
                        gap: 0,
                    });
                }
                Value::Pair(..) | Value::ValuePair(..) => {
                    let (count_val, gap_val) = match spec {
                        Value::Pair(k, v) => (Value::str(k.clone()), v.as_ref().clone()),
                        Value::ValuePair(k, v) => (k.as_ref().clone(), v.as_ref().clone()),
                        _ => unreachable!(),
                    };
                    let count = match &count_val {
                        Value::Int(n) => RotorCount::Fixed(*n as usize),
                        Value::Num(n) => RotorCount::Fixed(*n as usize),
                        Value::Rat(n, d) => {
                            RotorCount::Fixed(if *d != 0 { (n / d) as usize } else { 0 })
                        }
                        Value::Str(s) => RotorCount::Fixed(s.parse::<i64>().unwrap_or(0) as usize),
                        _ => RotorCount::Fixed(0),
                    };
                    let gap = match &gap_val {
                        Value::Int(n) => *n,
                        Value::Num(n) => *n as i64,
                        Value::Rat(n, d) => {
                            if *d != 0 {
                                n / d
                            } else {
                                0
                            }
                        }
                        _ => 0,
                    };
                    rotor_specs.push(RotorSpec { count, gap });
                }
                Value::HyperWhatever => {
                    rotor_specs.push(RotorSpec {
                        count: RotorCount::Whatever,
                        gap: 0,
                    });
                }
                Value::Range(start, end) | Value::RangeExcl(start, end) => {
                    let is_excl = matches!(spec, Value::RangeExcl(..));
                    let end_val = if is_excl { *end - 1 } else { *end };
                    if end_val == i64::MAX || *end == i64::MAX {
                        // 1..* — infinite range, treated like cycling counts
                        let mut counts = Vec::new();
                        for i in *start.. {
                            counts.push(i);
                            if counts.len() > 10000 {
                                break; // safety limit
                            }
                        }
                        rotor_specs.push(RotorSpec {
                            count: RotorCount::Range(counts),
                            gap: 0,
                        });
                    } else {
                        let mut counts = Vec::new();
                        for i in *start..=end_val {
                            counts.push(i);
                        }
                        rotor_specs.push(RotorSpec {
                            count: RotorCount::Range(counts),
                            gap: 0,
                        });
                    }
                }
                _ => {
                    // Try to coerce to int
                    if let Some(n) = to_float_value(spec) {
                        if n.is_infinite() && n.is_sign_positive() {
                            rotor_specs.push(RotorSpec {
                                count: RotorCount::Inf,
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
            return Ok(Value::Seq(Arc::new(Vec::new())));
        }

        // Get the items to rotor over (force LazyList if needed)
        let target = if let Value::LazyList(ll) = &target {
            Value::array(self.force_lazy_list_bridge(ll)?)
        } else {
            target
        };
        let items = if crate::runtime::utils::is_shaped_array(&target) {
            crate::runtime::utils::shaped_array_leaves(&target)
        } else {
            Self::value_to_list(&target)
        };

        if items.is_empty() {
            return Ok(Value::Seq(Arc::new(Vec::new())));
        }

        let mut result: Vec<Value> = Vec::new();
        let mut pos = 0usize;
        let mut spec_idx = 0usize;
        let mut range_sub_idx = 0usize; // for Range specs

        loop {
            if pos >= items.len() {
                break;
            }

            let spec = &rotor_specs[spec_idx % rotor_specs.len()];

            let count = match &spec.count {
                RotorCount::Fixed(n) => *n,
                RotorCount::Whatever | RotorCount::Inf => items.len() - pos,
                RotorCount::Range(counts) => {
                    let c = counts[range_sub_idx % counts.len()];
                    if c == i64::MAX {
                        items.len() - pos
                    } else {
                        c as usize
                    }
                }
            };

            let gap = spec.gap;

            // Take `count` items starting at pos
            let end = std::cmp::min(pos.saturating_add(count), items.len());
            let chunk_len = end - pos;
            let chunk: Vec<Value> = items[pos..end].to_vec();

            if chunk_len == count || (partial && (!chunk.is_empty() || count == 0)) {
                // When gap is negative and chunk is partial (not first chunk),
                // only emit if the chunk has enough elements to contain at least
                // one new element not already covered by the previous chunk's overlap.
                let skip_partial =
                    chunk_len < count && gap < 0 && !result.is_empty() && (chunk_len as i64) < -gap;
                if !skip_partial {
                    result.push(Value::array(chunk));
                }
            }

            if chunk_len < count {
                break;
            }

            // Advance position: count + gap (gap can be negative for overlap)
            let new_pos = (pos as i64).saturating_add((count as i64).saturating_add(gap));
            if new_pos < 0 {
                // Negative gap past start of list
                let mut attrs = HashMap::new();
                attrs.insert("got".to_string(), Value::Int(new_pos));
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
            pos = new_pos as usize;

            // Advance spec index
            match &spec.count {
                RotorCount::Range(counts) => {
                    range_sub_idx += 1;
                    if range_sub_idx >= counts.len() {
                        range_sub_idx = 0;
                        spec_idx += 1;
                    }
                }
                _ => {
                    spec_idx += 1;
                }
            }
        }

        Ok(Value::Seq(Arc::new(result)))
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
            match arg {
                Value::Pair(key, value) if key == "off" => {
                    start_off = value.truthy();
                }
                _ => {
                    conditions.push(arg.clone());
                }
            }
        }

        // Get items to iterate over
        let items = match &target {
            // Non-iterable scalar: treat as a single-element list
            Value::Int(_)
            | Value::Num(_)
            | Value::Str(_)
            | Value::Bool(_)
            | Value::Rat(..)
            | Value::FatRat(..)
            | Value::BigInt(_)
            | Value::BigRat(..)
            | Value::Complex(..)
            | Value::Nil => vec![target.clone()],
            _ => crate::runtime::utils::value_to_list(&target),
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

        Ok(Value::Seq(Arc::new(result)))
    }
}
