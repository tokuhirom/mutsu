use super::*;

impl Interpreter {
    pub(super) fn eval_sequence(
        &mut self,
        left: Value,
        right: Value,
        exclusive: bool,
    ) -> Result<Value, RuntimeError> {
        let seeds_raw = Self::value_to_list(&left);
        if seeds_raw.is_empty() {
            return Ok(Value::Array(vec![]));
        }

        // Separate seed values from generator closure
        let mut seeds: Vec<Value> = Vec::new();
        let mut generator: Option<Value> = None;
        for v in &seeds_raw {
            if matches!(v, Value::Sub { .. }) {
                generator = Some(v.clone());
            } else {
                seeds.push(v.clone());
            }
        }
        if seeds.is_empty() {
            // All items were closures; use the closure as a zero-arg generator
            // e.g. { 3+2 } ... *
            // treat as if seeds = [] and generator produces values from scratch
        }

        // Parse endpoint: could be scalar, Inf/Whatever (None), closure (predicate),
        // regex, or array (first is limit, rest are extra)
        enum EndpointKind {
            Value(Value),
            Closure(Value),
            Regex(String),
        }
        let (endpoint, endpoint_kind, extra_rhs) = match &right {
            Value::Num(f) if f.is_infinite() => (None, None, vec![]),
            Value::Array(items) => {
                if items.is_empty() {
                    return Err(RuntimeError::new(
                        "Cannot use an empty list as endpoint of a sequence".to_string(),
                    ));
                }
                let first = &items[0];
                let rest: Vec<Value> = items[1..].to_vec();
                match first {
                    Value::Num(f) if f.is_infinite() => (None, None, rest),
                    Value::Sub { .. } => (
                        Some(first.clone()),
                        Some(EndpointKind::Closure(first.clone())),
                        rest,
                    ),
                    Value::Regex(pat) => (
                        Some(first.clone()),
                        Some(EndpointKind::Regex(pat.clone())),
                        rest,
                    ),
                    _ => (
                        Some(first.clone()),
                        Some(EndpointKind::Value(first.clone())),
                        rest,
                    ),
                }
            }
            Value::Sub { .. } => (
                Some(right.clone()),
                Some(EndpointKind::Closure(right.clone())),
                vec![],
            ),
            Value::Regex(pat) => (
                Some(right.clone()),
                Some(EndpointKind::Regex(pat.clone())),
                vec![],
            ),
            other => (
                Some(other.clone()),
                Some(EndpointKind::Value(other.clone())),
                vec![],
            ),
        };

        // Determine generation mode
        enum SeqMode {
            Arithmetic(f64),
            Geometric(f64),
            Closure,
        }

        let mode = if generator.is_some() {
            SeqMode::Closure
        } else if seeds.len() >= 2 {
            // Check if all seeds are numeric
            let floats: Vec<f64> = seeds.iter().filter_map(Self::seq_value_to_f64).collect();
            if floats.len() == seeds.len() {
                // Use the last few elements to determine pattern
                // First check: are ALL diffs equal? (pure arithmetic)
                let diffs: Vec<f64> = floats.windows(2).map(|w| w[1] - w[0]).collect();
                let all_diffs_equal = diffs.windows(2).all(|w| (w[0] - w[1]).abs() < 1e-12);

                if all_diffs_equal {
                    SeqMode::Arithmetic(diffs[diffs.len() - 1])
                } else if floats.len() >= 3 {
                    // Check if last 3 values form a geometric progression
                    let last3 = &floats[floats.len() - 3..];
                    let r1 = if last3[0].abs() > 1e-15 {
                        last3[1] / last3[0]
                    } else {
                        f64::NAN
                    };
                    let r2 = if last3[1].abs() > 1e-15 {
                        last3[2] / last3[1]
                    } else {
                        f64::NAN
                    };
                    if !r1.is_nan() && !r2.is_nan() && (r1 - r2).abs() < 1e-12 {
                        SeqMode::Geometric(r2)
                    } else {
                        // Use last difference as arithmetic step
                        SeqMode::Arithmetic(diffs[diffs.len() - 1])
                    }
                } else {
                    SeqMode::Arithmetic(diffs[diffs.len() - 1])
                }
            } else {
                // String constant sequences: check if all seeds are equal strings
                let all_str = seeds.iter().all(|v| matches!(v, Value::Str(_)));
                if all_str && seeds.len() >= 2 {
                    let all_same = seeds.windows(2).all(|w| {
                        if let (Value::Str(a), Value::Str(b)) = (&w[0], &w[1]) {
                            a == b
                        } else {
                            false
                        }
                    });
                    if all_same {
                        SeqMode::Arithmetic(0.0) // constant string sequence
                    } else {
                        SeqMode::Arithmetic(1.0) // default
                    }
                } else {
                    SeqMode::Arithmetic(1.0)
                }
            }
        } else if seeds.len() == 1 {
            // Single seed: determine direction from endpoint
            if let Some(ref ep) = endpoint {
                if let (Some(sv), Some(ev)) = (
                    Self::seq_value_to_f64(&seeds[0]),
                    Self::seq_value_to_f64(ep),
                ) {
                    if ev >= sv {
                        SeqMode::Arithmetic(1.0)
                    } else {
                        SeqMode::Arithmetic(-1.0)
                    }
                } else {
                    SeqMode::Arithmetic(1.0)
                }
            } else {
                SeqMode::Arithmetic(1.0) // infinite, default increasing
            }
        } else {
            // No seeds (only had a closure)
            SeqMode::Closure
        };

        // Check for "wrong side" sequences: arithmetic with endpoint on wrong side
        if let SeqMode::Arithmetic(step) = &mode
            && let Some(ref ep) = endpoint
            && let Some(ep_f) = Self::seq_value_to_f64(ep)
            && seeds.len() >= 2
        {
            let last_f = Self::seq_value_to_f64(seeds.last().unwrap()).unwrap_or(0.0);
            if (*step > 0.0 && ep_f < last_f) || (*step < 0.0 && ep_f > last_f) {
                let mut result = Vec::new();
                for s in &seeds {
                    if let Some(sv) = Self::seq_value_to_f64(s) {
                        if *step > 0.0 && sv > ep_f {
                            break;
                        }
                        if *step < 0.0 && sv < ep_f {
                            break;
                        }
                    }
                    if exclusive && Self::seq_values_equal(s, ep) {
                        break;
                    }
                    result.push(s.clone());
                }
                result.extend(extra_rhs);
                return Ok(Value::Array(result));
            }
        }

        // Check for "wrong side" on geometric sequences too
        if let SeqMode::Geometric(ratio) = &mode
            && let Some(ref ep) = endpoint
            && let Some(ep_f) = Self::seq_value_to_f64(ep)
            && seeds.len() >= 2
        {
            let last_f = Self::seq_value_to_f64(seeds.last().unwrap()).unwrap_or(0.0);
            if *ratio > 0.0 {
                // Non-alternating: check direction
                if (*ratio > 1.0 && ep_f < last_f)
                    || (*ratio < 1.0 && *ratio > 0.0 && ep_f > last_f)
                {
                    let mut result = Vec::new();
                    for s in &seeds {
                        if let Some(sv) = Self::seq_value_to_f64(s) {
                            if *ratio > 1.0 && sv > ep_f {
                                break;
                            }
                            if *ratio < 1.0 && sv < ep_f {
                                break;
                            }
                        }
                        if exclusive && Self::seq_values_equal(s, ep) {
                            break;
                        }
                        result.push(s.clone());
                    }
                    result.extend(extra_rhs);
                    return Ok(Value::Array(result));
                }
            } else {
                // Alternating (negative ratio): check |value| against |endpoint|
                let abs_last = last_f.abs();
                let abs_ep = ep_f.abs();
                if ratio.abs() > 1.0 && abs_ep < abs_last {
                    // Check if endpoint is theoretically reachable
                    let first_f = Self::seq_value_to_f64(&seeds[0]).unwrap_or(1.0).abs();
                    let reachable = if first_f > 1e-15 {
                        let log_val = (abs_ep / first_f).ln() / ratio.abs().ln();
                        (log_val - log_val.round()).abs() < 1e-9 && log_val >= 0.0
                    } else {
                        false
                    };
                    if !reachable {
                        let mut result = Vec::new();
                        for s in &seeds {
                            if let Some(sv) = Self::seq_value_to_f64(s)
                                && sv.abs() > abs_ep
                            {
                                break;
                            }
                            if exclusive && Self::seq_values_equal(s, ep) {
                                break;
                            }
                            result.push(s.clone());
                        }
                        result.extend(extra_rhs);
                        return Ok(Value::Array(result));
                    }
                }
            }
        }

        // For closure/regex endpoints, check if any seed already satisfies the predicate
        if !seeds.is_empty() {
            if let Some(EndpointKind::Closure(ref closure_val)) = endpoint_kind
                && let Value::Sub {
                    params,
                    body,
                    env: cenv,
                    ..
                } = closure_val
            {
                // Determine arity from params
                let arity = if !params.is_empty() { params.len() } else { 1 };

                for (i, _) in seeds.iter().enumerate() {
                    let saved = self.env.clone();
                    for (k, v) in cenv {
                        self.env.insert(k.clone(), v.clone());
                    }

                    // Collect the appropriate number of previous values up to position i+1
                    let args: Vec<Value> = if i + 1 < arity {
                        // Not enough values yet - pad with Nil
                        let mut args = vec![Value::Nil; arity - (i + 1)];
                        args.extend(seeds[..=i].iter().cloned());
                        args
                    } else {
                        // Take the last 'arity' values up to position i
                        seeds[i + 1 - arity..=i].to_vec()
                    };

                    // Bind parameters
                    for (j, param) in params.iter().enumerate() {
                        if j < args.len() {
                            self.env.insert(param.clone(), args[j].clone());
                        }
                    }

                    // Bind $_ to last arg
                    if let Some(last_arg) = args.last() {
                        self.env.insert("_".to_string(), last_arg.clone());
                    }

                    let predicate_result = self.eval_block_value(body)?;
                    self.env = saved;
                    if predicate_result.truthy() {
                        let end = if exclusive { i } else { i + 1 };
                        let mut result: Vec<Value> = seeds[..end].to_vec();
                        result.extend(extra_rhs);
                        return Ok(Value::Array(result));
                    }
                }
            }
            if let Some(EndpointKind::Regex(ref pat)) = endpoint_kind {
                for (i, seed) in seeds.iter().enumerate() {
                    let s = seed.to_string_value();
                    if self.regex_find_first(pat, &s).is_some() {
                        let end = if exclusive { i } else { i + 1 };
                        let mut result: Vec<Value> = seeds[..end].to_vec();
                        result.extend(extra_rhs);
                        return Ok(Value::Array(result));
                    }
                }
            }
        }

        // Check if any seed matches the endpoint — for geometric/alternating sequences,
        // the endpoint may match an earlier seed, not just the last one
        if !seeds.is_empty()
            && let Some(ref ep) = endpoint
            && matches!(endpoint_kind, Some(EndpointKind::Value(_)))
        {
            // Check last seed first (most common case)
            let last_seed = seeds.last().unwrap();
            if Self::seq_values_equal(last_seed, ep) {
                if exclusive {
                    let mut end = seeds.len();
                    while end > 0 && Self::seq_values_equal(&seeds[end - 1], ep) {
                        end -= 1;
                    }
                    let mut result: Vec<Value> = seeds[..end].to_vec();
                    result.extend(extra_rhs);
                    return Ok(Value::Array(result));
                } else {
                    let mut result = seeds.clone();
                    result.extend(extra_rhs);
                    return Ok(Value::Array(result));
                }
            }
            // For geometric/alternating: also check if the endpoint matches earlier seeds
            if matches!(mode, SeqMode::Geometric(_)) {
                for (i, s) in seeds.iter().enumerate() {
                    if Self::seq_values_equal(s, ep) {
                        if exclusive {
                            let mut result: Vec<Value> = seeds[..i].to_vec();
                            result.extend(extra_rhs);
                            return Ok(Value::Array(result));
                        } else {
                            let mut result: Vec<Value> = seeds[..=i].to_vec();
                            result.extend(extra_rhs);
                            return Ok(Value::Array(result));
                        }
                    }
                }
            }
        }

        // Generate values
        let mut result: Vec<Value> = seeds.clone();
        let max_gen = match (&mode, &endpoint_kind) {
            (SeqMode::Closure, Some(EndpointKind::Value(_))) => 256,
            (_, Some(_)) => 10000,
            (_, None) => 32,
        };
        let is_string_seq = !seeds.is_empty() && matches!(seeds.last(), Some(Value::Str(_)));

        for _ in 0..max_gen {
            let next = match &mode {
                SeqMode::Closure => {
                    let genfn = generator.as_ref().unwrap();
                    if let Value::Sub {
                        params, body, env, ..
                    } = genfn
                    {
                        let saved = self.env.clone();
                        for (k, v) in env {
                            self.env.insert(k.clone(), v.clone());
                        }

                        // Determine arity from params
                        let arity = if !params.is_empty() { params.len() } else { 1 };

                        // Collect the appropriate number of previous values
                        let result_len = result.len();
                        let args: Vec<Value> = if result_len == 0 {
                            vec![Value::Nil; arity]
                        } else if result_len < arity {
                            // Not enough values yet - pad with Nil
                            let mut args = vec![Value::Nil; arity - result_len];
                            args.extend(result.iter().cloned());
                            args
                        } else {
                            // Take the last 'arity' values
                            result[result_len - arity..].to_vec()
                        };

                        // Bind parameters
                        for (i, param) in params.iter().enumerate() {
                            if i < args.len() {
                                self.env.insert(param.clone(), args[i].clone());
                            }
                        }

                        // Bind $_ to last arg
                        if let Some(last_arg) = args.last() {
                            self.env.insert("_".to_string(), last_arg.clone());
                        }

                        let val = self.eval_block_value(body)?;
                        self.env = saved;
                        val
                    } else {
                        break;
                    }
                }
                SeqMode::Arithmetic(step) => {
                    if is_string_seq {
                        if *step == 0.0 {
                            // String constant sequence
                            result.last().unwrap().clone()
                        } else {
                            // String succession sequence: use .succ
                            let last = result.last().unwrap();
                            if let Value::Str(s) = last {
                                Value::Str(Self::string_succ(s))
                            } else {
                                break;
                            }
                        }
                    } else {
                        let last = result.last().unwrap();
                        Self::seq_add(last, *step)
                    }
                }
                SeqMode::Geometric(ratio) => {
                    let last = result.last().unwrap();
                    Self::seq_mul(last, *ratio)
                }
            };

            if endpoint_kind.is_none() {
                match next {
                    Value::Slip(items) => result.extend(items),
                    other => result.push(other),
                }
                continue;
            }

            // Check endpoint
            if let Some(ref epk) = endpoint_kind {
                match epk {
                    EndpointKind::Closure(closure_val) => {
                        // Call the closure with the generated value(s) as predicate
                        if let Value::Sub {
                            params,
                            body,
                            env: cenv,
                            ..
                        } = closure_val
                        {
                            let saved = self.env.clone();
                            for (k, v) in cenv {
                                self.env.insert(k.clone(), v.clone());
                            }

                            // Determine arity from params
                            let arity = if !params.is_empty() { params.len() } else { 1 };

                            // Collect the appropriate number of values including the new one
                            let result_len = result.len();
                            let args: Vec<Value> = if result_len == 0 {
                                vec![next.clone(); arity]
                            } else if result_len < arity - 1 {
                                // Not enough values yet - pad with Nil
                                let mut args = vec![Value::Nil; arity - result_len - 1];
                                args.extend(result.iter().cloned());
                                args.push(next.clone());
                                args
                            } else {
                                // Take the last 'arity-1' values plus the new one
                                let mut args = result[result_len - (arity - 1)..].to_vec();
                                args.push(next.clone());
                                args
                            };

                            // Bind parameters
                            for (i, param) in params.iter().enumerate() {
                                if i < args.len() {
                                    self.env.insert(param.clone(), args[i].clone());
                                }
                            }

                            // Bind $_ to last arg
                            if let Some(last_arg) = args.last() {
                                self.env.insert("_".to_string(), last_arg.clone());
                            }

                            let predicate_result = self.eval_block_value(body)?;
                            self.env = saved;
                            if predicate_result.truthy() {
                                if !exclusive {
                                    result.push(next);
                                }
                                break;
                            }
                        }
                    }
                    EndpointKind::Regex(pat) => {
                        // Match regex against the string value of next
                        let s = next.to_string_value();
                        if self.regex_find_first(pat, &s).is_some() {
                            if !exclusive {
                                result.push(next);
                            }
                            break;
                        }
                    }
                    EndpointKind::Value(ep) => {
                        if let Value::Package(type_name) = ep
                            && super::value_type_name(&next) == type_name
                        {
                            if !exclusive {
                                result.push(next);
                            }
                            break;
                        }
                        if Self::seq_values_equal(&next, ep) {
                            if !exclusive {
                                result.push(next);
                            }
                            break;
                        }
                        // Check if we went past the endpoint
                        if let (Some(nf), Some(ef)) =
                            (Self::seq_value_to_f64(&next), Self::seq_value_to_f64(ep))
                        {
                            match &mode {
                                SeqMode::Arithmetic(step) => {
                                    if *step > 0.0 && nf > ef {
                                        break;
                                    }
                                    if *step < 0.0 && nf < ef {
                                        break;
                                    }
                                }
                                SeqMode::Geometric(ratio) => {
                                    if *ratio > 0.0 {
                                        // Non-alternating geometric
                                        if *ratio > 1.0 && nf > ef {
                                            break;
                                        }
                                        if *ratio > 0.0 && *ratio < 1.0 && nf < ef {
                                            break;
                                        }
                                    } else {
                                        // Alternating geometric: check if endpoint is reachable
                                        let first_f = if !seeds.is_empty() {
                                            Self::seq_value_to_f64(&seeds[0]).unwrap_or(1.0).abs()
                                        } else {
                                            1.0
                                        };
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
                                                break;
                                            }
                                            if ratio.abs() < 1.0
                                                && ratio.abs() > 0.0
                                                && nf.abs() < ef.abs()
                                            {
                                                break;
                                            }
                                        }
                                    }
                                }
                                SeqMode::Closure => {
                                    // For closures, don't auto-stop based on direction
                                }
                            }
                        }
                    }
                }
            }

            result.push(next);
        }

        result.extend(extra_rhs);
        Ok(Value::Array(result))
    }
}
