use super::*;

impl Interpreter {
    fn collect_sequence_args_fixed(
        result: &[Value],
        arity: usize,
    ) -> Result<Vec<Value>, RuntimeError> {
        if arity == 0 {
            return Ok(Vec::new());
        }
        if result.len() < arity {
            return Err(RuntimeError::new(format!(
                "Too few positionals passed; expected {arity} arguments but got {}",
                result.len()
            )));
        }
        Ok(result[result.len() - arity..].to_vec())
    }

    fn collect_sequence_args_slurpy(result: &[Value], min_arity: usize) -> Vec<Value> {
        if result.len() >= min_arity {
            return result.to_vec();
        }
        let mut args = vec![Value::Nil; min_arity - result.len()];
        args.extend(result.iter().cloned());
        args
    }

    fn sequence_routine_param_mode(&self, package: &str, name: &str) -> SequenceRoutineParamMode {
        let name = name.strip_prefix('&').unwrap_or(name);
        if name.starts_with("prefix:<") || name.starts_with("postfix:<") {
            return SequenceRoutineParamMode::Fixed(1);
        }
        if name.starts_with("infix:<") {
            return SequenceRoutineParamMode::Fixed(2);
        }

        let local_prefix = format!("{package}::{name}/");
        let global_prefix = format!("GLOBAL::{name}/");
        let mut fixed_arity = 0usize;
        let mut slurpy_min: Option<usize> = None;

        for (key, def) in &self.functions {
            let loose_match = key.contains(&format!("::{name}/"));
            if !key.starts_with(&local_prefix) && !key.starts_with(&global_prefix) && !loose_match {
                continue;
            }

            let mut positional_non_slurpy = 0usize;
            let mut has_slurpy = false;
            if def.param_defs.is_empty() {
                positional_non_slurpy = def.params.len();
            } else {
                for pd in &def.param_defs {
                    if pd.named {
                        continue;
                    }
                    if pd.slurpy {
                        has_slurpy = true;
                    } else {
                        positional_non_slurpy += 1;
                    }
                }
            }

            if has_slurpy {
                slurpy_min = Some(match slurpy_min {
                    Some(existing) => existing.max(positional_non_slurpy),
                    None => positional_non_slurpy,
                });
            } else {
                fixed_arity = fixed_arity.max(positional_non_slurpy);
            }
        }

        if let Some(min) = slurpy_min {
            SequenceRoutineParamMode::Slurpy { min_arity: min }
        } else if fixed_arity > 0 {
            SequenceRoutineParamMode::Fixed(fixed_arity)
        } else {
            SequenceRoutineParamMode::Fixed(2)
        }
    }

    fn sequence_has_registered_routine(&self, package: &str, name: &str) -> bool {
        let name = name.strip_prefix('&').unwrap_or(name);
        let local_prefix = format!("{package}::{name}/");
        let global_prefix = format!("GLOBAL::{name}/");
        self.functions.keys().any(|key| {
            key.starts_with(&local_prefix)
                || key.starts_with(&global_prefix)
                || key.contains(&format!("::{name}/"))
        })
    }

    pub(super) fn eval_sequence(
        &mut self,
        left: Value,
        right: Value,
        exclusive: bool,
    ) -> Result<Value, RuntimeError> {
        let seeds_raw = Self::value_to_list(&left);
        if seeds_raw.is_empty() {
            return Ok(Value::array(vec![]));
        }

        // Separate seed values from generator closure
        let mut seeds: Vec<Value> = Vec::new();
        let mut generator: Option<Value> = None;
        for v in &seeds_raw {
            if matches!(v, Value::Sub(_) | Value::Routine { .. }) {
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
            Value::Whatever => (None, None, vec![]),
            Value::Junction { kind: _, values } => {
                // Junction endpoint: match against any of the junction values
                // Use the smallest/largest value as the effective endpoint for overshoot detection
                let _vals: Vec<Value> = values.iter().cloned().collect();
                (
                    Some(right.clone()),
                    Some(EndpointKind::Value(right.clone())),
                    vec![],
                )
            }
            Value::Array(items, ..) => {
                if items.is_empty() {
                    return Err(RuntimeError::new(
                        "Cannot use an empty list as endpoint of a sequence".to_string(),
                    ));
                }
                let first = &items[0];
                let rest: Vec<Value> = items[1..].to_vec();
                match first {
                    Value::Num(f) if f.is_infinite() => (None, None, rest),
                    Value::Whatever => (None, None, rest),
                    Value::Sub(_) => (
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
            // Range as endpoint: expand to list, first element is endpoint, rest are extras
            Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. } => {
                let items = super::utils::value_to_list(&right);
                if items.is_empty() {
                    return Err(RuntimeError::new(
                        "Cannot use an empty list as endpoint of a sequence".to_string(),
                    ));
                }
                let first = items[0].clone();
                let rest: Vec<Value> = items[1..].to_vec();
                (Some(first.clone()), Some(EndpointKind::Value(first)), rest)
            }
            Value::Sub(_) => (
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
            GeometricRat(i64, i64), // numerator, denominator - for rational ratios
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
                        // Check if we can use rational ratio (when seeds are all Int)
                        let all_int = seeds[seeds.len() - 3..]
                            .iter()
                            .all(|v| matches!(v, Value::Int(_)));
                        if all_int {
                            let a = if let Value::Int(i) = &seeds[seeds.len() - 3] {
                                *i
                            } else {
                                0
                            };
                            let b = if let Value::Int(i) = &seeds[seeds.len() - 2] {
                                *i
                            } else {
                                0
                            };
                            if a != 0 {
                                // Ratio is b/a as a fraction
                                let g = num_integer::Integer::gcd(&b, &a);
                                SeqMode::GeometricRat(b / g, a / g)
                            } else {
                                SeqMode::Geometric(r2)
                            }
                        } else {
                            SeqMode::Geometric(r2)
                        }
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
                        // Check direction from last two strings
                        let last_two = &seeds[seeds.len() - 2..];
                        if let (Value::Str(a), Value::Str(b)) = (&last_two[0], &last_two[1]) {
                            if b < a {
                                SeqMode::Arithmetic(-1.0) // descending
                            } else {
                                SeqMode::Arithmetic(1.0) // ascending
                            }
                        } else {
                            SeqMode::Arithmetic(1.0)
                        }
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
                } else if let (Value::Str(s), Value::Str(e)) = (&seeds[0], ep) {
                    // String direction: compare codepoints
                    if e >= s {
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
                return Ok(Value::array(result));
            }
        }

        // Check for "wrong side" on geometric sequences too
        let geo_ratio_f64 = match &mode {
            SeqMode::Geometric(r) => Some(*r),
            SeqMode::GeometricRat(n, d) => Some(*n as f64 / *d as f64),
            _ => None,
        };
        if let Some(ratio) = geo_ratio_f64
            && let Some(ref ep) = endpoint
            && let Some(ep_f) = Self::seq_value_to_f64(ep)
            && seeds.len() >= 2
        {
            let last_f = Self::seq_value_to_f64(seeds.last().unwrap()).unwrap_or(0.0);
            if ratio > 0.0 {
                // Non-alternating: check direction
                if (ratio > 1.0 && ep_f < last_f) || (ratio < 1.0 && ratio > 0.0 && ep_f > last_f) {
                    let mut result = Vec::new();
                    for s in &seeds {
                        if let Some(sv) = Self::seq_value_to_f64(s) {
                            if ratio > 1.0 && sv > ep_f {
                                break;
                            }
                            if ratio < 1.0 && sv < ep_f {
                                break;
                            }
                        }
                        if exclusive && Self::seq_values_equal(s, ep) {
                            break;
                        }
                        result.push(s.clone());
                    }
                    result.extend(extra_rhs);
                    return Ok(Value::array(result));
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
                        return Ok(Value::array(result));
                    }
                }
            }
        }

        // For closure/regex endpoints, check if any seed already satisfies the predicate
        if !seeds.is_empty() {
            if let Some(EndpointKind::Closure(ref closure_val)) = endpoint_kind
                && let Value::Sub(data) = closure_val
            {
                // Determine arity from params
                let arity = if !data.params.is_empty() {
                    data.params.len()
                } else {
                    1
                };

                for (i, _) in seeds.iter().enumerate() {
                    let saved = self.env.clone();
                    for (k, v) in &data.env {
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
                    for (j, param) in data.params.iter().enumerate() {
                        if j < args.len() {
                            self.env.insert(param.clone(), args[j].clone());
                        }
                    }

                    // Bind $_ to last arg
                    if let Some(last_arg) = args.last() {
                        self.env.insert("_".to_string(), last_arg.clone());
                    }
                    // Bind @_ to all values up to this point
                    self.env
                        .insert("@_".to_string(), Value::array(seeds[..=i].to_vec()));

                    let predicate_result = match self.eval_block_value(&data.body) {
                        Ok(v) => v,
                        Err(e) if e.return_value.is_some() => e.return_value.unwrap(),
                        Err(e) => return Err(e),
                    };
                    self.env = saved;
                    if predicate_result.truthy() {
                        let end = if exclusive { i } else { i + 1 };
                        let mut result: Vec<Value> = seeds[..end].to_vec();
                        result.extend(extra_rhs);
                        return Ok(Value::array(result));
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
                        return Ok(Value::array(result));
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
                    return Ok(Value::array(result));
                } else {
                    let mut result = seeds.clone();
                    result.extend(extra_rhs);
                    return Ok(Value::array(result));
                }
            }
            // For geometric/alternating: also check if the endpoint matches earlier seeds
            if matches!(mode, SeqMode::Geometric(_) | SeqMode::GeometricRat(..)) {
                for (i, s) in seeds.iter().enumerate() {
                    if Self::seq_values_equal(s, ep) {
                        if exclusive {
                            let mut result: Vec<Value> = seeds[..i].to_vec();
                            result.extend(extra_rhs);
                            return Ok(Value::array(result));
                        } else {
                            let mut result: Vec<Value> = seeds[..=i].to_vec();
                            result.extend(extra_rhs);
                            return Ok(Value::array(result));
                        }
                    }
                }
            }
        }

        // For geometric sequences with non-integer ratio, re-derive seeds (except first)
        // from the geometric formula to match Raku behavior:
        // (81, 27, 9 ... 1) gives (81, 27.0, 9.0, 3.0, 1.0)
        // Only apply when ALL seeds form a consistent geometric progression
        if let SeqMode::GeometricRat(num, den) = &mode {
            let ratio_f = *num as f64 / *den as f64;
            let all_geometric = seeds.len() >= 3 && {
                let floats: Vec<f64> = seeds.iter().filter_map(Self::seq_value_to_f64).collect();
                floats.len() == seeds.len()
                    && floats
                        .windows(2)
                        .all(|w| w[0].abs() > 1e-15 && ((w[1] / w[0]) - ratio_f).abs() < 1e-12)
            };
            if all_geometric && seeds.len() > 1 {
                let first = seeds[0].clone();
                for i in 1..seeds.len() {
                    seeds[i] = Self::seq_mul_rat(&seeds[i - 1], *num, *den);
                }
                seeds[0] = first; // keep first seed as-is
            }
        } else if let SeqMode::Geometric(ratio) = &mode
            && *ratio != ratio.floor()
            && seeds.len() > 1
        {
            for s in seeds.iter_mut().skip(1) {
                if let Value::Int(i) = s {
                    *s = Value::Num(*i as f64);
                }
            }
        }

        // After seed re-derivation, check if any seed matches a type endpoint
        if !seeds.is_empty()
            && let Some(EndpointKind::Value(Value::Package(ref type_name))) = endpoint_kind
        {
            for (i, s) in seeds.iter().enumerate() {
                if Self::seq_type_matches(s, type_name) {
                    let end = if exclusive { i } else { i + 1 };
                    let mut result: Vec<Value> = seeds[..end].to_vec();
                    result.extend(extra_rhs);
                    return Ok(Value::array(result));
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

        // Detect single-char codepoint mode: when all seeds and endpoint are single-char strings,
        // use codepoint increment/decrement instead of alphabetic .succ/.pred.
        let use_codepoint = is_string_seq
            && seeds
                .iter()
                .all(|v| matches!(v, Value::Str(s) if s.chars().count() == 1))
            && match &endpoint {
                Some(Value::Str(s)) => s.chars().count() == 1,
                None => true,
                _ => false,
            };

        for _ in 0..max_gen {
            let next = match &mode {
                SeqMode::Closure => {
                    let genfn = generator.as_ref().unwrap();
                    if let Value::Sub(data) = genfn {
                        if self.sequence_has_registered_routine(&data.package, &data.name) {
                            let args =
                                match self.sequence_routine_param_mode(&data.package, &data.name) {
                                    SequenceRoutineParamMode::Fixed(arity) => {
                                        Self::collect_sequence_args_fixed(&result, arity)?
                                    }
                                    SequenceRoutineParamMode::Slurpy { min_arity } => {
                                        Self::collect_sequence_args_slurpy(&result, min_arity)
                                    }
                                };
                            let call_name = data.name.strip_prefix('&').unwrap_or(&data.name);
                            self.call_function(call_name, args)?
                        } else {
                            let saved = self.env.clone();
                            for (k, v) in &data.env {
                                self.env.insert(k.clone(), v.clone());
                            }
                            let restore_env_with_side_effects =
                                |saved: std::collections::HashMap<String, Value>,
                                 current: &std::collections::HashMap<String, Value>|
                                 -> std::collections::HashMap<String, Value> {
                                    let mut merged = saved;
                                    for (k, v) in current {
                                        if merged.contains_key(k) {
                                            merged.insert(k.clone(), v.clone());
                                        }
                                    }
                                    merged
                                };

                            let slurpy_index = data
                                .params
                                .iter()
                                .position(|param| param.starts_with('@') || param.starts_with('%'));
                            let args: Vec<Value> = if data.params.is_empty() {
                                // No declared params: sequence generators still receive history in @_.
                                result.clone()
                            } else if let Some(min_arity) = slurpy_index {
                                Self::collect_sequence_args_slurpy(&result, min_arity)
                            } else {
                                let arity = data.params.len();
                                Self::collect_sequence_args_fixed(&result, arity)?
                            };

                            // Bind parameters
                            for (i, param) in data.params.iter().enumerate() {
                                if param.starts_with('@') {
                                    let rest = if i < args.len() {
                                        args[i..].to_vec()
                                    } else {
                                        Vec::new()
                                    };
                                    self.env.insert(param.clone(), Value::array(rest));
                                    break;
                                }
                                if param.starts_with('%') {
                                    let mut map = std::collections::HashMap::new();
                                    for item in args.iter().skip(i) {
                                        if let Value::Pair(k, v) = item {
                                            map.insert(k.clone(), (**v).clone());
                                        }
                                    }
                                    self.env.insert(
                                        param.clone(),
                                        Value::Hash(std::sync::Arc::new(map)),
                                    );
                                    break;
                                }
                                if let Some(arg) = args.get(i) {
                                    self.env.insert(param.clone(), arg.clone());
                                }
                            }

                            // Bind $_ to last arg
                            if let Some(last_arg) = args.last() {
                                self.env.insert("_".to_string(), last_arg.clone());
                            }
                            // Bind @_ to the argument history window.
                            self.env
                                .insert("@_".to_string(), Value::array(args.clone()));

                            let val = match self.eval_block_value(&data.body) {
                                Ok(v) => v,
                                Err(e) if e.return_value.is_some() => e.return_value.unwrap(),
                                Err(e) if e.is_last => {
                                    self.env = restore_env_with_side_effects(saved, &self.env);
                                    break;
                                }
                                Err(e) => {
                                    self.env = restore_env_with_side_effects(saved, &self.env);
                                    return Err(e);
                                }
                            };
                            self.env = restore_env_with_side_effects(saved, &self.env);
                            val
                        }
                    } else if let Value::Routine { name: rname, .. } = genfn {
                        let package = match genfn {
                            Value::Routine { package, .. } => package,
                            _ => unreachable!(),
                        };
                        let args = match self.sequence_routine_param_mode(package, rname) {
                            SequenceRoutineParamMode::Fixed(arity) => {
                                Self::collect_sequence_args_fixed(&result, arity)?
                            }
                            SequenceRoutineParamMode::Slurpy { min_arity } => {
                                Self::collect_sequence_args_slurpy(&result, min_arity)
                            }
                        };
                        self.call_sub_value(genfn.clone(), args, false)?
                    } else {
                        break;
                    }
                }
                SeqMode::Arithmetic(step) => {
                    if is_string_seq {
                        if *step == 0.0 {
                            // String constant sequence
                            result.last().unwrap().clone()
                        } else if use_codepoint {
                            // Single-char: use codepoint increment/decrement
                            let last = result.last().unwrap();
                            if let Value::Str(s) = last {
                                let ch = s.chars().next().unwrap_or('\0');
                                let delta = if *step > 0.0 { 1i32 } else { -1i32 };
                                if let Some(next_ch) =
                                    char::from_u32((ch as i64 + delta as i64) as u32)
                                {
                                    Value::Str(next_ch.to_string())
                                } else {
                                    break;
                                }
                            } else {
                                break;
                            }
                        } else if *step < 0.0 {
                            // String predecessor sequence: use .pred
                            let last = result.last().unwrap();
                            if let Value::Str(s) = last {
                                Value::Str(Self::string_pred(s))
                            } else {
                                break;
                            }
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
                        // For non-numeric values (e.g. custom objects), try .succ/.pred
                        if matches!(last, Value::Instance { .. }) {
                            let method = if *step >= 0.0 { "succ" } else { "pred" };
                            match self.call_method_with_values(last.clone(), method, vec![]) {
                                Ok(v) => v,
                                Err(_) => Self::seq_add(last, *step),
                            }
                        } else {
                            Self::seq_add(last, *step)
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
            let items_to_add: Vec<Value> = match next {
                Value::Slip(ref items) => items.iter().cloned().collect(),
                ref other => vec![other.clone()],
            };

            if endpoint_kind.is_none() {
                result.extend(items_to_add);
                continue;
            }

            // Check endpoint for each item (important for Slip returns)
            let mut should_break = false;
            for item in items_to_add {
                if should_break {
                    break;
                }
                if let Some(ref epk) = endpoint_kind {
                    match epk {
                        EndpointKind::Closure(closure_val) => {
                            if let Value::Sub(data) = closure_val {
                                let saved = self.env.clone();
                                for (k, v) in &data.env {
                                    self.env.insert(k.clone(), v.clone());
                                }

                                let arity = if !data.params.is_empty() {
                                    data.params.len()
                                } else {
                                    1
                                };

                                let result_len = result.len();
                                let args: Vec<Value> = if result_len == 0 {
                                    vec![item.clone(); arity]
                                } else if result_len < arity - 1 {
                                    let mut args = vec![Value::Nil; arity - result_len - 1];
                                    args.extend(result.iter().cloned());
                                    args.push(item.clone());
                                    args
                                } else {
                                    let mut args = result[result_len - (arity - 1)..].to_vec();
                                    args.push(item.clone());
                                    args
                                };

                                for (i, param) in data.params.iter().enumerate() {
                                    if i < args.len() {
                                        self.env.insert(param.clone(), args[i].clone());
                                    }
                                }
                                if let Some(last_arg) = args.last() {
                                    self.env.insert("_".to_string(), last_arg.clone());
                                }
                                // Bind @_ to all values including the current item
                                {
                                    let mut all_vals = result.clone();
                                    all_vals.push(item.clone());
                                    self.env.insert("@_".to_string(), Value::array(all_vals));
                                }

                                let predicate_result = match self.eval_block_value(&data.body) {
                                    Ok(v) => v,
                                    Err(e) if e.return_value.is_some() => e.return_value.unwrap(),
                                    Err(e) => return Err(e),
                                };
                                self.env = saved;
                                if predicate_result.truthy() {
                                    if !exclusive {
                                        result.push(item);
                                    }
                                    should_break = true;
                                    continue;
                                }
                            }
                        }
                        EndpointKind::Regex(pat) => {
                            let s = item.to_string_value();
                            if self.regex_find_first(pat, &s).is_some() {
                                if !exclusive {
                                    result.push(item);
                                }
                                should_break = true;
                                continue;
                            }
                        }
                        EndpointKind::Value(ep) => {
                            if let Value::Package(type_name) = ep
                                && Self::seq_type_matches(&item, type_name)
                            {
                                if !exclusive {
                                    result.push(item);
                                }
                                should_break = true;
                                continue;
                            }
                            if Self::seq_values_equal(&item, ep) {
                                if !exclusive {
                                    result.push(item);
                                }
                                should_break = true;
                                continue;
                            }
                            // Check if we went past the endpoint (string comparison)
                            if let (Value::Str(ns), Value::Str(es)) = (&item, ep)
                                && let SeqMode::Arithmetic(step) = &mode
                            {
                                if *step > 0.0 && ns > es {
                                    should_break = true;
                                    continue;
                                }
                                if *step < 0.0 && ns < es {
                                    should_break = true;
                                    continue;
                                }
                            }
                            // Check if we went past the endpoint (numeric)
                            if let (Some(nf), Some(ef)) =
                                (Self::seq_value_to_f64(&item), Self::seq_value_to_f64(ep))
                            {
                                match &mode {
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
                                        let ratio = match &mode {
                                            SeqMode::Geometric(r) => *r,
                                            SeqMode::GeometricRat(n, d) => *n as f64 / *d as f64,
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
                                            let first_f = if !seeds.is_empty() {
                                                Self::seq_value_to_f64(&seeds[0])
                                                    .unwrap_or(1.0)
                                                    .abs()
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
        }

        result.extend(extra_rhs);
        // When the endpoint is infinite (None), return a LazyList to preserve laziness
        if endpoint.is_none() && endpoint_kind.is_none() {
            Ok(Value::LazyList(std::sync::Arc::new(
                crate::value::LazyList {
                    body: vec![],
                    env: std::collections::HashMap::new(),
                    cache: std::sync::Mutex::new(Some(result)),
                },
            )))
        } else {
            Ok(Value::array(result))
        }
    }

    /// Evaluate a chained sequence: infix:<...>(seed, waypoint1, waypoint2, ..., endpoint)
    /// Each waypoint can be a scalar or a list. If a list, the first element is the endpoint
    /// for the previous segment, and the whole list is the seed for the next segment.
    pub(super) fn eval_chained_sequence(
        &mut self,
        args: &[Value],
        exclude_end: bool,
    ) -> Result<Value, RuntimeError> {
        if args.len() < 3 {
            // Not a chained sequence
            let left = args[0].clone();
            let right = args.last().cloned().unwrap_or(Value::Nil);
            return self.eval_sequence(left, right, exclude_end);
        }

        // Build segments: [(seed, endpoint), ...]
        // First element is always the initial seed
        // Middle elements are waypoints (can be lists)
        // Last element is the final endpoint
        let mut all_results: Vec<Value> = Vec::new();
        let mut current_seed = vec![args[0].clone()];

        for i in 1..args.len() {
            let waypoint = &args[i];
            let is_last = i == args.len() - 1;

            // Determine endpoint for this segment and seed for next
            let (endpoint, next_seed) = if is_last {
                // Final endpoint - use exclude_end for the last segment only
                (waypoint.clone(), vec![])
            } else {
                // Waypoint: if it's a list, first element is endpoint, whole list is next seed
                match waypoint {
                    Value::Array(items, ..) if !items.is_empty() => {
                        let ep = items[0].clone();
                        (ep, items.to_vec())
                    }
                    _ => (waypoint.clone(), vec![waypoint.clone()]),
                }
            };

            // Run this segment (never exclude end for intermediate segments)
            let seg_exclude_end = if is_last { exclude_end } else { false };
            let left = if current_seed.len() == 1 {
                current_seed[0].clone()
            } else {
                Value::array(current_seed.clone())
            };
            let seg_result = self.eval_sequence(left, endpoint, seg_exclude_end)?;

            // Collect results, avoiding duplicates at segment boundaries
            let items: Option<Vec<Value>> = match &seg_result {
                Value::Array(items, ..) => Some(items.as_ref().clone()),
                Value::LazyList(ll) => ll.cache.lock().unwrap().clone(),
                _ => None,
            };
            if let Some(items) = items {
                if all_results.is_empty() {
                    all_results.extend(items.iter().cloned());
                } else if !items.is_empty() {
                    // Skip the first element of subsequent segments if it matches
                    // the last element of the accumulated result (avoid duplication)
                    let start = if !all_results.is_empty()
                        && Self::seq_values_equal(all_results.last().unwrap(), &items[0])
                    {
                        1
                    } else {
                        0
                    };
                    all_results.extend(items[start..].iter().cloned());
                }
            }

            current_seed = next_seed;
        }

        Ok(Value::array(all_results))
    }
}

enum SequenceRoutineParamMode {
    Fixed(usize),
    Slurpy { min_arity: usize },
}
