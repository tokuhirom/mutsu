use super::super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Evaluate a `** {code}` quantifier code block and return (min, max).
    /// The code should return either a numeric value (exact count) or a Range.
    /// Returns None if the code fails to evaluate or produces an invalid/infinite value;
    /// in invalid cases a pending error is set via PENDING_REGEX_ERROR for the caller to propagate.
    pub(super) fn eval_regex_repeat_code(
        &self,
        code: &str,
        caps: &RegexCaptures,
    ) -> Option<(usize, Option<usize>)> {
        let (stmts, _) = crate::parse_dispatch::parse_source(code).ok()?;
        let env = self.make_regex_eval_env(caps);
        let mut interp = Interpreter {
            env,
            current_package: Arc::new(RwLock::new(self.current_package())),
            ..Default::default()
        };
        self.copy_decl_registry_into(&mut interp);
        let val = match interp.eval_block_value(&stmts) {
            Ok(v) => v,
            Err(_) => return None,
        };

        /// Helper: check if a Value is a non-numeric type (Str/Bool/etc.) for range endpoints.
        fn is_non_numeric_value(v: &Value) -> bool {
            matches!(v, Value::Str(_) | Value::Bool(_))
        }

        /// Helper: extract f64 from a Value, treating NaN/Inf specially.
        fn endpoint_to_f64(v: &Value) -> f64 {
            match v {
                Value::Num(n) => *n,
                Value::Int(i) => *i as f64,
                Value::Rat(n, d) => *n as f64 / *d as f64,
                _ => v.to_f64(),
            }
        }

        match &val {
            Value::Range(start, end) => {
                let min = (*start).max(0) as usize;
                let max = if *end == i64::MAX {
                    None
                } else {
                    Some((*end).max(0) as usize)
                };
                // Check for empty range (min > max)
                if let Some(max_val) = max
                    && min > max_val
                {
                    Self::set_quantifier_value_error("empty-range", "Quantifier range is empty");
                    return None;
                }
                Some((min, max))
            }
            Value::RangeExcl(start, end) => {
                let min = (*start).max(0) as usize;
                let max = if *end == i64::MAX {
                    None
                } else {
                    Some(((*end) - 1).max(0) as usize)
                };
                if let Some(max_val) = max
                    && min > max_val
                {
                    Self::set_quantifier_value_error("empty-range", "Quantifier range is empty");
                    return None;
                }
                Some((min, max))
            }
            Value::RangeExclStart(start, end) => {
                let min = ((*start) + 1).max(0) as usize;
                let max = if *end == i64::MAX {
                    None
                } else {
                    Some((*end).max(0) as usize)
                };
                if let Some(max_val) = max
                    && min > max_val
                {
                    Self::set_quantifier_value_error("empty-range", "Quantifier range is empty");
                    return None;
                }
                Some((min, max))
            }
            Value::RangeExclBoth(start, end) => {
                let min = ((*start) + 1).max(0) as usize;
                let max = if *end == i64::MAX {
                    None
                } else {
                    Some(((*end) - 1).max(0) as usize)
                };
                if let Some(max_val) = max
                    && min > max_val
                {
                    Self::set_quantifier_value_error("empty-range", "Quantifier range is empty");
                    return None;
                }
                Some((min, max))
            }
            Value::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                // Check for non-numeric range endpoints (e.g., strings, NaN endpoints)
                if is_non_numeric_value(start.as_ref()) || is_non_numeric_value(end.as_ref()) {
                    Self::set_quantifier_value_error(
                        "non-numeric-range",
                        "Quantifier range has non-numeric endpoint",
                    );
                    return None;
                }
                let start_f = endpoint_to_f64(start.as_ref());
                let end_f = endpoint_to_f64(end.as_ref());
                // NaN in either endpoint → non-numeric-range
                if start_f.is_nan() || end_f.is_nan() {
                    Self::set_quantifier_value_error(
                        "non-numeric-range",
                        "Quantifier range has non-numeric (NaN) endpoint",
                    );
                    return None;
                }
                // Inf as start → error (infinite lower bound)
                if start_f.is_infinite() && start_f > 0.0 {
                    Self::set_quantifier_value_error("inf", "Quantifier lower bound is Inf");
                    return None;
                }
                // Compute min and max.
                // For float ranges, Raku uses floor for the inclusive bound
                // and floor+1 for the exclusive bound.
                let min_f = if start_f.is_infinite() && start_f < 0.0 {
                    // -Inf start: effective min is 0
                    0.0
                } else if *excl_start {
                    start_f.floor() + 1.0
                } else {
                    start_f.floor()
                };
                let min = if min_f < 0.0 { 0 } else { min_f as usize };

                let max = if end_f.is_infinite() && end_f > 0.0 {
                    None // +Inf end → unbounded
                } else {
                    let max_f = if *excl_end {
                        end_f.ceil() - 1.0
                    } else {
                        end_f.floor()
                    };
                    let max_val = if max_f < 0.0 { 0 } else { max_f as usize };
                    Some(max_val)
                };
                // Empty range check
                if let Some(max_val) = max
                    && min > max_val
                {
                    Self::set_quantifier_value_error("empty-range", "Quantifier range is empty");
                    return None;
                }
                Some((min, max))
            }
            Value::Str(s) => {
                // String values that cannot parse as a number are non-numeric
                match s.trim().parse::<f64>() {
                    Ok(n) if n.is_nan() => {
                        Self::set_quantifier_value_error(
                            "non-numeric",
                            "Quantifier value is not numeric",
                        );
                        None
                    }
                    Ok(n) if n.is_infinite() && n > 0.0 => {
                        Self::set_quantifier_value_error("inf", "Quantifier value is Inf");
                        None
                    }
                    Ok(n) => {
                        let n = n.max(0.0) as usize;
                        Some((n, Some(n)))
                    }
                    Err(_) => {
                        // Non-parseable string like "meow"
                        Self::set_quantifier_value_error(
                            "non-numeric",
                            "Quantifier value is not numeric",
                        );
                        None
                    }
                }
            }
            _ => {
                let n = val.to_f64();
                // Non-numeric value (NaN)
                if n.is_nan() {
                    Self::set_quantifier_value_error(
                        "non-numeric",
                        "Quantifier value is not numeric",
                    );
                    return None;
                }
                // Positive Inf is an error; negative Inf is treated as 0.
                if n.is_infinite() && n > 0.0 {
                    Self::set_quantifier_value_error("inf", "Quantifier value is Inf");
                    return None;
                }
                let n = n.max(0.0) as usize;
                Some((n, Some(n)))
            }
        }
    }

    /// Enable eager collection of plain code blocks during regex matching.
    /// When enabled, code blocks are recorded even if the overall match fails.
    pub(in crate::runtime) fn enable_eager_code_blocks(&self) {
        super::regex_helpers::EAGER_CODE_BLOCKS.with(|slot| *slot.borrow_mut() = Some(Vec::new()));
    }

    /// Drain and return eagerly-collected code blocks, disabling collection.
    pub(in crate::runtime) fn drain_eager_code_blocks(&self) -> Vec<CodeBlockContext> {
        super::regex_helpers::EAGER_CODE_BLOCKS
            .with(|slot| slot.borrow_mut().take().unwrap_or_default())
    }

    /// Execute code blocks collected during regex matching for side effects.
    pub(in crate::runtime) fn execute_regex_code_blocks(
        &mut self,
        code_blocks: &[CodeBlockContext],
    ) {
        for ctx in code_blocks {
            let Ok((stmts, _)) = crate::parse_dispatch::parse_source(&ctx.code) else {
                continue;
            };
            // Set up $/ as a match object for the matched-so-far text
            let match_obj = Value::make_match_object_with_captures(
                ctx.matched_so_far.clone(),
                0,
                ctx.matched_so_far.chars().count() as i64,
                &[],
                &HashMap::new(),
            );
            self.env.insert("/".to_string(), match_obj.clone());
            // Set up $¢ (current match cursor) — same as $/ for in-progress match
            self.env.insert("\u{00A2}".to_string(), match_obj);
            // Set up positional captures ($0, $1, ...)
            for (i, val) in ctx.positional.iter().enumerate() {
                let pos_match = Value::make_match_object_with_captures(
                    val.clone(),
                    0,
                    val.chars().count() as i64,
                    &[],
                    &HashMap::new(),
                );
                self.env.insert(i.to_string(), pos_match);
            }
            // Set up named captures as $<name> variables
            let ast_hint = self.env.get("made").cloned().unwrap_or(Value::Nil);
            for (k, v) in &ctx.named {
                let to_match_with_ast = |text: &str, ast: &Value| -> Value {
                    let match_obj = Value::make_match_object_with_captures(
                        text.to_string(),
                        0,
                        text.chars().count() as i64,
                        &[],
                        &HashMap::new(),
                    );
                    if let Value::Instance {
                        class_name,
                        attributes,
                        ..
                    } = match_obj
                    {
                        let attrs = attributes.as_ref().clone();
                        attrs.insert("ast".to_string(), ast.clone());
                        Value::make_instance(class_name, (attrs).to_map())
                    } else {
                        match_obj
                    }
                };
                let value = if v.len() == 1 {
                    to_match_with_ast(&v[0], &ast_hint)
                } else {
                    Value::array(
                        v.iter()
                            .map(|s| to_match_with_ast(s, &ast_hint))
                            .collect::<Vec<_>>(),
                    )
                };
                self.env.insert(format!("<{}>", k), value);
            }
            // Snapshot env keys and values before execution
            let snapshot: HashMap<Symbol, String> = self
                .env
                .iter()
                .map(|(k, v)| (*k, format!("{:?}", v)))
                .collect();
            let _ = self.eval_block_value(&stmts);
            // Record changed env variables as pending local updates for the outer VM
            for (k, v) in &self.env {
                let old_repr = snapshot.get(k).map(|s| s.as_str()).unwrap_or("");
                let new_repr = format!("{:?}", v);
                if old_repr != new_repr {
                    let name = k.resolve();
                    // Slice C' (docs/vm-single-store.md, open-question #2): an
                    // embedded regex `{ ... }` / `:my`/`:let` block writes a caller
                    // lexical *directly* into `env`, bypassing
                    // `set_env_with_main_alias`. If a carrier is active (the regex
                    // ran inside an EVAL / interpreter fallback), log the name into
                    // the carrier write set too, so the carrier-return writeback
                    // reconciles it precisely and the blanket `env_dirty` net can be
                    // dropped for non-EVAL carriers as well. Logging a superset is
                    // safe — the writeback filters by the caller's compiled slots.
                    if let Some(set) = self.carrier_writes.as_mut() {
                        set.insert(name.clone());
                    }
                    self.pending_local_updates.push((name, v.clone()));
                }
            }
        }
    }
}
