use super::vm_string_regex_ops::*;
use super::*;

impl Interpreter {
    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_transliterate_op(
        &mut self,
        code: &CompiledCode,
        from_idx: u32,
        to_idx: u32,
        delete: bool,
        complement: bool,
        squash: bool,
        non_destructive: bool,
    ) -> Result<(), RuntimeError> {
        let from = Self::const_str(code, from_idx);
        let to = Self::const_str(code, to_idx);
        let target = self.env().get("_").cloned().unwrap_or(Value::Nil);
        // If $_ is bound to a read-only topic (e.g. `with 'literal' { ... }`),
        // tr/// must throw X::Assignment::RO. The `with` desugaring marks the
        // topic value with a Mixin override `__mutsu_topic_ro__` when the
        // condition expression is a literal.
        if !non_destructive
            && let Value::Mixin(_, overrides) = &target
            && overrides.contains_key("__mutsu_topic_ro__")
        {
            let mut attrs = std::collections::HashMap::new();
            attrs.insert(
                "message".to_string(),
                Value::str(format!(
                    "Cannot modify an immutable Str ({})",
                    target.to_string_value()
                )),
            );
            attrs.insert("value".to_string(), target);
            return Err(RuntimeError::typed("X::Assignment::RO", attrs));
        }
        let text = target.to_string_value();

        let translated = crate::builtins::transliterate::transliterate(
            &text, from, to, delete, squash, complement,
        );
        let translated_value = Value::str(translated.clone());

        // tr/// (lowercase) always modifies $_; TR/// (uppercase) only modifies
        // $_ in smartmatch context (so that $var ~~ TR/// writes back to $var).
        if !non_destructive || self.in_smartmatch_rhs {
            self.env_mut().insert("_".to_string(), translated_value);
        }
        // Signal to the smartmatch handler that this is a transliterate result
        // so it returns the result directly (as StrDistance) instead of comparing.
        if self.in_smartmatch_rhs {
            self.transliterate_in_smartmatch = true;
        }
        // tr/// (destructive) returns a StrDistance object holding both the
        // original and the transliterated string; it stringifies to `after`.
        // TR/// (non-destructive) returns a plain Str with the translated text.
        let result = if non_destructive {
            Value::str(translated)
        } else {
            let mut sd_attrs = std::collections::HashMap::new();
            sd_attrs.insert("before".to_string(), Value::str(text));
            sd_attrs.insert("after".to_string(), Value::str(translated));
            Value::make_instance(Symbol::intern("StrDistance"), sd_attrs)
        };
        self.stack.push(result);
        Ok(())
    }

    /// Check if a value is "listy" (array, hash, range, seq, etc.) for hyper op purposes.
    /// Scalars return false, meaning the hyper result should not be wrapped in an array.
    pub(super) fn is_listy(val: &Value) -> bool {
        matches!(
            val,
            Value::Array(..)
                | Value::Seq(..)
                | Value::Hash(..)
                | Value::Range(..)
                | Value::RangeExcl(..)
                | Value::RangeExclStart(..)
                | Value::RangeExclBoth(..)
                | Value::GenericRange { .. }
                | Value::LazyList(..)
                | Value::Set(..)
                | Value::Bag(..)
                | Value::Mix(..)
        )
    }

    /// Build an `X::HyperOp::Infinite` exception carrying the `side` attribute
    /// (`left` / `right` / `both`) identifying which operand(s) are infinite.
    fn hyperop_infinite_error(side: &str) -> RuntimeError {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("side".to_string(), Value::str(side.to_string()));
        let msg = "Lists on both sides of non-dwimmy hyperop are not of the same length, \
                   and at least one is lazy or infinite"
            .to_string();
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        let mut err = RuntimeError::new(format!("X::HyperOp::Infinite: {}", msg));
        err.exception = Some(Box::new(Value::make_instance(
            crate::symbol::Symbol::intern("X::HyperOp::Infinite"),
            attrs,
        )));
        err
    }

    /// Build an `X::HyperOp::NonDWIM` exception carrying `left-elems`,
    /// `right-elems`, and `operator` attributes for a non-dwimmy hyper op length
    /// mismatch. `op` is the infix source (e.g. `+`); `.operator` is exposed as
    /// a routine handle named `infix:<+>` so `.operator.name` works.
    fn hyperop_nondwim_error(left_elems: usize, right_elems: usize, op: &str) -> RuntimeError {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("left-elems".to_string(), Value::Int(left_elems as i64));
        attrs.insert("right-elems".to_string(), Value::Int(right_elems as i64));
        attrs.insert(
            "operator".to_string(),
            Value::Routine {
                package: crate::symbol::Symbol::intern("GLOBAL"),
                name: crate::symbol::Symbol::intern(&format!("infix:<{}>", op)),
                is_regex: false,
            },
        );
        let msg = format!(
            "Lists on both sides of non-dwimmy hyperop are not of the same length: \
             left: {} elements, right: {} elements",
            left_elems, right_elems
        );
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        let mut err = RuntimeError::new(format!("X::HyperOp::NonDWIM: {}", msg));
        err.exception = Some(Box::new(Value::make_instance(
            crate::symbol::Symbol::intern("X::HyperOp::NonDWIM"),
            attrs,
        )));
        err
    }

    pub(super) fn exec_hyper_op(
        &mut self,
        code: &CompiledCode,
        op_idx: u32,
        dwim_left: bool,
        dwim_right: bool,
    ) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap_or(Value::Nil);
        let left = self.stack.pop().unwrap_or(Value::Nil);
        let op = Self::const_str(code, op_idx).to_string();
        // X::HyperOp::Infinite: when the result length is determined by an
        // infinite/lazy operand, the hyper op cannot produce a finite result.
        // Checked once at the top level (nested elements are already realized).
        let left_inf = Self::is_listy(&left) && crate::builtins::methods_0arg::is_value_lazy(&left);
        let right_inf =
            Self::is_listy(&right) && crate::builtins::methods_0arg::is_value_lazy(&right);
        if left_inf || right_inf {
            // A side determines the result length unless it is the *sole* dwim
            // (cycling) side: `!(dwim_x && !dwim_other)`.
            let left_determines = !dwim_left || dwim_right;
            let right_determines = !dwim_right || dwim_left;
            if (left_inf && left_determines) || (right_inf && right_determines) {
                let side = match (left_inf, right_inf) {
                    (true, true) => "both",
                    (true, false) => "left",
                    _ => "right",
                };
                return Err(Self::hyperop_infinite_error(side));
            }
        }
        let result = self.hyper_op_pair(&op, &left, &right, dwim_left, dwim_right)?;
        self.stack.push(result);
        Ok(())
    }

    /// Apply a hyper binary op to a pair of values, recursing into nested
    /// Iterables so that e.g. `(1, {a=>2}, 4) <<~>> <a b c>` distributes the
    /// op into the hash element, yielding `("1a", {a=>"2b"}, "4c")`.
    fn hyper_op_pair(
        &mut self,
        op: &str,
        left: &Value,
        right: &Value,
        dwim_left: bool,
        dwim_right: bool,
    ) -> Result<Value, RuntimeError> {
        // Hyper op on two hashes: combine values key-by-key, with the dwim arrows
        // selecting the resulting key set. A missing value on either side uses the
        // operator's identity element (e.g. 0 for `+`).
        if let (Value::Hash(la), Value::Hash(ra)) = (&left, &right) {
            let la = la.clone();
            let ra = ra.clone();
            // Key set by dwim direction:
            //   >>op<<  (neither dwims)  -> union
            //   <<op>>  (both dwim)      -> intersection
            //   >>op>>  (right dwims)    -> left's keys
            //   <<op<<  (left dwims)     -> right's keys
            let keys: Vec<String> = match (dwim_left, dwim_right) {
                (false, false) => {
                    let mut ks: Vec<String> = la.keys().cloned().collect();
                    for k in ra.keys() {
                        if !la.contains_key(k) {
                            ks.push(k.clone());
                        }
                    }
                    ks
                }
                (true, true) => la.keys().filter(|k| ra.contains_key(*k)).cloned().collect(),
                (false, true) => la.keys().cloned().collect(),
                (true, false) => ra.keys().cloned().collect(),
            };
            let identity = runtime::reduction_identity(op);
            let mut result = std::collections::HashMap::with_capacity(keys.len());
            for key in keys {
                let l = la.get(&key).unwrap_or(&identity).clone();
                let r = ra.get(&key).unwrap_or(&identity).clone();
                let v = self.hyper_op_pair(op, &l, &r, dwim_left, dwim_right)?;
                result.insert(key, v);
            }
            return Ok(Value::Hash(Value::hash_arc(result)));
        }
        // Hyper op between a hash and a scalar: apply the op to each value with
        // the scalar broadcast over every key (`%h >>*>> 4`, `2 <<**<< %h`).
        if let Value::Hash(map) = &left
            && !Self::is_listy(right)
        {
            let map = map.clone();
            let mut result = std::collections::HashMap::with_capacity(map.len());
            for (key, value) in map.iter() {
                let v = self.hyper_op_pair(op, value, right, dwim_left, dwim_right)?;
                result.insert(key.clone(), v);
            }
            return Ok(Value::Hash(Value::hash_arc(result)));
        }
        if let Value::Hash(map) = &right
            && !Self::is_listy(left)
        {
            let map = map.clone();
            let mut result = std::collections::HashMap::with_capacity(map.len());
            for (key, value) in map.iter() {
                let v = self.hyper_op_pair(op, left, value, dwim_left, dwim_right)?;
                result.insert(key.clone(), v);
            }
            return Ok(Value::Hash(Value::hash_arc(result)));
        }
        // At least one side is a (non-hash) Iterable: distribute element-wise,
        // recursing so nested Iterables/Hashes are handled at every depth.
        if Self::is_listy(left) || Self::is_listy(right) {
            let mut left_list = Interpreter::value_to_list(left);
            let mut right_list = Interpreter::value_to_list(right);
            // A list literal ending in `*` (Whatever) is "infinitely extensible
            // by copying its last real element". Strip the trailing Whatever;
            // such a side adapts to the other side's length (like a dwim side)
            // but pads with its last real element instead of cycling.
            let left_ext = matches!(left_list.last(), Some(Value::Whatever));
            let right_ext = matches!(right_list.last(), Some(Value::Whatever));
            if left_ext {
                left_list.pop();
            }
            if right_ext {
                right_list.pop();
            }
            let left_len = left_list.len();
            let right_len = right_list.len();
            // A side determines the result length only when it neither dwims nor
            // is `*`-extensible.
            let left_fixed = !dwim_left && !left_ext;
            let right_fixed = !dwim_right && !right_ext;
            // A non-dwimmy, non-extensible hyper requires equal lengths.
            if left_fixed && right_fixed && left_len != right_len {
                return Err(Self::hyperop_nondwim_error(left_len, right_len, op));
            }
            // An empty operand cannot be cycled or padded to fill a dwim side, so
            // any empty side yields an empty result (`True »+» ()` is `()`, not a
            // `0`-padded `(1,)`). The fixed-length mismatch above already covers
            // the case where the empty side must raise X::HyperOp::NonDWIM.
            if left_len == 0 || right_len == 0 {
                return Ok(Value::array(Vec::new()));
            }
            let result_len = if left_fixed {
                left_len
            } else if right_fixed {
                right_len
            } else {
                std::cmp::max(left_len, right_len)
            };
            // A `*`-extensible side pads with its last real element; an ordinary
            // dwim side cycles from the start.
            let l_index = |i: usize| {
                if left_ext {
                    i.min(left_len - 1)
                } else {
                    i % left_len
                }
            };
            let r_index = |i: usize| {
                if right_ext {
                    i.min(right_len - 1)
                } else {
                    i % right_len
                }
            };
            let mut results = Vec::with_capacity(result_len);
            for i in 0..result_len {
                let l = &left_list[l_index(i)];
                let r = &right_list[r_index(i)];
                results.push(self.hyper_op_pair(op, l, r, dwim_left, dwim_right)?);
            }
            // Preserve List kind when inputs are Lists (not Arrays)
            let left_is_array = matches!(left, Value::Array(_, crate::value::ArrayKind::Array));
            let right_is_array = matches!(right, Value::Array(_, crate::value::ArrayKind::Array));
            return if !left_is_array && !right_is_array {
                Ok(Value::Array(
                    std::sync::Arc::new(crate::value::ArrayData::new(results)),
                    crate::value::ArrayKind::List,
                ))
            } else {
                Ok(Value::real_array(results))
            };
        }
        // Base case: both operands are scalars.
        if op == "~~" {
            return Ok(Value::Bool(self.vm_smart_match(left, right)));
        }
        // Try user-defined infix dispatch first when either operand is an
        // instance, to avoid built-in ops silently coercing objects.
        if matches!(left, Value::Instance { .. }) || matches!(right, Value::Instance { .. }) {
            let infix_name = format!("infix:<{}>", op);
            if let Some(v) = self.try_user_infix(&infix_name, left, right)? {
                return Ok(v);
            }
        }
        self.eval_reduction_operator_values(op, left, right)
    }

    /// The QuantHash kind and mutability of a value, if it is a Set/Bag/Mix.
    pub(super) fn quanthash_kind(v: &Value) -> Option<(QuantKind, bool)> {
        match v {
            Value::Set(_, m) => Some((QuantKind::Set, *m)),
            Value::Bag(_, m) => Some((QuantKind::Bag, *m)),
            Value::Mix(_, m) => Some((QuantKind::Mix, *m)),
            _ => None,
        }
    }

    /// Project a QuantHash to a plain `key => weight` Hash so the existing hash
    /// hyper logic applies. Set membership becomes `True`, Bag/Mix weights become
    /// Int/Num. Non-QuantHash values pass through unchanged (scalar broadcast).
    pub(super) fn quanthash_to_hash(v: &Value) -> Value {
        let map: std::collections::HashMap<String, Value> = match v {
            Value::Set(d, _) => d
                .elements
                .iter()
                .map(|k| (k.clone(), Value::Bool(true)))
                .collect(),
            Value::Bag(d, _) => d
                .counts
                .iter()
                .map(|(k, c)| (k.clone(), Value::from_bigint(c.clone())))
                .collect(),
            Value::Mix(d, _) => d
                .weights
                .iter()
                .map(|(k, w)| (k.clone(), Value::Num(*w)))
                .collect(),
            other => return other.clone(),
        };
        Value::Hash(Value::hash_arc(map))
    }

    /// Rebuild a QuantHash of the given kind/mutability from a result Hash,
    /// applying Rakudo's QuantHash coercion: Set keeps truthy keys, Bag keeps
    /// strictly-positive integer weights, Mix keeps non-zero weights.
    pub(super) fn hash_to_quanthash(v: Value, kind: QuantKind, mutable: bool) -> Value {
        let Value::Hash(map) = v else {
            return v;
        };
        match kind {
            QuantKind::Set => {
                let elems: std::collections::HashSet<String> = map
                    .iter()
                    .filter(|(_, val)| val.truthy())
                    .map(|(k, _)| k.clone())
                    .collect();
                if mutable {
                    Value::set_hash(elems)
                } else {
                    Value::set(elems)
                }
            }
            QuantKind::Bag => {
                let counts: std::collections::HashMap<String, i64> = map
                    .iter()
                    .filter_map(|(k, val)| {
                        let c = crate::runtime::utils::to_int(val);
                        (c > 0).then(|| (k.clone(), c))
                    })
                    .collect();
                if mutable {
                    Value::bag_hash(counts)
                } else {
                    Value::bag(counts)
                }
            }
            QuantKind::Mix => {
                let weights: std::collections::HashMap<String, f64> = map
                    .iter()
                    .filter_map(|(k, val)| {
                        crate::runtime::utils::to_float_value(val).map(|w| (k.clone(), w))
                    })
                    .collect();
                if mutable {
                    Value::mix_hash(weights)
                } else {
                    Value::mix(weights)
                }
            }
        }
    }
}
