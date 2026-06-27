use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(super) fn dispatch_substr_eq(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Separate positional and named args
        let mut positional: Vec<Value> = Vec::new();
        let mut ignore_case = false;
        let mut ignore_mark = false;
        for arg in args {
            if let Value::Pair(key, value) = arg {
                match key.as_str() {
                    "i" | "ignorecase" => ignore_case = value.truthy(),
                    "m" | "ignoremark" => ignore_mark = value.truthy(),
                    _ => {}
                }
            } else {
                positional.push(arg.clone());
            }
        }
        if positional.is_empty() {
            return Err(RuntimeError::new(
                "Too few positionals passed to 'substr-eq'",
            ));
        }
        // Type objects (Package) as needle should throw
        if let Value::Package(type_name) = &positional[0] {
            return Err(RuntimeError::new(format!(
                "Cannot resolve caller substr-eq({}:U)",
                type_name
            )));
        }
        let text = target.to_string_value();
        let needle = positional[0].to_string_value();
        let len = text.chars().count() as i64;
        let start = if let Some(pos) = positional.get(1) {
            match self.substr_resolve_position(pos, len as usize) {
                Ok(v) => v,
                Err(err) => return Ok(Self::runtime_error_to_failure(err)),
            }
        } else {
            0
        };
        if start < 0 || start > len {
            return Ok(RuntimeError::out_of_range_failure(
                "start",
                Value::Int(start),
                &format!("0..{}", len),
            ));
        }
        let substr: String = text
            .chars()
            .skip(start as usize)
            .take(needle.chars().count())
            .collect();
        let eq = match (ignore_case, ignore_mark) {
            (false, false) => substr == needle,
            (true, false) => substr.to_lowercase() == needle.to_lowercase(),
            (false, true) => self.strip_marks(&substr) == self.strip_marks(&needle),
            (true, true) => {
                self.strip_marks(&substr).to_lowercase() == self.strip_marks(&needle).to_lowercase()
            }
        };
        Ok(Value::Bool(eq))
    }

    pub(super) fn dispatch_substr(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let s = target.to_string_value();
        let chars: Vec<char> = s.chars().collect();
        let total_len = chars.len();

        // Check if first arg is a Range — handle substr($str, 6..8) form
        if let Some(first_arg) = args.first()
            && let Some((range_start, range_end)) =
                self.substr_extract_range(first_arg, total_len)?
        {
            let rs = range_start.min(total_len);
            let re = range_end.min(total_len);
            return Ok(Value::str(chars[rs..re].iter().collect()));
        }

        // First arg: start position
        let start_raw: i64 = if let Some(pos) = args.first() {
            self.substr_resolve_position(pos, total_len)?
        } else {
            0
        };

        // Resolve negative start (from WhateverCode calling convention)
        let start = if start_raw < 0 {
            total_len as i64 + start_raw
        } else {
            start_raw
        };

        // Out-of-range check: return Failure wrapping X::OutOfRange
        if start < 0 || start as usize > total_len {
            return self.substr_out_of_range_failure(start, total_len);
        }

        let start = start as usize;

        // Second arg: length (can be Int, WhateverCode/Sub, Num/Inf, or absent)
        let end = if let Some(len_val) = args.get(1) {
            match len_val {
                Value::Int(i) => {
                    let len = (*i).max(0) as usize;
                    (start + len).min(total_len)
                }
                Value::Num(f) if f.is_infinite() && *f > 0.0 => total_len,
                Value::Num(f) => {
                    let len = (*f as i64).max(0) as usize;
                    (start + len).min(total_len)
                }
                Value::Rat(n, d) if *d != 0 => {
                    let len = (*n / *d).max(0) as usize;
                    (start + len).min(total_len)
                }
                Value::Sub { .. } => {
                    // WhateverCode: call with remaining length to get actual length
                    let remaining = if start <= total_len {
                        (total_len - start) as i64
                    } else {
                        0
                    };
                    let result =
                        self.eval_call_on_value(len_val.clone(), vec![Value::Int(remaining)])?;
                    let len = match &result {
                        Value::Int(i) => (*i).max(0) as usize,
                        Value::Num(f) => (*f as i64).max(0) as usize,
                        Value::Rat(n, d) if *d != 0 => (*n / *d).max(0) as usize,
                        _ => 0,
                    };
                    (start + len).min(total_len)
                }
                _ => total_len, // default: take rest
            }
        } else {
            total_len // no length: take rest
        };

        Ok(Value::str(chars[start..end].iter().collect()))
    }

    /// substr-rw in non-lvalue context: just return the substring (same as substr).
    /// When a variable name is available, returns a Proxy for binding support.
    pub(super) fn dispatch_substr_rw(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        self.dispatch_substr(target, args)
    }

    /// Create a Proxy for substr-rw binding.
    /// The Proxy's FETCH returns the current substring and STORE modifies the original string.
    pub(crate) fn make_substr_rw_proxy(
        &mut self,
        var_name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let target = self
            .env
            .get(var_name)
            .cloned()
            .unwrap_or(Value::str(String::new()));
        let s = target.to_string_value();
        let chars: Vec<char> = s.chars().collect();
        let str_len = chars.len();

        // Resolve range
        let (start, end) = self.resolve_substr_rw_range(args, str_len)?;
        let len = end - start;

        let make_param_def = |name: &str| crate::ast::ParamDef {
            name: name.to_string(),
            default: None,
            multi_invocant: false,
            required: false,
            named: false,
            slurpy: false,
            double_slurpy: false,
            onearg: false,
            sigilless: false,
            type_constraint: None,
            literal_value: None,
            sub_signature: None,
            where_constraint: None,
            traits: Vec::new(),
            optional_marker: false,
            outer_sub_signature: None,
            code_signature: None,
            is_invocant: false,
            shape_constraints: None,
        };

        // Create FETCH sub: reads substr from the variable
        let fetch_body = vec![crate::ast::Stmt::Expr(crate::ast::Expr::MethodCall {
            target: Box::new(crate::ast::Expr::Var(var_name.to_string())),
            name: crate::symbol::Symbol::intern("substr"),
            args: vec![
                crate::ast::Expr::Literal(Value::Int(start as i64)),
                crate::ast::Expr::Literal(Value::Int(len as i64)),
            ],
            modifier: None,
            quoted: false,
        })];
        let fetcher = Value::make_sub(
            crate::symbol::Symbol::intern(""),
            crate::symbol::Symbol::intern("__substr_rw_fetch"),
            vec!["$".to_string()],
            vec![make_param_def("$")],
            fetch_body,
            false,
            self.env.clone(),
        );

        // Create STORE sub: modifies the variable via substr-rw lvalue assignment
        let store_body = vec![crate::ast::Stmt::Expr(crate::ast::Expr::Call {
            name: crate::symbol::Symbol::intern("__mutsu_assign_named_sub_lvalue"),
            args: vec![
                crate::ast::Expr::Literal(Value::str("substr-rw".to_string())),
                crate::ast::Expr::ArrayLiteral(vec![
                    crate::ast::Expr::Var(var_name.to_string()),
                    crate::ast::Expr::Literal(Value::Int(start as i64)),
                    crate::ast::Expr::Literal(Value::Int(len as i64)),
                ]),
                crate::ast::Expr::Var("__mutsu_substr_rw_store_value".to_string()),
            ],
        })];
        let storer = Value::make_sub(
            crate::symbol::Symbol::intern(""),
            crate::symbol::Symbol::intern("__substr_rw_store"),
            vec!["$".to_string(), "__mutsu_substr_rw_store_value".to_string()],
            vec![
                make_param_def("$"),
                make_param_def("__mutsu_substr_rw_store_value"),
            ],
            store_body,
            false,
            self.env.clone(),
        );

        Ok(Value::Proxy {
            fetcher: Box::new(fetcher),
            storer: Box::new(storer),
            subclass: None,
            decontainerized: false,
        })
    }

    /// Resolve a position argument to an i64, handling Int, Num, Rat, BigInt, WhateverCode/Sub.
    pub(crate) fn substr_resolve_position(
        &mut self,
        pos: &Value,
        total_len: usize,
    ) -> Result<i64, RuntimeError> {
        match pos {
            Value::Int(i) => Ok(*i),
            Value::Num(f) => Ok(*f as i64),
            Value::Rat(n, d) if *d != 0 => Ok(*n / *d),
            Value::BigInt(b) => {
                if b.as_ref() > &num_bigint::BigInt::from(i64::MAX)
                    || b.as_ref() < &num_bigint::BigInt::from(i64::MIN)
                {
                    Err(self.out_of_range_error(Value::bigint((**b).clone())))
                } else {
                    Ok(b.to_string().parse::<i64>().unwrap_or(0))
                }
            }
            Value::Sub { .. } => {
                // WhateverCode/Callable: call with total_len to resolve position
                let result =
                    self.eval_call_on_value(pos.clone(), vec![Value::Int(total_len as i64)])?;
                match &result {
                    Value::Int(i) => Ok(*i),
                    Value::Num(f) => Ok(*f as i64),
                    Value::Rat(n, d) if *d != 0 => Ok(*n / *d),
                    _ => Ok(0),
                }
            }
            other => Ok(other.to_string_value().parse::<i64>().unwrap_or(0)),
        }
    }

    /// Extract start/end indices from a Range value for substr.
    /// Returns Some((start, end)) if the value is a Range, None otherwise.
    pub(crate) fn substr_extract_range(
        &mut self,
        val: &Value,
        total_len: usize,
    ) -> Result<Option<(usize, usize)>, RuntimeError> {
        match val {
            Value::Range(a, b) => {
                let end = b.saturating_add(1).max(0) as usize;
                Ok(Some((*a as usize, end)))
            }
            Value::RangeExcl(a, b) => Ok(Some((*a as usize, *b as usize))),
            Value::RangeExclStart(a, b) => {
                let end = b.saturating_add(1).max(0) as usize;
                Ok(Some(((*a + 1) as usize, end)))
            }
            Value::RangeExclBoth(a, b) => Ok(Some(((*a + 1) as usize, *b as usize))),
            Value::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                let s = self.substr_resolve_position(start, total_len)?;
                let s = if *excl_start { s + 1 } else { s };
                let e_val = self.substr_resolve_position(end, total_len)?;
                // For Inf end (e.g., 10..*), e_val will be very large; clamp later
                let e = if *excl_end { e_val } else { e_val + 1 };
                let s = s.max(0) as usize;
                let e = e.max(0) as usize;
                Ok(Some((s, e)))
            }
            _ => Ok(None),
        }
    }

    /// Return a Failure wrapping X::OutOfRange for substr out-of-range start.
    fn substr_out_of_range_failure(
        &self,
        start: i64,
        total_len: usize,
    ) -> Result<Value, RuntimeError> {
        let mut ex_attrs = std::collections::HashMap::new();
        ex_attrs.insert(
            "what".to_string(),
            Value::str("Start argument to substr".to_string()),
        );
        ex_attrs.insert("got".to_string(), Value::str(start.to_string()));
        ex_attrs.insert("range".to_string(), Value::str(format!("0..{}", total_len)));
        ex_attrs.insert(
            "message".to_string(),
            Value::str("X::OutOfRange".to_string()),
        );
        let exception = Value::make_instance(Symbol::intern("X::OutOfRange"), ex_attrs);
        let mut failure_attrs = std::collections::HashMap::new();
        failure_attrs.insert("exception".to_string(), exception);
        failure_attrs.insert("handled".to_string(), Value::Bool(false));
        Ok(Value::make_instance(
            Symbol::intern("Failure"),
            failure_attrs,
        ))
    }
}
