use super::*;
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use num_traits::{Signed, ToPrimitive, Zero};

impl Interpreter {
    pub(crate) fn repeat_error(class_name: &str, message: String) -> RuntimeError {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(message.clone()));
        let ex = Value::make_instance(Symbol::intern(class_name), attrs);
        let mut err = RuntimeError::new(message);
        err.exception = Some(Box::new(ex));
        err
    }

    pub(crate) fn parse_repeat_count(value: &Value) -> Result<Option<i64>, RuntimeError> {
        let mut current = value;
        while let Value::Mixin(inner, _) = current {
            current = inner;
        }
        match current {
            Value::Whatever => Ok(None),
            Value::Int(i) => Ok(Some(*i)),
            Value::BigInt(n) => Ok(Some(n.to_i64().unwrap_or(i64::MAX))),
            Value::Num(f) => {
                if f.is_nan() {
                    return Err(Self::repeat_error(
                        "X::Numeric::CannotConvert",
                        "Cannot convert NaN to Int".to_string(),
                    ));
                }
                if f.is_infinite() {
                    if f.is_sign_positive() {
                        return Ok(None);
                    }
                    return Err(Self::repeat_error(
                        "X::Numeric::CannotConvert",
                        "Cannot convert -Inf to Int".to_string(),
                    ));
                }
                Ok(Some(f.trunc() as i64))
            }
            Value::Rat(n, d) => {
                if *d == 0 {
                    if *n > 0 {
                        return Ok(None);
                    }
                    let msg = if *n < 0 {
                        "Cannot convert -Inf to Int"
                    } else {
                        "Cannot convert NaN to Int"
                    };
                    return Err(Self::repeat_error(
                        "X::Numeric::CannotConvert",
                        msg.to_string(),
                    ));
                }
                Ok(Some(n / d))
            }
            Value::FatRat(n, d) => {
                if d.is_zero() {
                    if n.is_positive() {
                        return Ok(None);
                    }
                    let msg = if n.is_negative() {
                        "Cannot convert -Inf to Int"
                    } else {
                        "Cannot convert NaN to Int"
                    };
                    return Err(Self::repeat_error(
                        "X::Numeric::CannotConvert",
                        msg.to_string(),
                    ));
                }
                Ok(Some((n / d).to_i64().unwrap_or(i64::MAX)))
            }
            Value::BigRat(n, d) => {
                if d.is_zero() {
                    if n.is_positive() {
                        return Ok(None);
                    }
                    let msg = if n.is_negative() {
                        "Cannot convert -Inf to Int"
                    } else {
                        "Cannot convert NaN to Int"
                    };
                    return Err(Self::repeat_error(
                        "X::Numeric::CannotConvert",
                        msg.to_string(),
                    ));
                }
                Ok(Some((n.as_ref() / d.as_ref()).to_i64().unwrap_or(i64::MAX)))
            }
            Value::Str(s) => {
                let parsed = s.trim().parse::<f64>().map_err(|_| {
                    Self::repeat_error(
                        "X::Str::Numeric",
                        format!("Cannot convert string '{}' to a number", s),
                    )
                })?;
                Self::parse_repeat_count(&Value::Num(parsed))
            }
            Value::Array(items, ..) => Ok(Some(items.len() as i64)),
            Value::Seq(items) => Ok(Some(items.len() as i64)),
            Value::LazyList(ll) => Ok(Some(
                ll.cache
                    .lock()
                    .unwrap_or_else(|e| e.into_inner())
                    .as_ref()
                    .map_or(0usize, |v| v.len()) as i64,
            )),
            Value::Package(_) => Ok(Some(0)),
            _ => Ok(Some(0)),
        }
    }

    /// Build a lazy-cached repeat list that records the repeat's *logical* element
    /// count (which may far exceed the materialized cache, or be infinite), so
    /// `.elems` / `.iterator.count-only` report the true count of `LHS xx N`
    /// without materializing N elements.
    pub(crate) fn make_repeat_lazy_cache_counted(items: Vec<Value>, count: Value) -> Value {
        let mut ll = crate::value::LazyList::new_cached(items);
        ll.elems_count = Some(count);
        Value::LazyList(std::sync::Arc::new(ll))
    }

    /// The logical element count of `LHS xx right` when the result is lazy
    /// (`right` exceeded the eager limit or is infinite). `*`/`∞`/`Inf` map to
    /// `Inf`; a finite count keeps its exact (possibly big) integer value.
    pub(crate) fn repeat_logical_count(right: &Value) -> Value {
        match right {
            Value::Whatever | Value::HyperWhatever => Value::Num(f64::INFINITY),
            Value::Num(n) if n.is_infinite() => Value::Num(*n),
            Value::Int(_) | Value::BigInt(_) => right.clone(),
            other => {
                let f = other.to_f64();
                if f.is_infinite() {
                    Value::Num(f)
                } else {
                    Value::Int(f as i64)
                }
            }
        }
    }

    pub(crate) fn repeat_lhs_once(&mut self, left: &Value) -> Result<Value, RuntimeError> {
        match left {
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. } => {
                let saved_topic = self.env.get("_").cloned();
                let result = self.eval_call_on_value(left.clone(), Vec::new());
                match saved_topic {
                    Some(value) => {
                        self.env.insert("_".to_string(), value);
                    }
                    None => {
                        self.env.remove("_");
                    }
                }
                result
            }
            // Seq → List when used as xx LHS (Raku caches/listifies Seq on repeat)
            Value::Seq(items) => Ok(Value::array(items.as_ref().clone())),
            // xx thunks the LHS: each repetition must be an independent copy
            Value::Array(items, kind) => Ok(Value::Array(Arc::new(items.as_ref().clone()), *kind)),
            Value::Hash(map) => Ok(Value::Hash(Value::hash_arc(map.as_ref().clone()))),
            _ => Ok(left.clone()),
        }
    }

    pub(crate) fn make_x_whatevercode(&self, left: Value) -> Value {
        let mut env = crate::env::Env::new();
        env.insert(
            "__mutsu_callable_type".to_string(),
            Value::str_from("WhateverCode"),
        );
        let param = "__wc_0".to_string();
        let body = vec![Stmt::Expr(Expr::Binary {
            left: Box::new(Expr::Literal(left)),
            op: TokenKind::Ident("x".to_string()),
            right: Box::new(Expr::Var(param.clone())),
        })];
        Value::make_sub(
            Symbol::intern(&self.current_package()),
            Symbol::intern("<whatevercode-x>"),
            vec![param],
            Vec::new(),
            body,
            false,
            env,
        )
    }

    pub(super) fn call_repeat_infix(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            if op == "xx" {
                return Err(Self::repeat_error(
                    "Exception",
                    "xx with no args throws".to_string(),
                ));
            }
            return Ok(reduction_identity(op));
        }
        if args.len() == 1 {
            return Ok(args[0].clone());
        }

        let mut acc = args[0].clone();
        for rhs in &args[1..] {
            match op {
                "x" => {
                    if let Value::Package(name) = rhs
                        && name == "Int"
                        && !self.warning_suppressed()
                    {
                        self.write_warn_to_stderr(&format!(
                            "Use of uninitialized value of type {} in numeric context",
                            name
                        ));
                    }
                    if matches!(rhs, Value::Whatever) {
                        acc = self.make_x_whatevercode(acc);
                        continue;
                    }
                    let Some(n_raw) = Self::parse_repeat_count(rhs)? else {
                        return Err(Self::repeat_error(
                            "X::Numeric::CannotConvert",
                            "Cannot convert Inf to Int".to_string(),
                        ));
                    };
                    let n = n_raw.max(0) as usize;
                    let repeated = crate::runtime::utils::coerce_to_str(&acc).repeat(n);
                    // NFC-normalize after repetition: combining marks from the end
                    // of one copy may interact with the start of the next copy
                    use unicode_normalization::UnicodeNormalization;
                    acc = Value::str(repeated.nfc().collect::<String>());
                }
                "xx" => {
                    const EAGER_LIMIT: usize = 10_000;
                    const LAZY_CACHE: usize = 4_096;
                    // Callable LHS is expensive (each iteration calls eval_call_on_value),
                    // so use a much smaller cache to avoid timeouts on `callable xx *`.
                    const LAZY_CACHE_CALLABLE: usize = 256;
                    if let Value::Package(name) = rhs
                        && name == "Int"
                        && !self.warning_suppressed()
                    {
                        self.write_warn_to_stderr(&format!(
                            "Use of uninitialized value of type {} in numeric context",
                            name
                        ));
                    }
                    let is_callable = matches!(
                        acc,
                        Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
                    );
                    let lazy_cache = if is_callable {
                        LAZY_CACHE_CALLABLE
                    } else {
                        LAZY_CACHE
                    };
                    let count = Self::parse_repeat_count(rhs)?;
                    let (repeat, lazy) = match count {
                        Some(n) if n <= 0 => (0usize, false),
                        Some(n) if (n as usize) <= EAGER_LIMIT => (n as usize, false),
                        Some(n) => ((n as usize).min(lazy_cache), true),
                        None => (lazy_cache, true),
                    };
                    let mut items = Vec::with_capacity(repeat);
                    if let Value::Slip(slip_items) = &acc {
                        if slip_items.is_empty() {
                            items.extend(std::iter::repeat_n(Value::Nil, repeat));
                        } else {
                            for _ in 0..repeat {
                                items.extend(slip_items.iter().cloned());
                            }
                        }
                    } else {
                        for _ in 0..repeat {
                            items.push(self.repeat_lhs_once(&acc)?);
                        }
                    }
                    acc = if lazy {
                        let count = Self::repeat_logical_count(rhs);
                        Self::make_repeat_lazy_cache_counted(items, count)
                    } else {
                        Value::Seq(std::sync::Arc::new(items))
                    };
                }
                _ => unreachable!(),
            }
        }
        Ok(acc)
    }
}
