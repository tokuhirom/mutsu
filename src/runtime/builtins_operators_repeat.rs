use super::*;
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::ValueView;
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
        while let ValueView::Mixin(inner, _) = current.view() {
            current = inner;
        }
        match current.view() {
            ValueView::Whatever => Ok(None),
            ValueView::Int(i) => Ok(Some(i)),
            ValueView::BigInt(n) => Ok(Some(n.to_i64().unwrap_or(i64::MAX))),
            ValueView::Num(f) => {
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
            ValueView::Rat(n, d) => {
                if d == 0 {
                    if n > 0 {
                        return Ok(None);
                    }
                    let msg = if n < 0 {
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
            ValueView::FatRat(n, d) => {
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
            ValueView::BigRat(n, d) => {
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
            ValueView::Str(s) => {
                let parsed = s.trim().parse::<f64>().map_err(|_| {
                    Self::repeat_error(
                        "X::Str::Numeric",
                        format!("Cannot convert string '{}' to a number", *s),
                    )
                })?;
                Self::parse_repeat_count(&Value::num(parsed))
            }
            ValueView::Array(items, ..) => Ok(Some(items.len() as i64)),
            ValueView::Seq(items) => Ok(Some(items.len() as i64)),
            ValueView::LazyList(ll) => Ok(Some(
                ll.cache
                    .lock()
                    .unwrap_or_else(|e| e.into_inner())
                    .as_ref()
                    .map_or(0usize, |v| v.len()) as i64,
            )),
            ValueView::Package(_) => Ok(Some(0)),
            _ => Ok(Some(0)),
        }
    }

    /// Build a lazy-cached repeat list that records the repeat's *logical* element
    /// count (which may far exceed the materialized cache, or be infinite), so
    /// `.elems` / `.iterator.count-only` report the true count of `LHS xx N`
    /// without materializing N elements.
    /// The largest `xx` count that is materialized eagerly.
    pub(crate) const REPEAT_EAGER_LIMIT: usize = 1_000_000;

    /// `LHS xx COUNT` -- the single implementation behind the VM opcode, the
    /// runtime operator fallback and the `[xx]` reduction.
    ///
    /// A finite count is eager in Raku (all N elements are built, `.is-lazy`
    /// is False), so any count up to `EAGER_LIMIT` is materialized. Above
    /// that -- an astronomically large count (`42 xx 2**62`) or an infinite
    /// one (`xx *`) -- the result is a lazy `PipeAdaptor::Repeat` stage that
    /// builds each repetition only when it is pulled; no prefix is cached up
    /// front, so `(42 xx *)[10**5]` is `42` rather than `Nil` (#9159).
    // Cost: O(k * s) for a count k <= 10**6, s = Slip width (every repetition
    // built eagerly, as in Rakudo); O(1) otherwise, then O(s) per repetition
    // pulled.
    pub(crate) fn list_repeat(
        &mut self,
        left: &Value,
        right: &Value,
    ) -> Result<Value, RuntimeError> {
        const EAGER_LIMIT: usize = Interpreter::REPEAT_EAGER_LIMIT;
        // Warn on uninitialized type object used as repeat count
        if let ValueView::Package(name) = right.view()
            && name == "Int"
        {
            self.warn_uninitialized_repeat_count(&name.resolve())?;
        }
        let count = Self::parse_repeat_count(right)?;
        match count {
            Some(n) if n <= 0 => Ok(Value::seq(Vec::new())),
            Some(n) if (n as usize) <= EAGER_LIMIT => {
                let mut items = Vec::with_capacity(n as usize);
                for _ in 0..n {
                    self.repeat_lhs_into(left, &mut items)?;
                }
                Ok(Value::seq(items))
            }
            _ => Ok(Self::repeat_lazy_value(left, right, count)),
        }
    }

    /// The lazy result of `LHS xx COUNT` for a count above the eager limit
    /// (`count` is the parsed count, `None` for `*`/`Inf`).
    // Cost: O(1).
    pub(crate) fn repeat_lazy_value(left: &Value, right: &Value, count: Option<i64>) -> Value {
        let mut ll = crate::value::LazyList::new_adaptor_pipe(
            Value::NIL,
            left.clone(),
            crate::value::PipeAdaptor::Repeat {
                remaining: count.map(|n| n as u64),
            },
        );
        // `(42 xx *).is-lazy` is True and `.elems` reports the logical
        // count; nothing else in the value records it.
        ll.elems_count = Some(Self::repeat_logical_count(right));
        Value::lazy_list(crate::gc::Gc::new(ll))
    }

    /// Append one repetition of an `xx` LHS to `out`: a Slip LHS contributes
    /// its elements (an empty one a single `Nil`), and so does a callable LHS
    /// whose call yields a Slip (`Slip(1,2) xx *` is 1,2,1,2,...).
    // Cost: O(s), s = Slip width, plus one call for a callable LHS.
    pub(crate) fn repeat_lhs_into(
        &mut self,
        left: &Value,
        out: &mut Vec<Value>,
    ) -> Result<(), RuntimeError> {
        if let ValueView::Slip(slip_items) = left.view() {
            if slip_items.is_empty() {
                out.push(Value::NIL);
            } else {
                out.extend(slip_items.iter().cloned());
            }
            return Ok(());
        }
        let v = self.repeat_lhs_once(left)?;
        if let ValueView::Slip(sub) = v.view() {
            out.extend(sub.iter().cloned());
        } else {
            out.push(v);
        }
        Ok(())
    }

    /// The logical element count of `LHS xx right` when the result is lazy
    /// (`right` exceeded the eager limit or is infinite). `*`/`∞`/`Inf` map to
    /// `Inf`; a finite count keeps its exact (possibly big) integer value.
    pub(crate) fn repeat_logical_count(right: &Value) -> Value {
        match right.view() {
            ValueView::Whatever | ValueView::HyperWhatever => Value::num(f64::INFINITY),
            ValueView::Num(n) if n.is_infinite() => Value::num(n),
            ValueView::Int(_) | ValueView::BigInt(_) => right.clone(),
            _ => {
                let f = right.to_f64();
                if f.is_infinite() {
                    Value::num(f)
                } else {
                    Value::int(f as i64)
                }
            }
        }
    }

    pub(crate) fn repeat_lhs_once(&mut self, left: &Value) -> Result<Value, RuntimeError> {
        match left.view() {
            ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. } => {
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
            ValueView::Seq(items) => Ok(Value::array(items.to_vec())),
            // xx thunks the LHS: each repetition must be an independent copy
            ValueView::Array(items, kind) => Ok(Value::array_with_kind(
                crate::gc::Gc::new(items.as_ref().clone()),
                kind,
            )),
            ValueView::Hash(map) => {
                Ok(Value::hash_with_data(Value::hash_arc(map.as_ref().clone())))
            }
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
                    if let ValueView::Package(name) = rhs.view()
                        && name == "Int"
                    {
                        self.warn_uninitialized_repeat_count(&name.resolve())?;
                    }
                    if matches!(rhs.view(), ValueView::Whatever) {
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
                    // Infix `x` stringifies its LEFT operand via `.Str` (the
                    // right operand is a repeat count and must stay numeric,
                    // so it is NOT run through this) — mirrors
                    // `exec_string_repeat_op` / `exec_concat_op`. This is an
                    // internal redispatch with no surrounding CallMethod op,
                    // so drain any captured-outer writeback into the caller's
                    // slot.
                    let caller_code = self.current_code;
                    acc = self.coerce_stringy_operand(acc)?;
                    self.reconcile_caller_after_internal_dispatch(caller_code);
                    acc = crate::builtins::str_prim::repeat(&acc, n)?;
                }
                "xx" => {
                    acc = self.list_repeat(&acc, rhs)?;
                }
                _ => unreachable!(),
            }
        }
        Ok(acc)
    }
}
