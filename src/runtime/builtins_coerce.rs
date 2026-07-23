use super::*;
use crate::value::ValueView;

impl Interpreter {
    /// The container-type coercion calls `Array(...)` / `List(...)` / `Hash(...)`.
    /// Each argument becomes one element (Raku does not deep-flatten these:
    /// `Array((1,2), 3).elems` is 2), so the args list is materialized directly.
    /// A single type-object argument (`Array(Int)`) is a parametric type request
    /// rather than a value coercion, so it passes through as a `Type(Type)`
    /// package rendering, mirroring [`builtin_coerce`].
    pub(super) fn builtin_container_coerce(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // `Array(Int)` etc.: a lone type-object argument is a parametric type,
        // not a value list. Render it like the scalar coercions do.
        if args.len() == 1
            && let ValueView::Package(sym) = args[0].view()
        {
            return Ok(Value::package(Symbol::intern(&format!(
                "{name}({})",
                sym.resolve()
            ))));
        }
        let items: Vec<Value> = args.to_vec();
        Ok(match name {
            "Array" => Value::real_array(items),
            "List" => Value::array(items),
            "Hash" => self.build_hash_from_items_warning(items)?,
            _ => Value::real_array(items),
        })
    }

    pub(super) fn builtin_coerce(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let Some(value) = args.first().cloned() else {
            // `Int()` with no args is a coercion type term `Int(Any)`, not a call.
            return Ok(Value::package(Symbol::intern(&format!("{name}(Any)"))));
        };
        if let Some(source) = match value.view() {
            ValueView::Package(sym) => Some(sym.resolve()),
            ValueView::Nil => Some("Any".to_string()),
            _ => None,
        } {
            return Ok(Value::package(Symbol::intern(&format!("{name}({source})"))));
        }
        let coerced = match name {
            // `Rat($x)` / `FatRat($x)` / `Complex($x)` delegate to the method
            // form (`$x.Rat` …), which carries the full native coercion logic
            // incl. Failure semantics for invalid strings (JSON::Unmarshal's
            // `multi _unmarshal(Any:D $json, Rat) { Rat($json) }`).
            "Rat" | "FatRat" | "Complex" => {
                return self.call_method_with_values(value.clone(), name, vec![]);
            }
            "Int" => match value.view() {
                ValueView::Int(i) => Value::int(i),
                ValueView::Num(f) => Value::int(f as i64),
                ValueView::Rat(_, 0) => {
                    return Ok(RuntimeError::divide_by_zero_failure_for_method(
                        "Int", "Rational",
                    ));
                }
                ValueView::Rat(n, d) => Value::int(n / d),
                ValueView::FatRat(_, 0) => {
                    return Ok(RuntimeError::divide_by_zero_failure_for_method(
                        "Int", "Rational",
                    ));
                }
                ValueView::FatRat(n, d) => Value::int(n / d),
                ValueView::Complex(r, _) => Value::int(r as i64),
                // Delegate to `Str.Int` so an invalid string yields the same
                // X::Str::Numeric Failure as the method form (`"abc".Int`),
                // rather than silently coercing to 0.
                ValueView::Str(_) => {
                    return self.call_method_with_values(value.clone(), name, vec![]);
                }
                ValueView::Bool(b) => Value::int(if b { 1 } else { 0 }),
                _ => Value::int(0),
            },
            "Num" => match value.view() {
                ValueView::Int(i) => Value::num(i as f64),
                ValueView::Num(f) => Value::num(f),
                ValueView::Rat(n, d) => {
                    if d == 0 {
                        Value::num(if n == 0 {
                            f64::NAN
                        } else if n > 0 {
                            f64::INFINITY
                        } else {
                            f64::NEG_INFINITY
                        })
                    } else {
                        Value::num(n as f64 / d as f64)
                    }
                }
                ValueView::Complex(r, im) => {
                    let tolerance = self
                        .get_dynamic_var("*TOLERANCE")
                        .ok()
                        .and_then(|v| match v.view() {
                            ValueView::Num(n) => Some(n),
                            ValueView::Rat(n, d) if d != 0 => Some(n as f64 / d as f64),
                            ValueView::Int(n) => Some(n as f64),
                            _ => None,
                        })
                        .unwrap_or(1e-15);
                    if im.abs() > tolerance {
                        let msg = format!(
                            "Cannot convert {}{}{}i to Num: imaginary part not zero",
                            r,
                            if im >= 0.0 { "+" } else { "" },
                            im
                        );
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("message".to_string(), Value::str(msg.clone()));
                        attrs.insert(
                            "target".to_string(),
                            Value::package(crate::symbol::Symbol::intern("Num")),
                        );
                        attrs.insert("source".to_string(), value.clone());
                        let ex = Value::make_instance(
                            crate::symbol::Symbol::intern("X::Numeric::Real"),
                            attrs,
                        );
                        let mut err = RuntimeError::new(msg);
                        err.exception = Some(Box::new(ex));
                        return Err(err);
                    }
                    Value::num(r)
                }
                // Delegate to `Str.Num` so an invalid string yields the same
                // X::Str::Numeric Failure as the method form, not a silent 0.
                ValueView::Str(_) => {
                    return self.call_method_with_values(value.clone(), name, vec![]);
                }
                ValueView::Bool(b) => Value::num(if b { 1.0 } else { 0.0 }),
                _ => Value::num(0.0),
            },
            "Str" => self.call_method_with_values(value, "Str", vec![])?,
            "Bool" => Value::truth(value.truthy()),
            "Uni" => {
                // Uni(codepoint) creates a Uni from a single codepoint value
                let cp = match value.view() {
                    ValueView::Int(i) => i as u32,
                    ValueView::Num(f) => f as u32,
                    _ => value.to_string_value().parse::<u32>().unwrap_or(0),
                };
                let text: String = char::from_u32(cp).into_iter().collect();
                Value::uni(String::new(), text)
            }
            _ => Value::NIL,
        };
        Ok(coerced)
    }
}
