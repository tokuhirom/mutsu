//! The `Format` class (Raku 6.e): a `Callable` wrapper around a sprintf-style
//! format string. `Format.new("%5s")` produces an object that stringifies to the
//! format, is callable (`$f("foo")` -> sprintf), and integrates with `.fmt`.
//!
//! Authoritative semantics mirror Rakudo's `src/core.e/Format.rakumod` and the
//! `.fmt(Format, $sep)` augmentations in `src/core.e/Fixups.rakumod`.

use std::collections::HashMap;

use crate::ast::{Expr, ParamDef, Stmt};
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};

use super::Interpreter;

/// Build a required positional `ParamDef` with the given (sigilless) name.
fn positional_param(name: &str) -> ParamDef {
    ParamDef {
        name: name.to_string(),
        default: None,
        multi_invocant: true,
        required: true,
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
    }
}

impl Interpreter {
    /// True if `value` is a `Format` instance.
    pub(super) fn is_format_instance(value: &Value) -> bool {
        matches!(value, Value::Instance { class_name, .. } if class_name.resolve() == "Format")
    }

    /// Read the format string out of a `Format` instance.
    fn format_string_of(value: &Value) -> Option<String> {
        match value {
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name.resolve() == "Format" => Some(
                attributes
                    .as_map()
                    .get("format")
                    .map(Value::to_string_value)
                    .unwrap_or_default(),
            ),
            _ => None,
        }
    }

    /// Build the underlying `Callable` (a `Sub`) for a format string. The sub has
    /// `count` required positional parameters and a body of `sprintf($fmt, ...)`.
    pub(super) fn format_callable(&self, fmt: &str, count: usize) -> Value {
        let mut params = Vec::with_capacity(count);
        let mut param_defs = Vec::with_capacity(count);
        let mut call_args: Vec<Expr> = Vec::with_capacity(count + 1);
        call_args.push(Expr::Literal(Value::str(fmt.to_string())));
        for i in 0..count {
            let pname = format!("a{i}");
            params.push(pname.clone());
            param_defs.push(positional_param(&pname));
            call_args.push(Expr::Var(pname));
        }
        let body = vec![Stmt::Expr(Expr::Call {
            name: Symbol::intern("sprintf"),
            args: call_args,
        })];
        Value::make_sub(
            Symbol::intern("Formatter"),
            Symbol::intern("format"),
            params,
            param_defs,
            body,
            false,
            self.env.clone(),
        )
    }

    /// Apply a format to a list of arguments, validating arity and types.
    /// This is the `CALL-ME` behaviour and the per-batch renderer used by `.fmt`.
    fn format_render(&self, fmt: &str, args: &[Value]) -> Result<Value, RuntimeError> {
        super::sprintf::validate_sprintf_directives(fmt, args.len())?;
        super::sprintf::validate_sprintf_arg_types(fmt, args)?;
        Ok(Value::str(super::sprintf::format_sprintf_args(fmt, args)))
    }

    /// Throw `X::Str::Sprintf::Directives::Count` for an arity mismatch during `.fmt`.
    fn format_throw_arity(fmt: &str, args_have: usize, args_used: usize) -> RuntimeError {
        let mut attrs = HashMap::new();
        attrs.insert("args-have".to_string(), Value::Int(args_have as i64));
        attrs.insert("args-used".to_string(), Value::Int(args_used as i64));
        attrs.insert("format".to_string(), Value::str(fmt.to_string()));
        let message = format!(
            "Your printf-style directives specify {} argument{}, but {} {} supplied",
            args_used,
            if args_used == 1 { "" } else { "s" },
            if args_have < 1 {
                "no".to_string()
            } else {
                args_have.to_string()
            },
            if args_have == 1 {
                "argument was"
            } else {
                "arguments were"
            },
        );
        attrs.insert("message".to_string(), Value::str(message.clone()));
        let mut err = RuntimeError::new(message);
        err.exception = Some(Box::new(Value::make_instance(
            Symbol::intern("X::Str::Sprintf::Directives::Count"),
            attrs,
        )));
        err
    }

    /// `Format.handle-iterator`: render a sequence of values, batching by the
    /// format's directive count and joining with `separator`.
    fn format_handle_iterator(
        &self,
        fmt: &str,
        count: usize,
        items: &[Value],
        separator: &str,
    ) -> Result<Value, RuntimeError> {
        // No-arg format: empty input renders to "", any value is an arity error.
        if count == 0 {
            if items.is_empty() {
                return Ok(Value::str(String::new()));
            }
            return Err(Self::format_throw_arity(fmt, 0, 1));
        }

        let mut parts: Vec<String> = Vec::new();
        let mut idx = 0;
        while idx < items.len() {
            let end = (idx + count).min(items.len());
            let batch = &items[idx..end];
            if batch.len() < count {
                // A non-empty short batch is an arity error.
                return Err(Self::format_throw_arity(fmt, batch.len(), count));
            }
            parts.push(self.format_render(fmt, batch)?.to_string_value());
            idx = end;
        }
        Ok(Value::str(parts.join(separator)))
    }

    /// Dispatch methods on a `Format` type object (`Format.new`) or instance
    /// (`.arity`, `.count`, `.Callable`, `.Str`, `CALL-ME`, ...). Returns `None`
    /// when the target/method is not a `Format` concern.
    pub(super) fn dispatch_format_method(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        // Format.new("...")
        if method == "new" && matches!(target, Value::Package(name) if name.resolve() == "Format") {
            let fmt = args
                .iter()
                .find(|a| !matches!(a, Value::Pair(..) | Value::ValuePair(..)))
                .map(Value::to_string_value)
                .unwrap_or_default();
            let mut attrs = HashMap::new();
            attrs.insert("format".to_string(), Value::str(fmt));
            return Some(Ok(Value::make_instance(Symbol::intern("Format"), attrs)));
        }

        let fmt = Self::format_string_of(target)?;
        let count = super::sprintf::sprintf_directive_count(&fmt);
        match method {
            "Str" | "gist" | "Stringy" => Some(Ok(Value::str(fmt))),
            "arity" | "count" => Some(Ok(Value::Int(count as i64))),
            "Callable" => Some(Ok(self.format_callable(&fmt, count))),
            "raku" | "perl" => Some(Ok(Value::str(format!("Format.new(\"{}\")", fmt)))),
            "CALL-ME" => Some(self.format_render(&fmt, args)),
            _ => None,
        }
    }

    /// Handle `<collection>.fmt($format, $separator)` where `$format` is a `Format`.
    /// Returns `None` when the first argument is not a `Format` instance, leaving the
    /// regular string-based `.fmt` path untouched.
    pub(super) fn dispatch_fmt_with_format(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let format_arg = args.first()?;
        if !Self::is_format_instance(format_arg) {
            return None;
        }
        let fmt = Self::format_string_of(format_arg)?;
        let count = super::sprintf::sprintf_directive_count(&fmt);

        Some(self.fmt_with_format_inner(target, &fmt, count, args))
    }

    fn fmt_with_format_inner(
        &mut self,
        target: &Value,
        fmt: &str,
        count: usize,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Pair.fmt($format) -> $format($key, $value); no separator.
        match target {
            Value::Pair(k, v) => {
                return self.format_render(fmt, &[Value::str(k.clone()), (**v).clone()]);
            }
            Value::ValuePair(k, v) => {
                return self.format_render(fmt, &[(**k).clone(), (**v).clone()]);
            }
            _ => {}
        }

        // Lazy sequences cannot be formatted.
        if Self::is_lazy_for_coerce(target) {
            return Err(Self::cannot_lazy_fmt());
        }

        let items = self.fmt_items_for_format(target, count)?;
        let separator = match args.get(1) {
            Some(sep) => sep.to_string_value(),
            None => Self::default_fmt_separator(target),
        };
        self.format_handle_iterator(fmt, count, &items, &separator)
    }

    /// The default separator for `.fmt(Format)`: " " for positional types, "\n"
    /// for associative types (Set/Bag/Mix/Hash/Map).
    fn default_fmt_separator(target: &Value) -> String {
        match target {
            Value::Array(..)
            | Value::Seq(_)
            | Value::Slip(_)
            | Value::LazyList(_)
            | Value::Range(_, _)
            | Value::RangeExcl(_, _)
            | Value::RangeExclStart(_, _)
            | Value::RangeExclBoth(_, _)
            | Value::GenericRange { .. } => " ".to_string(),
            _ => "\n".to_string(),
        }
    }

    /// Produce the sequence of values to format for a collection, following the
    /// 6.e `.fmt` rules: positional types yield their elements; associative types
    /// yield keys (count == 1) or flattened key/value pairs (count >= 2).
    fn fmt_items_for_format(
        &mut self,
        target: &Value,
        count: usize,
    ) -> Result<Vec<Value>, RuntimeError> {
        let is_associative = matches!(
            target,
            Value::Hash(_) | Value::Set(_, _) | Value::Bag(_, _) | Value::Mix(_, _)
        );
        if is_associative {
            let method = if count == 1 { "keys" } else { "kv" };
            let result = self.call_method_with_values(target.clone(), method, Vec::new())?;
            Ok(super::value_to_list(&result))
        } else {
            Ok(super::value_to_list(target))
        }
    }

    /// `X::Cannot::Lazy` with action ".fmt".
    fn cannot_lazy_fmt() -> RuntimeError {
        let mut attrs = HashMap::new();
        attrs.insert("action".to_string(), Value::str(".fmt".to_string()));
        attrs.insert(
            "message".to_string(),
            Value::str("Cannot .fmt a lazy list".to_string()),
        );
        let mut err = RuntimeError::new("Cannot .fmt a lazy list".to_string());
        err.exception = Some(Box::new(Value::make_instance(
            Symbol::intern("X::Cannot::Lazy"),
            attrs,
        )));
        err
    }
}
