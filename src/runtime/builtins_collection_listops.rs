use super::*;
use std::collections::HashMap as StdHashMap;

impl Interpreter {
    pub(super) fn builtin_shift(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            let msg = "X::TypeCheck::Argument: Calling shift(Any) will never work with declared signature ($)".to_string();
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            let ex = Value::make_instance(Symbol::intern("X::TypeCheck::Argument"), attrs);
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(ex));
            return Err(err);
        }
        if args.len() > 1 {
            return Err(RuntimeError::new(format!(
                "Too many positionals passed; expected 1 argument but got {}",
                args.len()
            )));
        }
        Ok(match args.first().cloned() {
            Some(Value::Array(mut items, ..)) => {
                if items.is_empty() {
                    make_empty_array_failure("shift")
                } else {
                    Arc::make_mut(&mut items).remove(0)
                }
            }
            _ => make_empty_array_failure("shift"),
        })
    }

    pub(super) fn builtin_pop(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            let msg =
                "Calling pop() will never work with signature of the proto ($, *%)".to_string();
            let mut attrs = StdHashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            let ex = Value::make_instance(Symbol::intern("X::TypeCheck::Argument"), attrs);
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(ex));
            return Err(err);
        }
        if args.len() > 1 {
            return Err(RuntimeError::new(format!(
                "Too many positionals passed to pop; expected 1 argument but got {}",
                args.len()
            )));
        }
        match args.first() {
            Some(Value::Array(_, kind)) if kind.is_lazy() => {
                return Err(RuntimeError::cannot_lazy("pop"));
            }
            _ => {}
        }
        Ok(match args.first().cloned() {
            Some(Value::Array(mut items, ..)) => {
                let items_mut = Arc::make_mut(&mut items);
                if items_mut.is_empty() {
                    make_empty_array_failure("pop")
                } else {
                    items_mut.pop().unwrap_or(Value::Nil)
                }
            }
            _ => make_empty_array_failure("pop"),
        })
    }

    pub(super) fn builtin_join(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let sep = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        // `join(sep, *@rest)` joins the flattened rest. Force any lazy lists
        // (join is eager over the flattened list) then run the single shared
        // `join_flat` helper — the same one the pure `native_function("join", ..)`
        // path uses. The previous inline copy here drifted from native (it had a
        // separate Range/Seq/itemized walk).
        let mut rest = Vec::with_capacity(args.len().saturating_sub(1));
        // `args.get(1..)` (not `&args[1..]`) so bare `join()` / `join(sep)` with no
        // list args doesn't panic on an empty slice.
        for v in args.get(1..).unwrap_or(&[]) {
            if let Value::LazyList(list) = v {
                // A genuinely-lazy `@`-array stays opaque (rendered `...` by
                // `flat_val`) rather than forcing its capped prefix — this is the
                // `"@a[]"` interpolation path (`join " ", @a`). (L2)
                if list.in_array_context() && list.is_genuinely_lazy() {
                    rest.push(v.clone());
                } else {
                    rest.push(Value::array(self.force_lazy_list(list)?));
                }
            } else {
                rest.push(v.clone());
            }
        }
        Ok(Value::str(
            crate::builtins::join_flat(&sep, &rest).unwrap_or_default(),
        ))
    }

    pub(super) fn builtin_list(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        // `list` on a single Seq is a no-op (returns the Seq as-is)
        if args.len() == 1
            && let Value::Seq(_) = &args[0]
        {
            return Ok(args[0].clone());
        }
        let mut result = Vec::new();
        for arg in args {
            result.extend(Self::value_to_list(arg));
        }
        Ok(Value::array(result))
    }

    pub(super) fn builtin_cache(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        // `cache` eagerly evaluates and caches the values into a List
        let mut result = Vec::new();
        for arg in args {
            result.extend(Self::value_to_list(arg));
        }
        Ok(Value::array(result))
    }

    pub(super) fn builtin_flat(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        // `flat(a, b, c)` flattens the argument *list*, so wrap the args in a
        // `List` and run the single shared `flat` helper (the same one the pure
        // `native_function("flat", ..)` path uses). The previous `flat_into`
        // copy unconditionally flattened nested `[...]`, which over-flattened
        // `flat(1, [2, [3, 4]], (5, 6))` (raku keeps the inner `[3 4]`).
        // Onearg rule: `flat($(1,2,3))` un-itemizes its sole argument and
        // descends, but `flat(0, $(1,2,3))` keeps each itemized arg single.
        let items: Vec<Value> = if args.len() == 1 {
            vec![crate::builtins::deitemize_flat_operand(&args[0])]
        } else {
            args.to_vec()
        };
        let list = Value::Array(
            std::sync::Arc::new(crate::value::ArrayData::new(items)),
            crate::value::ArrayKind::List,
        );
        let mut result = Vec::new();
        crate::builtins::flat_val(&list, &mut result, true);
        Ok(Value::Seq(std::sync::Arc::new(result)))
    }

    pub(super) fn builtin_slip(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut items = Vec::new();
        for arg in args {
            match arg {
                Value::Array(elems, ..) => items.extend(elems.iter().cloned()),
                Value::Seq(elems) => items.extend(elems.iter().cloned()),
                Value::Slip(elems) => items.extend(elems.iter().cloned()),
                Value::LazyList(ll) => {
                    if ll.scan_spec.is_some() {
                        items.extend(ll.force_scan_to(200_000));
                    } else {
                        let cached = ll.cache.lock().unwrap().clone().unwrap_or_default();
                        items.extend(cached);
                    }
                }
                other => items.push(other.clone()),
            }
        }
        Ok(Value::slip(items))
    }

    pub(super) fn builtin_reverse(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // `reverse()` (empty) and `reverse(a, b, c)` reverse the argument *list*
        // (so `reverse()` is `()`, not `Nil`). Only the single-arg form reverses a
        // collection.
        if args.len() != 1 {
            let mut items: Vec<Value> = args.to_vec();
            items.reverse();
            return Ok(Value::Seq(Arc::new(items)));
        }
        // LazyIoLines must be materialized by the interpreter (native bails).
        if let Value::LazyIoLines { handle, .. } = &args[0] {
            let mut lines = Vec::new();
            while let Some(line) = self.read_line_from_handle_value(handle)? {
                lines.push(Value::str(line));
            }
            lines.reverse();
            return Ok(Value::Seq(Arc::new(lines)));
        }
        // Single arg: delegate to the single shared native `reverse` (Array / Seq
        // / Slip / Range / 1-D shaped / Str), instead of a drifting second copy
        // that lacked Range/Slip/shaped arms.
        crate::builtins::native_function(Symbol::intern("reverse"), args).unwrap_or(Ok(Value::Nil))
    }

    pub(super) fn builtin_sort(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // Extract named args (:k, :by) and positional args
        let mut return_k = false;
        let mut by_callable: Option<Value> = None;
        let mut callable: Option<Value> = None;
        let mut positional: Vec<Value> = Vec::new();

        for arg in args {
            match arg {
                Value::Pair(key, val) if key == "k" => {
                    return_k = val.truthy();
                }
                Value::Pair(key, val) if key == "by" => {
                    by_callable = Some(val.as_ref().clone());
                }
                _ => positional.push(arg.clone()),
            }
        }

        // A lazy/infinite source cannot be sorted: throw X::Cannot::Lazy instead
        // of materializing it (matches raku). The comparator (a Sub) is never
        // lazy, so scanning all positionals is safe.
        if positional.iter().any(Self::is_lazy_for_coerce) {
            return Err(RuntimeError::cannot_lazy("sort"));
        }

        // sort(comparator, list, ...) or sort(list, ...) or sort(items...)
        if positional.len() >= 2 {
            let first = &positional[0];
            if matches!(first, Value::Sub(_) | Value::Routine { .. }) {
                callable = Some(first.clone());
                positional.remove(0);
            }
        }

        // Prefer :by over positional callable
        if by_callable.is_some() {
            callable = by_callable;
        }

        // Flatten positional args into items
        let mut items: Vec<Value> = Vec::new();
        for arg in &positional {
            if crate::runtime::utils::is_shaped_array(arg) {
                items.extend(crate::runtime::utils::shaped_array_leaves(arg));
            } else {
                match arg {
                    Value::Array(elems, ..) => items.extend(elems.iter().cloned()),
                    Value::Seq(elems) => items.extend(elems.iter().cloned()),
                    Value::Range(..)
                    | Value::RangeExcl(..)
                    | Value::RangeExclStart(..)
                    | Value::RangeExclBoth(..)
                    | Value::GenericRange { .. } => {
                        items.extend(Self::value_to_list(arg));
                    }
                    other => items.push(other.clone()),
                }
            }
        }

        // Build sort args for the shared sort entry point.
        let mut sort_args: Vec<Value> = Vec::new();
        if let Some(c) = callable {
            sort_args.push(c);
        }
        if return_k {
            sort_args.push(Value::Pair("k".to_string(), Box::new(Value::Bool(true))));
        }

        if items.is_empty() && positional.is_empty() {
            return Ok(Value::array(Vec::new()));
        }

        let mut caller = crate::runtime::methods_collection_ops::sort::InterpCaller(self);
        crate::runtime::methods_collection_ops::sort::sort_value_generic(
            &mut caller,
            Value::array(items),
            &sort_args,
        )
    }

    pub(super) fn builtin_unique(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            return Ok(Value::array(Vec::new()));
        }

        let mut positional = Vec::new();
        let mut method_args = Vec::new();
        for arg in args {
            match arg {
                Value::Pair(key, _) if key == "as" || key == "with" => {
                    method_args.push(arg.clone());
                }
                _ => positional.push(arg.clone()),
            }
        }

        if positional.is_empty() {
            return Ok(Value::array(Vec::new()));
        }

        let target = if positional.len() == 1 {
            positional[0].clone()
        } else {
            Value::array(positional)
        };
        self.call_method_with_values(target, "unique", method_args)
    }

    pub(super) fn builtin_repeated(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            return Ok(Value::array(Vec::new()));
        }

        let mut positional = Vec::new();
        let mut method_args = Vec::new();
        for arg in args {
            match arg {
                Value::Pair(key, _) if key == "as" || key == "with" => {
                    method_args.push(arg.clone());
                }
                _ => positional.push(arg.clone()),
            }
        }

        if positional.is_empty() {
            return Ok(Value::array(Vec::new()));
        }

        let target = if positional.len() == 1 {
            positional[0].clone()
        } else {
            Value::array(positional)
        };
        self.call_method_with_values(target, "repeated", method_args)
    }

    pub(super) fn builtin_squish(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            return Ok(Value::array(Vec::new()));
        }

        let mut positional = Vec::new();
        let mut method_args = Vec::new();
        for arg in args {
            match arg {
                Value::Pair(key, _) if key == "as" || key == "with" => {
                    method_args.push(arg.clone());
                }
                _ => positional.push(arg.clone()),
            }
        }

        if positional.is_empty() {
            return Ok(Value::array(Vec::new()));
        }

        let target = if positional.len() == 1 {
            positional[0].clone()
        } else {
            Value::array(positional)
        };
        self.call_method_with_values(target, "squish", method_args)
    }
}
