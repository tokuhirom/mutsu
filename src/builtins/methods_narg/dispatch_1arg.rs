use super::allomorph::allomorph_accepts;

use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

/// A built-in method call with one argument: the row of the built-in method
/// table (ADR-11276), then the cascade, then the row a receiver with no shape
/// reaches (`method_table::answer_shapeless`; an instance needs the
/// interpreter's MRO and is answered by the entries that have one).
// Cost: O(1) to decline the table (a bit test on the method symbol), otherwise
// a tag probe and one hash lookup plus the row's handler; then the cascade's,
// plus O(m) to test the name, m = audited names.
pub(crate) fn native_method_1arg(
    target: &Value,
    method_sym: Symbol,
    arg: &Value,
) -> Option<Result<Value, RuntimeError>> {
    if let Some(result) =
        crate::builtins::method_table::answer(target, method_sym, std::slice::from_ref(arg))
    {
        return Some(result);
    }
    native_method_1arg_without_table(target, method_sym, arg)
}

/// [`native_method_1arg`] without the method table: the name-matching
/// cascade and the shapeless rows, for the two callers that must not take a
/// table row (the debug cross-check, and the VM entry whose user-override veto
/// refused the row).
// Cost: the cascade's, plus O(m) to test the name, m = audited names.
pub(crate) fn native_method_1arg_without_table(
    target: &Value,
    method_sym: Symbol,
    arg: &Value,
) -> Option<Result<Value, RuntimeError>> {
    native_method_1arg_cascade(target, method_sym, arg).or_else(|| {
        crate::builtins::method_table::answer_shapeless(
            None,
            target.descalarize(),
            method_sym,
            std::slice::from_ref(arg),
        )
    })
}

fn native_method_1arg_cascade(
    target: &Value,
    method_sym: Symbol,
    arg: &Value,
) -> Option<Result<Value, RuntimeError>> {
    let method = method_sym.resolve();
    let method = method.as_str();

    // Scalar containers are transparent for method dispatch (no .VAR at this
    // arity): the held value is a call of its own, which asks the table first,
    // so the decline step below sees the receiver the table was asked about.
    let settled = target.descalarize();
    if !std::ptr::eq(settled, target) {
        return native_method_1arg(settled, method_sym, arg);
    }
    // A method call decontainerizes an itemized Hash receiver (`$map`, an
    // element read out of an Array) the same way, as the 0-arg entry and
    // `call_method_with_values` do (#10744): the table refuses the itemized
    // form, so the plain hash is asked again.
    // Cost: O(1), a tag probe (`hash_is_itemized` is false for every non-Hash).
    if target.hash_is_itemized() {
        return native_method_1arg(&target.clone().with_hash_itemized(false), method_sym, arg);
    }
    // An instance of a user subclass of `Int` answers `Int`'s methods on its
    // payload (`builtins::native_base`).
    if let Some(result) =
        crate::builtins::native_base::dispatch(target, method_sym, std::slice::from_ref(arg))
    {
        return Some(result);
    }
    // A plain allomorph's method with a row is the row's, on the part it reads
    // (§9.101): `<65>.uniprop("Script")` is `Common`, the script of "6".
    // Cost: O(1), a tag probe, one map lookup and at most two table lookups.
    if crate::builtins::method_table::allomorph::answers(
        target,
        method_sym,
        std::slice::from_ref(arg),
    ) {
        return None;
    }
    // Cost: O(n), n = bytes of the invocant.
    if method == "naive-word-wrapper" {
        return crate::builtins::naive_word_wrapper::native_naive_word_wrapper(target, &[arg]);
    }
    // `Str.AST(:compunit)`: the parse wrapped in a `RakuAST::CompUnit`.
    // Cost: O(n), n = source length.
    if method == "AST"
        && let ValueView::Str(source) = target.view()
        && let ValueView::Pair(key, flag) = arg.view()
        && key.as_str() == "compunit"
    {
        return Some(if flag.truthy() {
            crate::rakuast::str_dot_ast_compunit(&source)
        } else {
            crate::rakuast::str_dot_ast(&source)
        });
    }
    // Cost: O(1).
    if matches!(method, "replace-statement-list" | "set-expression")
        && let Some(result) = target.rakuast_set_field(method, arg.clone())
    {
        return Some(result);
    }
    if matches!(method, "add-statement" | "unshift-statement" | "push")
        && let Some(result) = target.rakuast_add_child(method, arg.clone())
    {
        return Some(result);
    }
    // The `Match.replace-with` row's implementation (`method_table::regex_match`).
    if method == "replace-with" && target.is_match_instance() {
        return crate::builtins::method_table::regex_match::replace_with(target, arg).map(Ok);
    }
    // `Backtrace` and `Backtrace::Frame`: the rows' handlers (ADR-11276 §9.35).
    if let Some(answer) =
        crate::builtins::method_table::backtrace::answer(target, method, std::slice::from_ref(arg))
    {
        return Some(answer);
    }
    // `Pod::Block::Declarator`'s accumulators. Rakudo builds a declarator pod
    // block by appending each `#|` / `#=` comment through `._add_leading` /
    // `._add_trailing`, space-joining the pieces; the same two methods are the
    // public way to build one by hand for `.^set_why`
    // (`Type/Metamodel/Documenting.rakudoc`). The attribute cell is
    // interior-mutable, so the append is visible through every alias of the
    // block -- which is what lets the documented
    // `my Pod::Block::Declarator $pod .= new; $pod._add_leading(...); $pod`
    // idiom work.
    if let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
        && class_name == "Pod::Block::Declarator"
        && let Some(key) = match method {
            "_add_leading" => Some("leading"),
            "_add_trailing" => Some("trailing"),
            _ => None,
        }
    {
        let appended = {
            let map = attributes.as_map();
            // Only a non-empty Str counts as accumulated text: a fresh
            // `Pod::Block::Declarator.new` leaves the slot holding the `Any`
            // type object, which would otherwise stringify into the result.
            let prev = match map.get(key).map(Value::view) {
                Some(ValueView::Str(s)) => s.to_string(),
                _ => String::new(),
            };
            let added = arg.to_string_value();
            if prev.is_empty() {
                added
            } else {
                format!("{prev} {added}")
            }
        };
        attributes.insert(key, Value::str(appended.clone()));
        // `contents` is the rendered text `.Str`/`.gist` read
        // (`value/display.rs`): leading and trailing joined by a newline,
        // with an absent half contributing nothing.
        let contents = {
            let map = attributes.as_map();
            let part = |k: &str| match map.get(k).map(Value::view) {
                Some(ValueView::Str(s)) if !s.is_empty() => Some(s.to_string()),
                _ => None,
            };
            match (part("leading"), part("trailing")) {
                (Some(l), Some(t)) => format!("{l}\n{t}"),
                (Some(l), None) => l,
                (None, Some(t)) => t,
                (None, None) => String::new(),
            }
        };
        attributes.insert("contents", Value::str(contents));
        // Rakudo hands back the raw `@!leading` / `@!trailing` array. mutsu
        // stores the joined text (the shape `.leading` itself reports), so the
        // accumulated string is returned; every documented idiom uses the call
        // in sink context.
        return Some(Ok(Value::str(appended)));
    }
    // Instance with __baggy_data__: delegate to the inner Bag/Set for collection methods
    if let ValueView::Instance { attributes, .. } = target.view()
        && let Some(inner) = attributes.as_map().get("__baggy_data__")
        && !matches!(
            method,
            "WHAT" | "WHICH" | "raku" | "gist" | "Str" | "perl" | "isa" | "^name"
        )
    {
        return native_method_1arg(inner, method_sym, arg);
    }
    // Cost: O(p), p = number of parts in the Version matcher.
    // Version subclasses carry the native matcher in an instance payload, so
    // expose the same ACCEPTS semantics as a native Version to their methods.
    if method == "ACCEPTS" {
        let version_value = |value: &Value| match value.view() {
            ValueView::Version { .. } => Some(value.clone()),
            ValueView::Instance { attributes, .. } => {
                attributes.as_map().get("__mutsu_version_value").cloned()
            }
            _ => None,
        };
        if let Some(matcher) = version_value(target)
            && let ValueView::Version {
                parts, plus, minus, ..
            } = matcher.view()
        {
            let candidate = version_value(arg).unwrap_or_else(|| arg.clone());
            return Some(Ok(Value::truth(runtime::Interpreter::version_smart_match(
                &candidate, parts, plus, minus,
            ))));
        }
    }
    // A `Str` argument of `base` and `polymod` numifies first (`10.base("2")`).
    // The other numeric methods are rows of `builtins::method_table`, which
    // numify their receiver and argument themselves (`Cool.round`, the math
    // rows).
    if matches!(method, "base" | "polymod")
        && let ValueView::Str(s) = arg.view()
    {
        let coerced = if let Ok(i) = s.parse::<i64>() {
            Some(Value::int(i))
        } else if let Ok(f) = s.parse::<f64>() {
            Some(Value::num(f))
        } else {
            None
        };
        if let Some(coerced) = coerced {
            return native_method_1arg(target, method_sym, &coerced);
        }
    }
    // A call the method table answers has no arm below this point: the row is
    // the method's only implementation (ADR-11276 §9.88). `native_method_1arg`
    // asked the table first, so this declines only for the callers that run
    // the cascade without it (`native_method_1arg_without_table`).
    // Cost: O(1) when no row has the name (a bit test); otherwise a tag probe,
    // one hash lookup and, for a row found, its handler.
    if crate::builtins::method_table::answer(target, method_sym, std::slice::from_ref(arg))
        .is_some()
    {
        return None;
    }
    // A match the table has no shape for (a grammar cursor, an instance of a
    // `Match` subclass) answers the rows a plain `Match` reaches (§9.89).
    if target.is_match_instance()
        && let Some(result) = crate::builtins::methods_0arg::match_owner_row(
            target,
            method_sym,
            std::slice::from_ref(arg),
        )
    {
        return Some(result);
    }
    // A positional index of a receiver whose row binds it (`Int:D`, and every
    // other index as Rakudo's candidates do) is the row's alone, through
    // `method_table::index_arg` (ADR-11276 §9.105).
    // Cost: O(1), a bit test, a tag probe and one hash lookup.
    if matches!(method, "AT-POS" | "EXISTS-POS")
        && crate::builtins::method_table::index_arg::binds(target, method_sym)
    {
        return None;
    }
    match method {
        // ACCEPTS for allomorphic types (IntStr, RatStr, NumStr, ComplexStr)
        "ACCEPTS" if matches!(target.view(), ValueView::Mixin(_, m) if m.contains_key("Str")) => {
            // Instance args need the interpreter to call .Numeric, so return None
            allomorph_accepts(target, arg).map(|result| Ok(Value::truth(result)))
        }
        "AT-POS" => {
            // Any other index is bound through its `.Int` (`index_arg`).
            if !matches!(
                arg.view(),
                ValueView::Int(_) | ValueView::Num(_) | ValueView::Rat(..) | ValueView::Str(_)
            ) {
                return None;
            }
            // The index a Str, Rat or Num stands for (the `AT-POS` rows'
            // implementation, `method_table::positional`).
            let idx = match crate::builtins::method_table::positional::at_pos_index(arg) {
                Ok(idx) => idx,
                Err(answer) => return answer,
            };
            // A Range or a list's element: the `AT-POS` rows' implementation.
            if let Some(answer) = crate::builtins::method_table::positional::at_pos_of(target, idx)
            {
                return Some(answer);
            }
            {
                match target.view() {
                    // `Nil` answers every method with itself.
                    ValueView::Nil => Some(Ok(Value::NIL)),
                    // A native-backed `CArray` (a buffer's elements are the
                    // `Blob`/`Buf` rows', `method_table::blob_pos`).
                    ValueView::Instance {
                        class_name,
                        attributes,
                        ..
                    } if crate::value::value_carray::is_native_carray_class(
                        &class_name.resolve(),
                    ) && crate::value::value_buf::has_buf_elems(&attributes) =>
                    {
                        if let Some(b) =
                            crate::value::value_buf::with_buf_elems(&attributes, |items| {
                                items.get(idx).cloned().unwrap_or(Value::int(0))
                            })
                        {
                            return Some(Ok(b));
                        }
                        Some(Ok(Value::int(0)))
                    }
                    // `Any.AT-POS` for an instance read by the subscript
                    // opcode, which has no interpreter for `Any`'s row: an
                    // instance without a positional protocol is the
                    // one-element list holding itself.
                    // TODO: the subscript opcode should
                    // reach `Any`'s row (`any_subscript::at_pos`) through
                    // `answer_shapeless` with its interpreter, as a method
                    // call does, and this branch go.
                    ValueView::Instance { attributes, .. }
                        if !attributes.contains_key("__mutsu_array_storage") =>
                    {
                        crate::builtins::method_table::any_subscript_at_pos(
                            target,
                            &[Value::int(idx as i64)],
                        )
                    }
                    _ => None,
                }
            }
        }
        "EXISTS-POS" => {
            // A Range's or list's slot: the `EXISTS-POS` rows' implementation
            // (`method_table::positional`).
            if let Some(answer) = crate::builtins::method_table::positional::exists_pos(
                target,
                std::slice::from_ref(arg),
            ) {
                return Some(answer);
            }
            // `Nil.EXISTS-POS` is `False`; a value that is not `Positional`
            // is `Any`'s row (`method_table::collections::any_subscript`).
            if matches!(target.view(), ValueView::Nil) {
                return Some(Ok(Value::FALSE));
            }
            None
        }
        // A value that is not `Associative` answers `Any`'s rows
        // (`method_table::collections::any_subscript`), a key of a hash or a
        // quant hash its own (`method_table::subscript`, which also take a
        // user-object key through `invoke_unadmitted`).
        "AT-KEY" => match target.view() {
            // A hash or a quant hash with no settled shape (a `Map`
            // subclass's raw storage): the associative rows' implementation.
            ValueView::Hash(_) | ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..) => {
                crate::builtins::method_table::subscript::at_key(target, std::slice::from_ref(arg))
            }
            // `Nil` answers every method with itself.
            ValueView::Nil => Some(Ok(Value::NIL)),
            // A type object autovivifies; another package is a stash, which
            // the interpreter looks up.
            ValueView::Package(name) if matches!(name.resolve().as_str(), "Any" | "Mu") => {
                Some(Ok(Value::package(crate::symbol::wk::any())))
            }
            _ => None,
        },
        "EXISTS-KEY" => match target.view() {
            ValueView::Hash(_) | ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..) => {
                crate::builtins::method_table::subscript::exists_key(
                    target,
                    std::slice::from_ref(arg),
                )
            }
            ValueView::Nil => Some(Ok(Value::FALSE)),
            ValueView::Package(name) if matches!(name.resolve().as_str(), "Any" | "Mu") => {
                Some(Ok(Value::FALSE))
            }
            _ => None,
        },
        _ => None,
    }
}
