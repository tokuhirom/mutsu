use super::allomorph::allomorph_accepts;

use super::fmt_contains::fmt_value_needs_coercion;
use super::numeric::{int_to_subscript, int_to_superscript};
use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};
use num_traits::ToPrimitive;

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
    match method {
        // ACCEPTS for allomorphic types (IntStr, RatStr, NumStr, ComplexStr)
        "ACCEPTS" if matches!(target.view(), ValueView::Mixin(_, m) if m.contains_key("Str")) => {
            // Instance args need the interpreter to call .Numeric, so return None
            allomorph_accepts(target, arg).map(|result| Ok(Value::truth(result)))
        }
        // ACCEPTS for Set/Bag/Mix types: equality check (the quant hashes'
        // rows' implementation, `method_table::subscript`).
        "ACCEPTS"
            if matches!(
                target.view(),
                ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..)
            ) =>
        {
            crate::builtins::method_table::subscript::accepts_quant(
                target,
                std::slice::from_ref(arg),
            )
        }
        // ACCEPTS for Pair: checks if the argument has the matching key->value
        // (the `Pair` row's implementation, `method_table::subscript`).
        "ACCEPTS"
            if matches!(
                target.view(),
                ValueView::Pair(..) | ValueView::ValuePair(..)
            ) =>
        {
            crate::builtins::method_table::subscript::accepts_pair(
                target,
                std::slice::from_ref(arg),
            )
        }
        // ACCEPTS for Range: value ~~ Range containment, Range ~~ Range subset
        // (the `Range` row's implementation, `method_table::subscript`).
        "ACCEPTS" if target.is_range() => crate::builtins::method_table::subscript::accepts_range(
            target,
            std::slice::from_ref(arg),
        ),
        "Str" => {
            // Int.Str(:superscript) and Int.Str(:subscript)
            if let ValueView::Pair(key, val) = arg.view()
                && val.truthy()
            {
                let int_val = match target.view() {
                    ValueView::Int(i) => Some(i),
                    ValueView::BigInt(bi) => bi.to_i64(),
                    ValueView::Num(f) => Some(f as i64),
                    ValueView::Bool(b) => Some(if b { 1 } else { 0 }),
                    _ => {
                        let s = target.to_string_value();
                        s.parse::<i64>().ok()
                    }
                };
                if let Some(n) = int_val {
                    match key.as_str() {
                        "superscript" => {
                            return Some(Ok(Value::str(int_to_superscript(n))));
                        }
                        "subscript" => {
                            return Some(Ok(Value::str(int_to_subscript(n))));
                        }
                        _ => {}
                    }
                }
            }
            // Default: just stringify
            Some(Ok(Value::str(target.to_string_value())))
        }
        // Cost: O(n + L * s), n = chars of the invocant, L = lines, s = |steps|.
        // A Capture's positional part: the `Capture` row's implementation
        // (`method_table::capture`).
        // A `Uni`'s positional subscript: the `Uni` rows' implementation
        // (`method_table::uni`).
        "AT-POS" if matches!(target.view(), ValueView::Uni(..)) => {
            crate::builtins::method_table::uni::at_pos(target, std::slice::from_ref(arg))
        }
        "EXISTS-POS" if matches!(target.view(), ValueView::Uni(..)) => {
            crate::builtins::method_table::uni::exists_pos(target, std::slice::from_ref(arg))
        }
        "AT-POS" if matches!(target.view(), ValueView::Capture { .. }) => {
            crate::builtins::method_table::capture::at_pos(target, std::slice::from_ref(arg))
        }
        "EXISTS-POS" if matches!(target.view(), ValueView::Capture { .. }) => {
            crate::builtins::method_table::capture::exists_pos(target, std::slice::from_ref(arg))
        }
        "AT-POS" => {
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
                    ValueView::Str(s) => {
                        let ch = s.chars().nth(idx).map(|c| Value::str(c.to_string()));
                        Some(Ok(ch.unwrap_or(Value::NIL)))
                    }
                    ValueView::Instance { .. } if target.is_match_instance() => {
                        let list_v = target.match_list();
                        if let Some(ValueView::Array(positional, ..)) =
                            list_v.as_ref().map(Value::view)
                        {
                            return Some(Ok(positional.get(idx).cloned().unwrap_or(Value::NIL)));
                        }
                        Some(Ok(Value::NIL))
                    }
                    ValueView::Instance {
                        class_name,
                        attributes,
                        ..
                    } if crate::runtime::utils::is_native_elems_class(&class_name.resolve())
                        && crate::value::value_buf::has_buf_elems(&attributes) =>
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
                    // IO::Path::Parts does Positional: `$parts[0]` is `volume => C:`,
                    // `[1]` the dirname pair, `[2]` the basename pair (fixed order).
                    // ADR-0021 I2: a data-minted pair defaults positional, not the
                    // named-argument marker flavour `Value::pair` mints — else `say
                    // $parts[0]` (no call-site fat-arrow) silently filters it out as
                    // an in-band named marker (#9820).
                    ValueView::Instance {
                        class_name,
                        attributes,
                        ..
                    } if class_name == "IO::Path::Parts" => {
                        Some(Ok(crate::runtime::io_path_parts_keys()
                            .get(idx)
                            .map(|key| {
                                let v =
                                    attributes.as_map().get(*key).cloned().unwrap_or(Value::NIL);
                                Value::value_pair(Value::str((*key).to_string()), v)
                            })
                            .unwrap_or(Value::NIL)))
                    }
                    // `Any.AT-POS`: an ordinary instance without a positional
                    // protocol is a one-element list holding itself. User
                    // `AT-POS` methods are resolved before this native fallback,
                    // while the subscript opcode keeps an `AT-KEY` protocol from
                    // being shadowed by this inherited default.
                    ValueView::Instance { attributes, .. }
                        if !attributes.contains_key("__mutsu_array_storage") =>
                    {
                        Some(Ok(if idx == 0 {
                            target.descalarize().clone()
                        } else {
                            crate::value::RuntimeError::out_of_range_failure(
                                "Index",
                                Value::int(idx as i64),
                                "0..0",
                            )
                        }))
                    }
                    // `Any.AT-POS`: a non-Positional value is a one-element list
                    // holding itself under a positional subscript, so index 0
                    // answers the value and everything else is out of range. The
                    // Associative containers are included, the mirror of the
                    // `EXISTS-POS` arm below — `%h.AT-POS(0)` is the hash rather
                    // than a missing-method error, and `%h.AT-POS(1)` the Failure.
                    _ if target.is_one_element_under_positional_subscript() => {
                        Some(Ok(if idx == 0 {
                            target.descalarize().clone()
                        } else {
                            crate::value::RuntimeError::out_of_range_failure(
                                "Index",
                                Value::int(idx as i64),
                                "0..0",
                            )
                        }))
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
            // (Past here the index is a non-negative number.)
            let idx = match arg.view() {
                ValueView::Int(i) => i,
                ValueView::Num(f) => f as i64,
                ValueView::Str(s) if s.trim().parse::<f64>().is_err() => {
                    return Some(Err(crate::builtins::methods_0arg::str_numeric_error(&s)));
                }
                _ => return Some(Ok(Value::FALSE)),
            };
            // `Any.EXISTS-POS`: a non-Positional value is a one-element list
            // holding itself under a positional subscript, so index 0 is the
            // only one that exists. The Associative containers are included —
            // `Hash`/`Set`/`Bag`/`Mix` do not do `Positional`, so
            // `%h.EXISTS-POS(1)` is False however many keys the hash has. A
            // class declaring its own EXISTS-POS is never shadowed by this.
            if target.is_one_element_under_positional_subscript() {
                return Some(Ok(Value::truth(idx == 0)));
            }
            None
        }
        // Cost: O(k) on Array/List, k = selected elements; O(e) otherwise,
        // e = receiver elements materialized before selecting a window.
        "zprintf" => {
            // Method form: '%f'.zprintf(value) — like sprintf but with zprintf
            // semantics. Mirrors the "sprintf" arm above: an Instance/Package/
            // mixin arg needs interpreter-aware coercion, so defer to the
            // slow-path `zprintf` arm (routes through `builtin_sprintf(.., true)`).
            if fmt_value_needs_coercion(arg) {
                return None;
            }
            let fmt = target.to_string_value();
            let rendered = runtime::format_zprintf(&fmt, Some(arg));
            Some(Ok(Value::str(rendered)))
        }
        // A number: the numeric types' `base` rows' implementation
        // (`method_table::numeric_base`).
        "base"
            if matches!(
                target.view(),
                ValueView::Int(_)
                    | ValueView::BigInt(_)
                    | ValueView::Bool(_)
                    | ValueView::Num(_)
                    | ValueView::Rat(..)
                    | ValueView::FatRat(..)
            ) =>
        {
            crate::builtins::method_table::numeric_base::base_of(target, arg, None)
        }
        // Buf/Blob `read-*` (1 arg: offset, native byte order): the rows'
        // implementation (`method_table::blob_read`).
        "read-num32" | "read-num64" | "read-uint8" | "read-int8" | "read-uint16" | "read-int16"
        | "read-uint32" | "read-int32" | "read-uint64" | "read-int64" | "read-uint128"
        | "read-int128" => crate::builtins::method_table::blob_read::read(
            method,
            target,
            std::slice::from_ref(arg),
        ),
        "AT-KEY" => match target.view() {
            // The associative rows' implementation (`method_table::subscript`).
            ValueView::Hash(_)
            | ValueView::Set(..)
            | ValueView::Bag(..)
            | ValueView::Mix(..)
            | ValueView::Pair(..)
            | ValueView::ValuePair(..)
            | ValueView::Capture { .. } => {
                crate::builtins::method_table::subscript::at_key(target, std::slice::from_ref(arg))
            }
            // IO::Path::Parts does Associative: `$parts<volume>` returns the part.
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "IO::Path::Parts" => {
                let key = arg.to_string_value();
                Some(Ok(attributes
                    .as_map()
                    .get(&key)
                    .cloned()
                    .unwrap_or(Value::NIL)))
            }
            ValueView::Nil => Some(Ok(Value::package(crate::symbol::wk::any()))),
            ValueView::Package(name) if matches!(name.resolve().as_str(), "Any" | "Mu") => {
                Some(Ok(Value::package(crate::symbol::wk::any())))
            }
            // Anything else does not do `Associative`, and raku's `Any.AT-KEY`
            // fails for it. An Instance/Mixin/Package may carry a user-defined
            // AT-KEY, so those keep falling through to the runtime dispatcher.
            _ if !matches!(
                target.view(),
                ValueView::Instance { .. } | ValueView::Mixin(..) | ValueView::Package(_)
            ) =>
            {
                Some(Ok(RuntimeError::assoc_indexing_failure(
                    crate::runtime::utils::value_type_name(target),
                )))
            }
            _ => None,
        },
        "EXISTS-KEY" => match target.view() {
            // The associative rows' implementation (`method_table::subscript`).
            ValueView::Hash(_)
            | ValueView::Set(..)
            | ValueView::Bag(..)
            | ValueView::Mix(..)
            | ValueView::Pair(..)
            | ValueView::ValuePair(..)
            | ValueView::Capture { .. } => crate::builtins::method_table::subscript::exists_key(
                target,
                std::slice::from_ref(arg),
            ),
            ValueView::Nil => Some(Ok(Value::FALSE)),
            ValueView::Package(name) if matches!(name.resolve().as_str(), "Any" | "Mu") => {
                Some(Ok(Value::FALSE))
            }
            // `Any.EXISTS-KEY` is always False, so a value that does not do
            // `Associative` has no keys at all (the Instance/Mixin/Package
            // carve-out mirrors the AT-KEY arm above).
            _ if !matches!(
                target.view(),
                ValueView::Instance { .. } | ValueView::Mixin(..) | ValueView::Package(_)
            ) =>
            {
                Some(Ok(Value::FALSE))
            }
            _ => None,
        },
        // A user class composing a buffer role over a VMArray body
        // (`class B does Blob[uint8] is repr('VMArray')`) has no shape; the
        // `Blob` row's handler answers it and keeps its class.
        "subbuf" => {
            crate::builtins::method_table::blob_read::subbuf(target, std::slice::from_ref(arg))
        }
        "isa" => {
            // Instance, Mixin(Instance), and Package values need interpreter
            // access for user-defined class hierarchies, role checks, and
            // subset type resolution, so fall through to the runtime handler.
            let needs_interpreter = match target.view() {
                ValueView::Instance { .. } | ValueView::Package(_) => true,
                ValueView::Mixin(inner, _) => {
                    matches!(inner.as_ref().view(), ValueView::Instance { .. })
                }
                _ => false,
            };
            if needs_interpreter {
                return None;
            }
            let type_name = match arg.view() {
                ValueView::Package(name) => name.resolve(),
                ValueView::Str(name) => name.to_string(),
                ValueView::Instance { class_name, .. } => class_name.resolve(),
                _ => {
                    // For defined values, extract the type name (e.g., 3.isa(4) checks Int)
                    // This matches Raku's behavior where .isa on a defined value
                    // uses the value's type, not its string representation.
                    crate::value::types::what_type_name(arg)
                }
            };
            // A parameterized type (`Array[Hash]`, `Hash[Int,Str]`) is the
            // typed container's own type: `my Hash @a; @a.isa(Array[Hash])`.
            // `isa_check` knows only nominal names, so compare the receiver's
            // full type name.
            if type_name.contains('[') {
                return Some(Ok(Value::truth(
                    crate::runtime::embedded_container_type_name(target).as_deref()
                        == Some(type_name.as_str()),
                )));
            }
            Some(Ok(Value::truth(target.isa_check(&type_name))))
        }
        _ => None,
    }
}
