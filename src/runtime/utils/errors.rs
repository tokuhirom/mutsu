use super::*;

/// `Date`/`DateTime` get their `.IO` from `Dateish`, whose signature takes a `Dateish:D`
/// invocant, so calling it on the type object is a concreteness error rather than a
/// stringification of `(Date)` into a path. Returns the error when `name` is one of them.
pub(crate) fn dateish_io_concreteness_error(name: &str) -> Option<RuntimeError> {
    if !matches!(name, "Date" | "DateTime") {
        return None;
    }
    Some(RuntimeError::parameter_invalid_concreteness(
        "Dateish", name, "IO", "self", true, // should_be_concrete
        true, // param_is_invocant
    ))
}

pub(crate) fn merge_junction(kind: JunctionKind, left: Value, right: Value) -> Value {
    // Infix junction operators (|, &, ^) always create a new 2-element
    // junction without flattening. List-associative flattening is handled
    // at compile time via JunctionAnyN/AllN/OneN opcodes.
    Value::junction(kind, vec![left, right])
}

/// Format a short representation of a value for type-check error messages,
/// matching Raku's format: e.g. `("hello")`, `(42)`.
pub(crate) fn value_short_repr(val: &Value) -> String {
    match val.view() {
        ValueView::Str(s) => format!("(\"{}\")", *s),
        ValueView::Int(n) => format!("({})", n),
        ValueView::BigInt(n) => format!("({})", *n),
        ValueView::Num(n) => format!("({})", n),
        ValueView::Bool(b) => format!("({})", if b { "True" } else { "False" }),
        ValueView::Rat(n, d) => format!("({}/{})", n, d),
        ValueView::BigRat(n, d) if val.is_bigfatrat() => format!("(FatRat.new({}, {}))", n, d),
        ValueView::BigRat(n, d) => format!("({}/{})", n, d),
        ValueView::FatRat(n, d) => format!("(FatRat.new({}, {}))", n, d),
        ValueView::Nil => "(Nil)".to_string(),
        _ => String::new(),
    }
}

/// Format the variable name for error messages, adding `$` sigil for
/// scalar variables that don't already have a sigil prefix.
pub(crate) fn format_var_name_for_error(name: &str) -> String {
    if name.starts_with('$')
        || name.starts_with('@')
        || name.starts_with('%')
        || name.starts_with('&')
    {
        name.to_string()
    } else {
        format!("${}", name)
    }
}

/// Build the standard X::TypeCheck::Assignment error message, matching Raku's format:
/// `Type check failed in assignment to $x; expected Int but got Str ("hello")`
pub(crate) fn type_check_assignment_error(var_name: &str, expected: &str, val: &Value) -> String {
    let display_name = format_var_name_for_error(var_name);
    let got_type = value_type_name(val);
    let repr = value_short_repr(val);
    if repr.is_empty() {
        format!(
            "Type check failed in assignment to {}; expected {} but got {}",
            display_name, expected, got_type
        )
    } else {
        format!(
            "Type check failed in assignment to {}; expected {} but got {} {}",
            display_name, expected, got_type, repr
        )
    }
}

/// Build a structured X::TypeCheck::Binding RuntimeError, for `:=` binds to a
/// typed scalar (e.g. `my Str $x := 3`), matching Raku's format:
/// `Type check failed in binding; expected Str but got Int (3)`
pub(crate) fn type_check_binding_typed_error(expected: &str, val: &Value) -> RuntimeError {
    let got_type = value_type_name(val);
    let repr = value_short_repr(val);
    // `.message` / `.Str` carry no class-name prefix (that is `.gist`'s job);
    // Rakudo's message is exactly `Type check failed in binding; expected ...`.
    let msg = if repr.is_empty() {
        format!(
            "Type check failed in binding; expected {} but got {}",
            expected, got_type
        )
    } else {
        format!(
            "Type check failed in binding; expected {} but got {} {}",
            expected, got_type, repr
        )
    };
    let mut attrs = std::collections::HashMap::new();
    attrs.insert(
        "expected".to_string(),
        crate::value::expected_type_object(expected),
    );
    attrs.insert("got".to_string(), val.clone());
    attrs.insert("operation".to_string(), Value::str("bind".to_string()));
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    RuntimeError::typed("X::TypeCheck::Binding", attrs)
}

/// Build a structured X::Dynamic::NotFound RuntimeError.
/// Thrown when assigning to a dynamic variable (`$*x` / `@*x` / `%*x`) that is
/// not present anywhere in the dynamic scope. `display_name` is the full
/// sigil+twigil form (e.g. `$*an_undeclared_dynvar`).
pub(crate) fn dynamic_not_found_error(display_name: &str) -> RuntimeError {
    let msg = format!("Dynamic variable {} not found", display_name);
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("name".to_string(), Value::str(display_name.to_string()));
    attrs.insert("symbol".to_string(), Value::str(display_name.to_string()));
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    RuntimeError::typed("X::Dynamic::NotFound", attrs)
}

/// Build a structured X::Caller::NotDynamic RuntimeError.
/// Thrown when accessing a caller-frame lexical through `CALLER::` (either the
/// `$CALLER::x` symbolic form or the `CALLER::<$x>` stash-subscript form) when
/// that variable is not declared `is dynamic`. `name` is the bare variable name
/// without sigil; the reported `symbol` re-adds the `$` sigil.
pub(crate) fn caller_not_dynamic_error(name: &str) -> RuntimeError {
    let symbol = format!("${name}");
    let msg =
        format!("Cannot access '{symbol}' through CALLER, because it is not declared as dynamic");
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("symbol".to_string(), Value::str(symbol));
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    RuntimeError::typed("X::Caller::NotDynamic", attrs)
}

/// Build a structured X::TypeCheck::Assignment RuntimeError.
/// This creates a proper exception object that `throws-like` can match.
pub(crate) fn type_check_assignment_typed_error(
    var_name: &str,
    expected: &str,
    val: &Value,
) -> RuntimeError {
    let msg = type_check_assignment_error(var_name, expected, val);
    let display_name = format_var_name_for_error(var_name);
    let mut attrs = std::collections::HashMap::new();
    // raku exposes `.expected` as the expected type OBJECT and `.got` as the
    // offending VALUE (not its type name), so `throws-like` matchers like
    // `expected => Int` / `got => 'foo'` succeed.
    attrs.insert(
        "expected".to_string(),
        crate::value::expected_type_object(expected),
    );
    attrs.insert("got".to_string(), val.clone());
    attrs.insert("symbol".to_string(), Value::str(display_name));
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    RuntimeError::typed("X::TypeCheck::Assignment", attrs)
}

/// Build the standard X::TypeCheck::Assignment error for array/hash elements:
/// `Type check failed for an element of @a; expected Int but got Str ("hi")`
pub(crate) fn type_check_element_error(var_name: &str, expected: &str, val: &Value) -> String {
    let display_name = format_var_name_for_error(var_name);
    let got_type = value_type_name(val);
    let repr = value_short_repr(val);
    if repr.is_empty() {
        format!(
            "Type check failed for an element of {}; expected {} but got {}",
            display_name, expected, got_type
        )
    } else {
        format!(
            "Type check failed for an element of {}; expected {} but got {} {}",
            display_name, expected, got_type, repr
        )
    }
}

/// Build a structured X::TypeCheck::Assignment RuntimeError for element type checks.
pub(crate) fn type_check_element_typed_error(
    var_name: &str,
    expected: &str,
    val: &Value,
) -> RuntimeError {
    let msg = type_check_element_error(var_name, expected, val);
    let display_name = format_var_name_for_error(var_name);
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("expected".to_string(), Value::str(expected.to_string()));
    // `.got` is the offending value itself (e.g. `42`), so `$!.got ~~ Int` holds,
    // matching Rakudo's X::TypeCheck.
    attrs.insert("got".to_string(), val.clone());
    attrs.insert("symbol".to_string(), Value::str(display_name));
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    RuntimeError::typed("X::TypeCheck::Assignment", attrs)
}

/// Compute the `.WHICH` string for a value, used as the internal key
/// in object hashes (`my %h{Any}`).
pub(crate) fn value_which_key(value: &Value) -> String {
    match value.view() {
        ValueView::Int(n) => format!("Int|{}", n),
        ValueView::BigInt(n) => format!("Int|{}", *n),
        ValueView::Num(n) => format!("Num|{}", n),
        ValueView::Str(s) => format!("Str|{}", *s),
        ValueView::Bool(b) => format!("Bool|{}", if b { 1 } else { 0 }),
        ValueView::Rat(n, d) => format!("Rat|{}/{}", n, d),
        ValueView::FatRat(n, d) => format!("FatRat|{}/{}", n, d),
        ValueView::BigRat(n, d) => {
            let flavour = if value.is_bigfatrat() {
                "FatRat"
            } else {
                "Rat"
            };
            format!("{}|{}/{}", flavour, n, d)
        }
        ValueView::Complex(r, i) => format!("Complex|{}+{}i", r, i),
        ValueView::Nil => format!("Nil|U{}", Symbol::intern("Nil").id()),
        ValueView::Package(name) => format!("{}|U{}", name.resolve(), name.id()),
        ValueView::CustomType(c) => format!("{}|U{}", c.name.resolve(), c.id),
        ValueView::Instance { id, .. } => {
            format!("{}|{}", value_type_name(value), id)
        }
        ValueView::Array(items, ..) => format!("Array|{:p}", crate::gc::Gc::as_ptr(&items)),
        ValueView::Hash(map) => format!("Hash|{:p}", crate::gc::Gc::as_ptr(&map)),
        // A Pair with a plain string key and a ValuePair holding a Str key are
        // the same identity (`("x" => 1) === (:x(1))`), so both render the key
        // through its own `.WHICH` (`Pair|Str|x|Int|1`, raku's format).
        ValueView::Pair(k, v) => format!("Pair|Str|{}|{}", k, value_which_key(v)),
        ValueView::ValuePair(k, v) => format!("Pair|{}|{}", value_which_key(k), value_which_key(v)),
        ValueView::Enum { enum_type, key, .. } => {
            format!("{}|{}", enum_type.resolve(), key.resolve())
        }
        // A `but`-mixed value (e.g. `"quux" but $role`) keeps the identity of
        // its base value but is a DISTINCT object per mixed-in role set — raku's
        // `.WHICH` is `Str+{<role>}|quux`. Fold the (sorted) mixin type/role
        // names into the key so two different roles over the same base value are
        // distinct object-hash keys, while the same value+role collides.
        //
        // An allomorph (IntStr/NumStr/RatStr/ComplexStr — a numeric inner with a
        // preserved `Str` part) keys by BOTH halves, matching raku's
        // `IntStr|Int|1|Str|1`: `IntStr.new(1, "one")` and `IntStr.new(1, "1")`
        // are distinct identities, which the role-name fold alone would collapse.
        ValueView::Mixin(inner, mixins) => {
            if let Some(allo_name) = crate::value::types::allomorph_type_name(inner, mixins) {
                let str_part = mixins
                    .get("Str")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                return format!("{}|{}|Str|{}", allo_name, value_which_key(inner), str_part);
            }
            let mut roles: Vec<&str> = mixins.keys().map(|s| s.as_str()).collect();
            roles.sort_unstable();
            format!(
                "{}+{{{}}}|{}",
                value_type_name(inner),
                roles.join(","),
                value_which_key(inner)
            )
        }
        _ => {
            format!("{}|{}", value_type_name(value), value.to_string_value())
        }
    }
}
