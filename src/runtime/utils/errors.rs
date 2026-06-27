use super::*;

pub(crate) fn merge_junction(kind: JunctionKind, left: Value, right: Value) -> Value {
    // Infix junction operators (|, &, ^) always create a new 2-element
    // junction without flattening. List-associative flattening is handled
    // at compile time via JunctionAnyN/AllN/OneN opcodes.
    Value::junction(kind, vec![left, right])
}

/// Format a short representation of a value for type-check error messages,
/// matching Raku's format: e.g. `("hello")`, `(42)`.
pub(crate) fn value_short_repr(val: &Value) -> String {
    match val {
        Value::Str(s) => format!("(\"{}\")", s),
        Value::Int(n) => format!("({})", n),
        Value::BigInt(n) => format!("({})", n),
        Value::Num(n) => format!("({})", n),
        Value::Bool(b) => format!("({})", if *b { "True" } else { "False" }),
        Value::Rat(n, d) => format!("({}/{})", n, d),
        Value::BigRat(n, d) => format!("({}/{})", n, d),
        Value::FatRat(n, d) => format!("(FatRat.new({}, {}))", n, d),
        Value::Nil => "(Nil)".to_string(),
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
            "X::TypeCheck::Assignment: Type check failed in assignment to {}; expected {} but got {}",
            display_name, expected, got_type
        )
    } else {
        format!(
            "X::TypeCheck::Assignment: Type check failed in assignment to {}; expected {} but got {} {}",
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
    let msg = if repr.is_empty() {
        format!(
            "X::TypeCheck::Binding: Type check failed in binding; expected {} but got {}",
            expected, got_type
        )
    } else {
        format!(
            "X::TypeCheck::Binding: Type check failed in binding; expected {} but got {} {}",
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
            "X::TypeCheck::Assignment: Type check failed for an element of {}; expected {} but got {}",
            display_name, expected, got_type
        )
    } else {
        format!(
            "X::TypeCheck::Assignment: Type check failed for an element of {}; expected {} but got {} {}",
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
    match value {
        Value::Int(n) => format!("Int|{}", n),
        Value::BigInt(n) => format!("Int|{}", n),
        Value::Num(n) => format!("Num|{}", n),
        Value::Str(s) => format!("Str|{}", s),
        Value::Bool(b) => format!("Bool|{}", if *b { 1 } else { 0 }),
        Value::Rat(n, d) => format!("Rat|{}/{}", n, d),
        Value::FatRat(n, d) => format!("FatRat|{}/{}", n, d),
        Value::BigRat(n, d) => format!("Rat|{}/{}", n, d),
        Value::Complex(r, i) => format!("Complex|{}+{}i", r, i),
        Value::Nil => format!("Nil|U{}", Symbol::intern("Nil").id()),
        Value::Package(name) => format!("{}|U{}", name.resolve(), name.id()),
        Value::CustomType(c) => format!("{}|U{}", c.name.resolve(), c.id),
        Value::Instance { id, .. } => {
            format!("{}|{}", value_type_name(value), id)
        }
        Value::Array(items, ..) => format!("Array|{:p}", Arc::as_ptr(items)),
        Value::Hash(map) => format!("Hash|{:p}", Arc::as_ptr(map)),
        Value::Pair(k, v) => format!("Pair|{}|{}", k, value_which_key(v)),
        Value::ValuePair(k, v) => format!("Pair|{}|{}", value_which_key(k), value_which_key(v)),
        Value::Enum { enum_type, key, .. } => {
            format!("{}|{}", enum_type.resolve(), key.resolve())
        }
        // A `but`-mixed value (e.g. `"quux" but $role`) keeps the identity of
        // its base value but is a DISTINCT object per mixed-in role set — raku's
        // `.WHICH` is `Str+{<role>}|quux`. Fold the (sorted) mixin type/role
        // names into the key so two different roles over the same base value are
        // distinct object-hash keys, while the same value+role collides.
        Value::Mixin(inner, mixins) => {
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
