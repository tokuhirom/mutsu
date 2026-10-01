//! The `X::TypeCheck::*` builders: the message and the exception object for a
//! value that fails a type constraint (assignment, element store, `:=` binding),
//! and the pure helpers naming the offending value (`got_type_name`,
//! `value_short_repr`).
//!
//! An object's `(repr)` is its `.raku`, a method call, so every builder takes the
//! repr as a parameter; `Interpreter::type_check_got_repr` renders it where an
//! interpreter is at hand (`runtime/type_check_repr.rs`).

use super::*;

/// Format a short representation of a value for type-check error messages,
/// matching Raku's format: e.g. `("hello")`, `(42)`, `([1, 2])`.
///
/// The text is the value's `.raku`, rendered by the one pure renderer
/// (`raku_value`) so every value kind -- numbers, `Rat`s, enums, `Pair`s,
/// `Range`s, `List`s, `Array`s, `Hash`es, `Set`s, ... -- reads exactly as the
/// method does.
///
/// A value whose `.raku` is only reachable through method dispatch (an
/// `Instance`, a `Sub`, or a container holding one) has no pure repr: a class
/// may override `raku`, so it answers `""` here and the interpreter-aware
/// `Interpreter::type_check_got_repr` supplies it where one is at hand.
// Cost: O(t), t = length of the value's `.raku` text (the whole structure is
// rendered, then cut to a constant length), as rakudo does.
pub(crate) fn value_short_repr(val: &Value) -> String {
    let val = &decont_for_repr(val);
    if crate::builtins::methods_0arg::raku_repr::needs_raku_dispatch(val)
        || crate::runtime::container_needs_raku_dispatch(val)
    {
        return String::new();
    }
    short_repr_of_raku(&crate::builtins::methods_0arg::raku_repr::raku_value(val))
}

/// `val` out of its `$` container, the way the type check saw it. A `for`
/// variable or a `my $x = (1, 2, 3)` holds its value itemized, which `.raku`
/// shows as `$(1, 2, 3)`; rakudo's message names the value itself
/// (`got List ((1, 2, 3))`), so the itemization is dropped before rendering.
// Cost: O(1).
pub(crate) fn decont_for_repr(val: &Value) -> Value {
    let val = val.clone().deitemize_for_sigil_bind();
    match val.view() {
        // A `Seq` is itemized on its handle, not by a `Scalar` wrapper.
        ValueView::Seq(body) if body.view() == crate::value::SeqView::ItemSeq => {
            Value::seq_body(body.as_bare_seq_view())
        }
        _ => val,
    }
}

/// Longest `.raku` text a type-check message shows in full; a longer one is cut
/// to [`SHORT_REPR_KEEP`] characters plus an ellipsis, as rakudo's
/// `X::TypeCheck` does (`got Str ("aaaaaaaaaaaaaaaaaaa...)`).
const SHORT_REPR_LIMIT: usize = 23;
const SHORT_REPR_KEEP: usize = 20;

/// Wrap a value's `.raku` text as the `(repr)` suffix of a type-check message,
/// truncating a long one the way rakudo does. Counts characters, not bytes.
// Cost: O(min(n, SHORT_REPR_LIMIT)), n = length of `raku` in characters.
pub(crate) fn short_repr_of_raku(raku: &str) -> String {
    match raku.char_indices().nth(SHORT_REPR_LIMIT) {
        Some(_) => {
            let keep_end = raku
                .char_indices()
                .nth(SHORT_REPR_KEEP)
                .map_or(raku.len(), |(i, _)| i);
            format!("({}...)", &raku[..keep_end])
        }
        None => format!("({raku})"),
    }
}

/// The type name to report as `got` in a type-check message. A type object
/// names ITSELF (`Int`), not the `Package` its runtime representation is —
/// rakudo says `but got Int (Int)`.
pub(crate) fn got_type_name(val: &Value) -> String {
    match val.view() {
        // `value_type_name` answers the generic `Any` for every instance; the
        // message names the object's class (`but got F (F.new)`).
        ValueView::Instance { class_name, .. } => {
            crate::value::user_facing_type_name(&class_name.resolve()).into_owned()
        }
        ValueView::Package(sym) => {
            let name = sym.resolve();
            if name.contains('\u{0}') {
                return crate::value::user_facing_type_name(&name).into_owned();
            }
            crate::value::enum_display_name(&name).unwrap_or(name)
        }
        ValueView::Enum { enum_type, .. } => {
            let name = enum_type.resolve();
            crate::value::enum_display_name(&name).unwrap_or(name)
        }
        _ => value_type_name(val).to_string(),
    }
}

/// True when `val` is the type object of the constraint's own nominal type —
/// the shape rakudo appends its "perhaps Nil" hint to. `my Str:D $x = Nil`
/// resets the container to `Str`, so the reported `got` is `Str` and the hint
/// applies; `my Str:D $x = Int` reports `Int` and gets no hint.
pub(crate) fn is_nominal_type_object_of(constraint: &str, val: &Value) -> bool {
    let ValueView::Package(sym) = val.view() else {
        return false;
    };
    let nominal = constraint
        .split_once(':')
        .map(|(base, _)| base)
        .unwrap_or(constraint);
    sym.resolve() == nominal
}

/// The `X::TypeCheck::Assignment` rakudo raises when a `:D`-constrained slot
/// ends up holding a type object, e.g.
/// `Type check failed in assignment to $!n; expected Int:D but got Int (Int)
///  (perhaps Nil was assigned to a :D which had no default?)`.
/// `constraint` carries the smiley (`Int:D`).
pub(crate) fn definite_type_check_assignment_error(
    var_name: &str,
    constraint: &str,
    val: &Value,
) -> RuntimeError {
    let mut msg = type_check_assignment_error(var_name, constraint, val);
    if is_nominal_type_object_of(constraint, val) {
        msg.push_str(" (perhaps Nil was assigned to a :D which had no default?)");
    }
    assignment_error_with_message(var_name, constraint, val, msg)
}

/// The compile-time-shaped `X::TypeCheck::Attribute::Default` rakudo raises for
/// an attribute initializer that can never satisfy the constraint, e.g.
/// `Can never assign default value Str ("str") to attribute '$!n', it expects: Int:D`.
pub(crate) fn attribute_default_never_assign_error(
    attr_name: &str,
    constraint: &str,
    val: &Value,
) -> RuntimeError {
    let repr = value_short_repr(val);
    let got = got_type_name(val);
    let msg = if repr.is_empty() {
        format!(
            "Can never assign default value {} to attribute '$!{}', it expects: {}",
            got, attr_name, constraint
        )
    } else {
        format!(
            "Can never assign default value {} {} to attribute '$!{}', it expects: {}",
            got, repr, attr_name, constraint
        )
    };
    let mut attrs = ValueMap::default();
    attrs.insert("name".to_string(), Value::str(format!("$!{}", attr_name)));
    attrs.insert(
        "expected".to_string(),
        crate::value::expected_type_object(constraint),
    );
    attrs.insert("got".to_string(), val.clone());
    attrs.insert("message".to_string(), Value::str(msg));
    RuntimeError::typed("X::TypeCheck::Attribute::Default", attrs)
}

/// Format the variable name for error messages, adding `$` sigil for
/// scalar variables that don't already have a sigil prefix.
/// Prefix of the compiler temp an lvalue subscript chain rooted at a method call
/// is bound to (`compile_expr_index_assign`). The rest of the name is the
/// accessor spelling plus a `#<n>` uniquifier, so an element type check raised
/// against the temp can still report `@!a` instead of an internal name.
pub(crate) const LVALUE_ROOT_TEMP_PREFIX: &str = "__mutsu_lvroot_";

pub(crate) fn format_var_name_for_error(name: &str) -> String {
    // An lvalue chain rooted at a method call runs against a compiler temp
    // (`__mutsu_lvroot_@.a#37[0]`). Recover the accessor spelling the user
    // actually wrote so the error reads `@!a[0]`, like rakudo, instead of
    // leaking the temp.
    if let Some(rest) = name.strip_prefix(LVALUE_ROOT_TEMP_PREFIX) {
        if let Some(hash) = rest.find('#') {
            let (accessor, tail) = rest.split_at(hash);
            let subscript = tail.find(['[', '{']).map(|i| &tail[i..]).unwrap_or("");
            return format!(
                "{}{}",
                format_var_name_for_error(&format!("{}.{}", &accessor[..1], &accessor[1..])),
                subscript
            );
        }
        return format_var_name_for_error(rest);
    }
    // Rakudo always names the ATTRIBUTE in an assignment error, whichever syntax
    // wrote it: `$.n = $v` (through the `is rw` accessor) reports `$!n`, exactly
    // as a direct `$!n = $v` does. Normalize the `.`-twigil accessor form here so
    // every assignment path agrees without each having to remember.
    let name = match name.as_bytes() {
        [b'$' | b'@' | b'%' | b'&', b'.', ..] => {
            let mut normalized = String::with_capacity(name.len());
            normalized.push(name.as_bytes()[0] as char);
            normalized.push('!');
            normalized.push_str(&name[2..]);
            return normalized;
        }
        [b'.', rest @ ..] if !rest.is_empty() => return format!("$!{}", &name[1..]),
        _ => name,
    };
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
    type_check_assignment_error_with_repr(var_name, expected, val, &value_short_repr(val))
}

/// [`type_check_assignment_error`] with the `(repr)` suffix supplied by the
/// caller (`""` for none), for a value whose repr needs the interpreter.
pub(crate) fn type_check_assignment_error_with_repr(
    var_name: &str,
    expected: &str,
    val: &Value,
    repr: &str,
) -> String {
    let display_name = format_var_name_for_error(var_name);
    // A package-scoped enum's constraint names its qualified identity, but
    // the message names it the way rakudo does: by its declared name (#9654).
    let expected_display = crate::value::enum_display_name(expected);
    let expected = expected_display.as_deref().unwrap_or(expected);
    // A lexical (`my`) type's storage name carries its declaration-site id
    // (ADR-0047); the message names it by its source spelling.
    let demangled;
    let expected = if expected.contains('\u{0}') {
        demangled = crate::value::user_facing_type_name(expected).into_owned();
        demangled.as_str()
    } else {
        expected
    };
    let got_type = got_type_name(val);
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
///
/// `repr` is the `(repr)` tail (`""` for none); `Interpreter::type_check_binding_failure`
/// supplies an object's `.raku`.
pub(crate) fn type_check_binding_typed_error(
    expected: &str,
    val: &Value,
    repr: &str,
) -> RuntimeError {
    let got_type = got_type_name(val);
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
    let mut attrs = ValueMap::default();
    attrs.insert(
        "expected".to_string(),
        crate::value::expected_type_object(expected),
    );
    attrs.insert("got".to_string(), val.clone());
    attrs.insert("operation".to_string(), Value::str("bind".to_string()));
    attrs.insert("message".to_string(), Value::str(msg));
    RuntimeError::typed("X::TypeCheck::Binding", attrs)
}

/// Build a structured X::TypeCheck::Assignment RuntimeError.
/// This creates a proper exception object that `throws-like` can match.
pub(crate) fn type_check_assignment_typed_error(
    var_name: &str,
    expected: &str,
    val: &Value,
) -> RuntimeError {
    type_check_assignment_typed_error_with_repr(var_name, expected, val, &value_short_repr(val))
}

/// [`type_check_assignment_typed_error`] with the `(repr)` suffix supplied by
/// the caller (`""` for none). `Interpreter::type_check_assignment_failure` is
/// the usual caller: it renders an object's `.raku` through method dispatch.
pub(crate) fn type_check_assignment_typed_error_with_repr(
    var_name: &str,
    expected: &str,
    val: &Value,
    repr: &str,
) -> RuntimeError {
    let msg = type_check_assignment_error_with_repr(var_name, expected, val, repr);
    assignment_error_with_message(var_name, expected, val, msg)
}

/// Shared body of the `X::TypeCheck::Assignment` builders: same attributes,
/// caller-supplied message (the `:D` path appends rakudo's "perhaps Nil" hint).
fn assignment_error_with_message(
    var_name: &str,
    expected: &str,
    val: &Value,
    msg: String,
) -> RuntimeError {
    let display_name = format_var_name_for_error(var_name);
    let mut attrs = ValueMap::default();
    // raku exposes `.expected` as the expected type OBJECT and `.got` as the
    // offending VALUE (not its type name), so `throws-like` matchers like
    // `expected => Int` / `got => 'foo'` succeed.
    attrs.insert(
        "expected".to_string(),
        crate::value::expected_type_object(expected),
    );
    attrs.insert("got".to_string(), val.clone());
    attrs.insert("symbol".to_string(), Value::str(display_name));
    attrs.insert("message".to_string(), Value::str(msg));
    RuntimeError::typed("X::TypeCheck::Assignment", attrs)
}

/// Build the standard X::TypeCheck::Assignment error message for array/hash
/// elements: `Type check failed for an element of @a; expected Int but got Str ("hi")`.
/// `repr` is the `(repr)` tail (`""` for none).
fn type_check_element_error(var_name: &str, expected: &str, val: &Value, repr: &str) -> String {
    let display_name = format_var_name_for_error(var_name);
    let got_type = got_type_name(val);
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

/// Build a structured X::TypeCheck::Assignment RuntimeError for element type
/// checks, its message naming the value by `repr` (`""` for none). An object's
/// repr is its `.raku`, so `Interpreter::type_check_element_failure` supplies it.
pub(crate) fn type_check_element_typed_error_with_repr(
    var_name: &str,
    expected: &str,
    val: &Value,
    repr: &str,
) -> RuntimeError {
    let msg = type_check_element_error(var_name, expected, val, repr);
    let display_name = format_var_name_for_error(var_name);
    let mut attrs = ValueMap::default();
    // `.expected` is the expected TYPE OBJECT, as raku's X::TypeCheck exposes
    // it (`$!.expected.^name` is `Int`, not `Str`) -- matching the sibling
    // `RuntimeError::typecheck_assignment_with_repr`, which has always used it.
    attrs.insert(
        "expected".to_string(),
        crate::value::expected_type_object(expected),
    );
    // `.got` is the offending value itself (e.g. `42`), so `$!.got ~~ Int` holds,
    // matching Rakudo's X::TypeCheck.
    attrs.insert("got".to_string(), val.clone());
    attrs.insert("symbol".to_string(), Value::str(display_name));
    attrs.insert("message".to_string(), Value::str(msg));
    RuntimeError::typed("X::TypeCheck::Assignment", attrs)
}
