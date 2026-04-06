#![allow(clippy::result_large_err)]

use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{ArrayKind, RuntimeError, Value};

/// Create a Failure value wrapping an X::OutOfRange exception with the given message.
/// Used for operations that should soft-fail rather than throw.
fn out_of_range_failure(message: &str) -> Value {
    let mut ex_attrs = HashMap::new();
    ex_attrs.insert("message".to_string(), Value::str(message.to_string()));
    let ex = Value::make_instance(Symbol::intern("X::OutOfRange"), ex_attrs);
    let mut failure_attrs = HashMap::new();
    failure_attrs.insert("exception".to_string(), ex);
    Value::make_instance(Symbol::intern("Failure"), failure_attrs)
}
use num_bigint::BigInt;
use num_traits::{ToPrimitive, Zero};
use std::collections::HashMap;
use std::sync::Arc;

/// ACCEPTS for allomorphic types (IntStr, RatStr, NumStr, ComplexStr).
/// The allomorph (target) accepts the argument if:
/// - arg is a plain Str: Str parts are equal
/// - arg is numeric (not allomorph, not Str): numeric parts are equal (==)
/// - arg is allomorphic: numeric parts are equal (==)
/// - arg has .Numeric (custom class): numeric parts are equal (==)
fn allomorph_accepts(target: &Value, arg: &Value) -> Option<bool> {
    let Value::Mixin(target_inner, target_mixins) = target else {
        return Some(false);
    };
    let target_str = target_mixins
        .get("Str")
        .map(|v| v.to_string_value())
        .unwrap_or_default();

    // Unwrap Scalar
    let arg = match arg {
        Value::Scalar(inner) => inner.as_ref(),
        other => other,
    };

    match arg {
        // Plain string: compare with the Str part
        Value::Str(s) => Some(**s == target_str),
        // Allomorphic argument: compare numeric parts
        Value::Mixin(arg_inner, arg_mixins) if arg_mixins.contains_key("Str") => {
            Some(allomorph_values_numerically_equal(target_inner, arg_inner))
        }
        // Numeric types: compare with the numeric part
        Value::Int(_)
        | Value::BigInt(_)
        | Value::Num(_)
        | Value::Rat(_, _)
        | Value::FatRat(_, _)
        | Value::BigRat(_, _)
        | Value::Complex(_, _)
        | Value::Bool(_) => Some(allomorph_values_numerically_equal(target_inner, arg)),
        // Instance (custom class with .Numeric): needs interpreter to call methods
        Value::Instance { .. } => None,
        _ => Some(false),
    }
}

/// Check if two values are numerically equal (like Raku's == operator).
fn allomorph_values_numerically_equal(a: &Value, b: &Value) -> bool {
    let a_f = allomorph_value_to_f64(a);
    let b_f = allomorph_value_to_f64(b);
    match (a_f, b_f) {
        (Some(af), Some(bf)) => {
            // Handle Complex: both real and imaginary must match
            if let (Value::Complex(ar, ai), Value::Complex(br, bi)) = (a, b) {
                return (ar - br).abs() < 1e-15 && (ai - bi).abs() < 1e-15;
            }
            // For Complex vs non-Complex
            if let Value::Complex(ar, ai) = a {
                return (ar - bf).abs() < 1e-15 && ai.abs() < 1e-15;
            }
            if let Value::Complex(br, bi) = b {
                return (af - br).abs() < 1e-15 && bi.abs() < 1e-15;
            }
            (af - bf).abs() < 1e-15
        }
        _ => false,
    }
}

fn allomorph_value_to_f64(v: &Value) -> Option<f64> {
    match v {
        Value::Int(i) => Some(*i as f64),
        Value::BigInt(n) => n.to_f64(),
        Value::Num(f) => Some(*f),
        Value::Rat(n, d) if *d != 0 => Some(*n as f64 / *d as f64),
        Value::FatRat(n, d) if *d != 0 => Some(*n as f64 / *d as f64),
        Value::BigRat(n, d) if !d.is_zero() => {
            Some(n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0))
        }
        Value::Complex(r, _) => Some(*r),
        Value::Bool(b) => Some(if *b { 1.0 } else { 0.0 }),
        Value::Mixin(inner, _) => allomorph_value_to_f64(inner),
        _ => None,
    }
}

/// Raku's Str.indent($steps) method implementation.
/// $?TABSTOP is hardcoded to 8.
/// Returns (result_string, optional_warning_message).
fn str_indent(s: &str, arg: &Value) -> (String, Option<String>) {
    const TABSTOP: usize = 8;

    // Determine the indent amount
    let is_whatever = matches!(arg, Value::Whatever);
    let steps: i64 = if is_whatever {
        0 // will be computed below
    } else {
        match arg {
            Value::Int(i) => *i,
            Value::BigInt(bi) => bi.to_i64().unwrap_or(0),
            Value::Num(f) => *f as i64,
            Value::Bool(b) => {
                if *b {
                    1
                } else {
                    0
                }
            }
            Value::Str(sv) => {
                // Coerce string to Int: supports "0x10", "0e0", etc.
                if let Some(stripped) = sv.strip_prefix("0x").or_else(|| sv.strip_prefix("0X")) {
                    i64::from_str_radix(stripped, 16).unwrap_or(0)
                } else if sv.contains('e') || sv.contains('E') {
                    sv.parse::<f64>().map(|f| f as i64).unwrap_or(0)
                } else {
                    sv.parse::<i64>().unwrap_or(0)
                }
            }
            _ => 0,
        }
    };

    if s.is_empty() {
        return (String::new(), None);
    }

    // Split into lines, preserving trailing newline
    let has_trailing_newline = s.ends_with('\n');
    let lines: Vec<&str> = if has_trailing_newline {
        s.strip_suffix('\n').unwrap().split('\n').collect()
    } else {
        s.split('\n').collect()
    };

    if is_whatever {
        // Find minimum indent of non-empty lines
        let min_indent = lines
            .iter()
            .filter(|line| !line.is_empty())
            .map(|line| visual_indent_width(line, TABSTOP))
            .min()
            .unwrap_or(0);
        if min_indent == 0 {
            return (s.to_string(), None);
        }
        let result_lines: Vec<String> = lines
            .iter()
            .map(|line| {
                if line.is_empty() {
                    String::new()
                } else {
                    indent_line_negative(line, min_indent as i64, TABSTOP)
                }
            })
            .collect();
        let mut result = result_lines.join("\n");
        if has_trailing_newline {
            result.push('\n');
        }
        return (result, None);
    }

    if steps == 0 {
        return (s.to_string(), None);
    }

    // Check for excess outdent warning
    let warning = if steps < 0 {
        let outdent = -steps as usize;
        let min_indent = lines
            .iter()
            .filter(|line| !line.is_empty())
            .map(|line| visual_indent_width(line, TABSTOP))
            .min()
            .unwrap_or(0);
        if outdent > min_indent {
            Some(format!(
                "Asked to remove {} spaces, but the shortest indent is {} spaces",
                outdent, min_indent
            ))
        } else {
            None
        }
    } else {
        None
    };

    let result_lines: Vec<String> = lines
        .iter()
        .map(|line| {
            if line.is_empty() {
                String::new()
            } else if steps > 0 {
                indent_line_positive(line, steps as usize, TABSTOP)
            } else {
                indent_line_negative(line, -steps, TABSTOP)
            }
        })
        .collect();

    let mut result = result_lines.join("\n");
    if has_trailing_newline {
        result.push('\n');
    }
    (result, warning)
}

/// Calculate the visual width of the leading whitespace of a line.
fn visual_indent_width(line: &str, tabstop: usize) -> usize {
    let mut width = 0;
    for ch in line.chars() {
        if ch == '\t' {
            // Tab advances to next tabstop
            width = (width / tabstop + 1) * tabstop;
        } else if ch == ' ' || is_unicode_space(ch) {
            width += 1;
        } else {
            break;
        }
    }
    width
}

/// Check if a character is a Unicode space (non-ASCII whitespace used for indentation).
fn is_unicode_space(ch: char) -> bool {
    ch != ' ' && ch != '\t' && ch.is_whitespace() && ch != '\n' && ch != '\r'
}

/// Indent a line by adding `steps` whitespace characters.
/// Follows Raku's "same space" rule: if the existing leading whitespace
/// consists of a single type of character, extend with that character type.
fn indent_line_positive(line: &str, steps: usize, tabstop: usize) -> String {
    let leading: Vec<char> = line
        .chars()
        .take_while(|c| *c == ' ' || *c == '\t' || is_unicode_space(*c))
        .collect();
    let rest = &line[leading.iter().collect::<String>().len()..];

    if leading.is_empty() {
        // No existing indent: just prepend spaces
        return " ".repeat(steps) + rest;
    }

    // Check if all leading whitespace is the same character
    let first = leading[0];
    let all_same = leading.iter().all(|c| *c == first);

    if all_same {
        if first == ' ' {
            // All spaces: add more spaces
            return " ".repeat(leading.len() + steps) + rest;
        } else if first == '\t' {
            // All tabs: if steps is a multiple of tabstop, add tabs
            if steps.is_multiple_of(tabstop) {
                return "\t".repeat(leading.len() + steps / tabstop) + rest;
            }
            // Otherwise add spaces after tabs
            let leading_str: String = leading.iter().collect();
            return leading_str + &" ".repeat(steps) + rest;
        } else {
            // All same Unicode space: add more of it
            let extended: String = std::iter::repeat_n(first, leading.len() + steps).collect();
            return extended + rest;
        }
    }

    // Mixed whitespace: add spaces after existing whitespace
    let leading_str: String = leading.iter().collect();
    leading_str + &" ".repeat(steps) + rest
}

/// Outdent a line by removing `de_indent` visual columns of whitespace from the left.
/// Follows Rakudo's algorithm: work from left, remove chars, then coalesce spaces into
/// adjacent tabs when not tabstop-aligned.
fn indent_line_negative(line: &str, de_indent: i64, tabstop: usize) -> String {
    let de_indent = de_indent as usize;

    // Collect leading whitespace chars with their visual widths
    let mut indent_chars: Vec<(char, usize)> = Vec::new();
    let mut indent_size = 0usize;
    let mut byte_offset = 0;
    for ch in line.chars() {
        if ch == '\t' {
            let width = tabstop - (indent_size % tabstop);
            indent_chars.push((ch, width));
            indent_size += width;
            byte_offset += ch.len_utf8();
        } else if ch == ' ' || is_unicode_space(ch) {
            indent_chars.push((ch, 1));
            indent_size += 1;
            byte_offset += ch.len_utf8();
        } else {
            break;
        }
    }
    let rest = &line[byte_offset..];

    // Work forwards from the left, removing chars
    let mut pos = 0usize;
    let mut idx = 0;
    while idx < indent_chars.len() && pos < de_indent {
        let (ch, _width) = indent_chars[idx];
        if ch == '\t' {
            // Tab: snap pos to previous tabstop, then advance by tabstop
            pos -= pos % tabstop;
            pos += tabstop;
        } else {
            pos += 1;
        }
        idx += 1;
    }

    // Coalesce: if pos is not tabstop-aligned, look ahead for a tab
    // within the next (tabstop - pos%tabstop) chars and absorb it
    if idx < indent_chars.len() && !pos.is_multiple_of(tabstop) {
        let check_count = tabstop - (pos % tabstop);
        let look_end = (idx + check_count).min(indent_chars.len());
        // Find the first tab in the look-ahead range
        let tab_offset = indent_chars[idx..look_end]
            .iter()
            .position(|(ch, _)| *ch == '\t');
        if let Some(offset) = tab_offset {
            // Remove everything from idx to idx+offset (inclusive)
            idx += offset + 1;
            pos -= pos % tabstop;
            pos += tabstop;
        }
    }

    // Build result: remaining indent chars + overshoot spaces + rest
    let mut result = String::new();
    for (ch, _) in indent_chars.iter().skip(idx) {
        result.push(*ch);
    }
    if pos > de_indent {
        result.push_str(&" ".repeat(pos - de_indent));
    }
    result.push_str(rest);
    result
}

fn flatten_with_depth(
    value: &Value,
    depth: Option<usize>,
    out: &mut Vec<Value>,
    flatten_arrays: bool,
) {
    if let Some(0) = depth {
        out.push(value.clone());
        return;
    }
    match value {
        Value::Array(items, kind) if *kind == ArrayKind::List || flatten_arrays => {
            let next_depth = depth.map(|d| d.saturating_sub(1));
            for item in items.iter() {
                flatten_with_depth(item, next_depth, out, flatten_arrays);
            }
        }
        Value::Range(..)
        | Value::RangeExcl(..)
        | Value::RangeExclStart(..)
        | Value::RangeExclBoth(..)
        | Value::GenericRange { .. } => out.extend(crate::runtime::utils::value_to_list(value)),
        other => out.push(other.clone()),
    }
}

fn parse_flat_depth(arg: &Value) -> Option<usize> {
    match arg {
        Value::Int(n) => Some((*n).max(0) as usize),
        _ => None,
    }
}

fn is_hammer_pair(arg: &Value) -> bool {
    matches!(arg, Value::Pair(key, val) if key == "hammer" && val.truthy())
}

fn flatten_target(target: &Value, depth: Option<usize>, flatten_arrays: bool) -> Value {
    let mut flat = Vec::new();
    if let Some(items) = target.as_list_items() {
        for item in items.iter() {
            flatten_with_depth(item, depth, &mut flat, flatten_arrays);
        }
    } else {
        flatten_with_depth(target, depth, &mut flat, flatten_arrays);
    }
    Value::Seq(Arc::new(flat))
}

pub(crate) fn fmt_joinable_target(target: &Value) -> bool {
    matches!(
        target,
        Value::Array(..)
            | Value::Seq(..)
            | Value::Slip(..)
            | Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. }
    )
}

/// Extract key and value from a Pair or ValuePair.
fn pair_key_value(val: &Value) -> Option<(Value, Value)> {
    match val {
        Value::Pair(k, v) => Some((Value::str(k.to_string()), *v.clone())),
        Value::ValuePair(k, v) => Some((*k.clone(), *v.clone())),
        _ => None,
    }
}

/// Format a single value or a pair for `.fmt()`.
/// If the value is a Pair, format with two args (key, value).
/// Otherwise, format as a single arg.
fn fmt_single_or_pair(fmt: &str, item: &Value) -> String {
    if let Some((k, v)) = pair_key_value(item) {
        runtime::format_sprintf_args(fmt, &[k, v])
    } else {
        runtime::format_sprintf(fmt, Some(item))
    }
}

fn contains_value_recursive(hay: &str, needle: &Value) -> Value {
    match needle {
        Value::Junction { kind, values } => {
            let mapped = values
                .iter()
                .map(|v| contains_value_recursive(hay, v))
                .collect::<Vec<_>>();
            Value::junction(kind.clone(), mapped)
        }
        _ => Value::Bool(hay.contains(&needle.to_string_value())),
    }
}

fn sample_weighted_mix_key(items: &HashMap<String, f64>) -> Option<Value> {
    let mut total = 0.0;
    for weight in items.values() {
        if weight.is_finite() && *weight > 0.0 {
            total += *weight;
        }
    }
    if total <= 0.0 {
        return None;
    }
    let mut needle = crate::builtins::rng::builtin_rand() * total;
    for (key, weight) in items {
        if !weight.is_finite() || *weight <= 0.0 {
            continue;
        }
        if needle <= *weight {
            return Some(Value::str(key.clone()));
        }
        needle -= *weight;
    }
    items
        .iter()
        .find_map(|(key, weight)| (*weight > 0.0).then(|| Value::str(key.clone())))
}

fn sample_weighted_bag_key(items: &HashMap<String, i64>) -> Option<Value> {
    let mut total: i128 = 0;
    for count in items.values() {
        if *count > 0 {
            total += *count as i128;
        }
    }
    if total <= 0 {
        return None;
    }
    let needle_f = crate::builtins::rng::builtin_rand() * total as f64;
    let mut needle = needle_f as i128;
    if needle >= total {
        needle = total - 1;
    }
    for (key, count) in items {
        if *count <= 0 {
            continue;
        }
        if needle < *count as i128 {
            return Some(Value::str(key.clone()));
        }
        needle -= *count as i128;
    }
    items
        .iter()
        .find_map(|(key, count)| (*count > 0).then(|| Value::str(key.clone())))
}

fn int_to_superscript(n: i64) -> String {
    const SUPER_DIGITS: [char; 10] = [
        '\u{2070}', '\u{00B9}', '\u{00B2}', '\u{00B3}', '\u{2074}', '\u{2075}', '\u{2076}',
        '\u{2077}', '\u{2078}', '\u{2079}',
    ];
    let s = n.to_string();
    s.chars()
        .map(|c| match c {
            '-' => '\u{207B}', // superscript minus
            d if d.is_ascii_digit() => SUPER_DIGITS[(d as u8 - b'0') as usize],
            _ => c,
        })
        .collect()
}

fn int_to_subscript(n: i64) -> String {
    const SUB_DIGITS: [char; 10] = [
        '\u{2080}', '\u{2081}', '\u{2082}', '\u{2083}', '\u{2084}', '\u{2085}', '\u{2086}',
        '\u{2087}', '\u{2088}', '\u{2089}',
    ];
    let s = n.to_string();
    s.chars()
        .map(|c| match c {
            '-' => '\u{208B}', // subscript minus
            d if d.is_ascii_digit() => SUB_DIGITS[(d as u8 - b'0') as usize],
            _ => c,
        })
        .collect()
}

// ── 1-arg method dispatch ────────────────────────────────────────────
/// Try to dispatch a 1-argument method call on a Value.
/// Compute the nth roots of a number. Used by both the `.roots` method and the
/// `roots()` builtin function. Handles edge cases: n <= 0 returns [NaN],
/// NaN/Inf inputs with n=1 return the input as Complex.
pub(crate) fn compute_roots(target: &Value, n_arg: &Value) -> Value {
    let n_int = match n_arg {
        Value::Int(i) => *i,
        Value::Num(f) => *f as i64,
        Value::Rat(n, d) if *d != 0 => *n / *d,
        Value::BigInt(bi) => {
            use num_traits::ToPrimitive;
            bi.to_i64().unwrap_or(0)
        }
        Value::Str(s) => s.parse::<i64>().unwrap_or(0),
        Value::Bool(b) => {
            if *b {
                1
            } else {
                0
            }
        }
        _ => runtime::to_int(n_arg),
    };

    // n <= 0: return [NaN]
    if n_int <= 0 {
        return Value::array(vec![Value::Num(f64::NAN)]);
    }

    let n = n_int as usize;

    // Get the complex parts of the target
    let (re, im) = match runtime::to_complex_parts(target) {
        Some(parts) => parts,
        None => {
            // If we can't convert, try as float
            let f = runtime::to_float_value(target).unwrap_or(f64::NAN);
            (f, 0.0)
        }
    };

    // Handle NaN: return [NaN+0i] (Complex NaN)
    if re.is_nan() || im.is_nan() {
        let mut roots = Vec::with_capacity(n);
        for _ in 0..n {
            roots.push(Value::Complex(f64::NAN, 0.0));
        }
        return Value::array(roots);
    }

    // Handle Inf/-Inf: return [Inf+0i] or [-Inf+0i] etc.
    if re.is_infinite() || im.is_infinite() {
        let mut roots = Vec::with_capacity(n);
        // For n=1, return the value itself as Complex
        // For n>1, the roots involve Inf which is complex
        for k in 0..n {
            if n == 1 {
                roots.push(Value::Complex(re, im));
            } else {
                // Infinity roots: magnitude is Inf, angles vary
                let theta = im.atan2(re);
                let angle = (theta + 2.0 * std::f64::consts::PI * k as f64) / n as f64;
                let rr = f64::INFINITY * angle.cos();
                let ii = f64::INFINITY * angle.sin();
                if ii.abs() < 1e-12 {
                    roots.push(Value::Num(rr));
                } else {
                    roots.push(Value::Complex(rr, ii));
                }
            }
        }
        return Value::array(roots);
    }

    // Check if target is zero
    if re == 0.0 && im == 0.0 {
        let mut roots = Vec::with_capacity(n);
        for _ in 0..n {
            roots.push(Value::Complex(0.0, 0.0));
        }
        return Value::array(roots);
    }

    // Normal case: compute nth roots using polar form
    let r = (re * re + im * im).sqrt();
    let theta = im.atan2(re);
    let mag = r.powf(1.0 / n as f64);
    let mut roots = Vec::with_capacity(n);
    for k in 0..n {
        let angle = (theta + 2.0 * std::f64::consts::PI * k as f64) / n as f64;
        let rr = mag * angle.cos();
        let ii = mag * angle.sin();
        if ii.abs() < 1e-12 {
            roots.push(Value::Num(rr));
        } else {
            roots.push(Value::Complex(rr, ii));
        }
    }
    Value::array(roots)
}

pub(crate) fn native_method_1arg(
    target: &Value,
    method_sym: Symbol,
    arg: &Value,
) -> Option<Result<Value, RuntimeError>> {
    let method = method_sym.resolve();
    let method = method.as_str();

    if let Value::Scalar(inner) = target {
        return native_method_1arg(inner, method_sym, arg);
    }
    // Cool numeric coercion: when a Str calls a numeric 1-arg method, coerce to numeric first.
    // Also coerce the arg if it's a Str for numeric methods.
    {
        let numeric_1arg_methods: &[&str] = &[
            "exp", "log", "round", "roots", "unpolar", "base", "polymod", "expmod", "atan2",
        ];
        if numeric_1arg_methods.contains(&method) {
            let coerced_target = if let Value::Str(s) = target {
                if let Ok(i) = s.parse::<i64>() {
                    Some(Value::Int(i))
                } else if let Ok(f) = s.parse::<f64>() {
                    Some(Value::Num(f))
                } else {
                    return None;
                }
            } else {
                None
            };
            let coerced_arg = if let Value::Str(s) = arg {
                if let Ok(i) = s.parse::<i64>() {
                    Some(Value::Int(i))
                } else if let Ok(f) = s.parse::<f64>() {
                    Some(Value::Num(f))
                } else {
                    None
                }
            } else {
                None
            };
            if coerced_target.is_some() || coerced_arg.is_some() {
                let t = coerced_target.as_ref().unwrap_or(target);
                let a = coerced_arg.as_ref().unwrap_or(arg);
                return native_method_1arg(t, method_sym, a);
            }
        }
    }
    match method {
        // ACCEPTS for allomorphic types (IntStr, RatStr, NumStr, ComplexStr)
        "ACCEPTS" if matches!(target, Value::Mixin(_, m) if m.contains_key("Str")) => {
            // Instance args need the interpreter to call .Numeric, so return None
            allomorph_accepts(target, arg).map(|result| Ok(Value::Bool(result)))
        }
        // ACCEPTS for Range: value ~~ Range containment, Range ~~ Range subset
        "ACCEPTS" if target.is_range() => {
            let result = if arg.is_range() {
                // Range ~~ Range: subset check — delegate to pure_smart_match
                crate::vm::vm_smart_match::pure_smart_match(arg, target).unwrap_or(false)
            } else {
                // Value ~~ Range: containment check
                runtime::Interpreter::value_in_range(arg, target)
            };
            Some(Ok(Value::Bool(result)))
        }
        "Str" => {
            // Int.Str(:superscript) and Int.Str(:subscript)
            if let Value::Pair(key, val) = arg
                && val.truthy()
            {
                let int_val = match target {
                    Value::Int(i) => Some(*i),
                    Value::BigInt(bi) => bi.to_i64(),
                    Value::Num(f) => Some(*f as i64),
                    Value::Bool(b) => Some(if *b { 1 } else { 0 }),
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
        "chop" => {
            // Type objects (Package) should throw
            if let Value::Package(type_name) = target {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller chop({}:U)",
                    type_name,
                ))));
            }
            let s = target.to_string_value();
            let n = match arg {
                Value::Int(i) => (*i).max(0) as usize,
                Value::BigInt(bi) => {
                    use num_traits::ToPrimitive;
                    bi.to_usize().unwrap_or(usize::MAX)
                }
                Value::Num(f) => (*f as i64).max(0) as usize,
                _ => arg.to_string_value().parse::<usize>().unwrap_or(1),
            };
            let char_count = s.chars().count();
            let keep = char_count.saturating_sub(n);
            let result: String = s.chars().take(keep).collect();
            Some(Ok(Value::str(result)))
        }
        "uniprop" => {
            let prop_name = arg.to_string_value();
            match target {
                Value::Package(_) => {
                    let msg = "Cannot resolve caller uniprop".to_string();
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("message".to_string(), Value::str(msg.clone()));
                    let ex = Value::make_instance(
                        crate::symbol::Symbol::intern("X::Multi::NoMatch"),
                        attrs,
                    );
                    let mut err = RuntimeError::new(&msg);
                    err.exception = Some(Box::new(ex));
                    return Some(Err(err));
                }
                Value::Int(i) => {
                    let cp = *i as u32;
                    return Some(Ok(
                        crate::builtins::uniprop::unicode_property_value_for_codepoint(
                            cp,
                            Some(&prop_name),
                        ),
                    ));
                }
                _ => {}
            }
            let s = target.to_string_value();
            if s.is_empty() {
                return Some(Ok(Value::Nil));
            }
            let ch = s.chars().next().unwrap();
            Some(Ok(crate::builtins::uniprop::unicode_property_value(
                ch, &prop_name,
            )))
        }
        "unimatch" => {
            let prop_value = arg.to_string_value();
            match target {
                Value::Package(_) => {
                    let msg = "Cannot resolve caller unimatch".to_string();
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("message".to_string(), Value::str(msg.clone()));
                    let ex = Value::make_instance(
                        crate::symbol::Symbol::intern("X::Multi::NoMatch"),
                        attrs,
                    );
                    let mut err = RuntimeError::new(&msg);
                    err.exception = Some(Box::new(ex));
                    return Some(Err(err));
                }
                Value::Int(i) => {
                    let cp = *i as u32;
                    return Some(Ok(crate::builtins::uniprop::unimatch_for_codepoint(
                        cp,
                        &prop_value,
                        None,
                    )));
                }
                _ => {}
            }
            let s = target.to_string_value();
            if s.is_empty() {
                return Some(Ok(Value::Nil));
            }
            let ch = s.chars().next().unwrap();
            Some(Ok(Value::Bool(crate::builtins::uniprop::unimatch(
                ch,
                &prop_value,
                None,
            ))))
        }
        "uniprops" => {
            let prop_name = arg.to_string_value();
            let s = target.to_string_value();
            if s.is_empty() {
                return Some(Ok(Value::array(vec![])));
            }
            let props: Vec<Value> = s
                .chars()
                .map(|ch| crate::builtins::uniprop::unicode_property_value(ch, &prop_name))
                .collect();
            Some(Ok(Value::array(props)))
        }
        "contains" => {
            if let Value::Package(type_name) = arg {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller contains({}:U)",
                    type_name,
                ))));
            }
            let s = target.to_string_value();
            Some(Ok(contains_value_recursive(&s, arg)))
        }
        // starts-with and ends-with are handled in runtime/methods.rs
        // to support named args (:i, :ignorecase, :m, :ignoremark)
        "samemark" => {
            let target_str = target.to_string_value();
            let source_str = arg.to_string_value();
            Some(Ok(Value::str(crate::builtins::samemark_string(
                &target_str,
                &source_str,
            ))))
        }
        "samecase" => {
            let source_str = target.to_string_value();
            let pattern_str = arg.to_string_value();
            Some(Ok(Value::str(crate::builtins::samecase_string(
                &source_str,
                &pattern_str,
            ))))
        }
        "decode" => {
            let encoding = arg.to_string_value();
            super::decode_buf_method(target, Some(&encoding))
        }
        "Rat" => {
            // .Rat(epsilon) — use continued fraction algorithm with given epsilon
            let epsilon = match arg {
                Value::Num(f) => *f,
                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                Value::Int(i) => *i as f64,
                _ => 1e-6,
            };
            let result = match target {
                Value::Rat(_, _) => target.clone(),
                Value::Int(i) => Value::Rat(*i, 1),
                Value::Num(f) => {
                    if f.is_nan() {
                        Value::Rat(0, 0)
                    } else if f.is_infinite() {
                        if f.is_sign_positive() {
                            Value::Rat(1, 0)
                        } else {
                            Value::Rat(-1, 0)
                        }
                    } else {
                        super::num_to_rat_with_epsilon(*f, epsilon)
                    }
                }
                Value::FatRat(n, d) => Value::Rat(*n, *d),
                Value::Str(s) => {
                    if let Ok(f) = s.parse::<f64>() {
                        super::num_to_rat_with_epsilon(f, epsilon)
                    } else {
                        Value::Rat(0, 1)
                    }
                }
                _ => Value::Rat(0, 1),
            };
            Some(Ok(result))
        }
        "FatRat" => {
            // .FatRat or .FatRat(epsilon) — convert to FatRat
            let result = match target {
                Value::FatRat(_, _) => target.clone(),
                Value::Int(i) => Value::FatRat(*i, 1),
                Value::Rat(n, d) => Value::FatRat(*n, *d),
                Value::Num(f) => {
                    let denom = 1_000_000i64;
                    let numer = (f * denom as f64).round() as i64;
                    Value::FatRat(numer, denom)
                }
                _ => Value::FatRat(0, 1),
            };
            Some(Ok(result))
        }
        "index" => {
            // Fall through to runtime dispatch for type objects, named args (Pairs),
            // array of needles, and multi-arg calls handled by dispatch_index
            if matches!(arg, Value::Package(_) | Value::Pair(..) | Value::Array(..)) {
                return None;
            }
            let s = target.to_string_value();
            let needle = arg.to_string_value();
            match s.find(&needle) {
                Some(pos) => {
                    let char_pos = s[..pos].chars().count();
                    Some(Ok(Value::Int(char_pos as i64)))
                }
                None => Some(Ok(Value::Nil)),
            }
        }
        "substr" => {
            let start = match arg {
                Value::Int(i) if *i >= 0 => *i as usize,
                Value::Int(_) => return None, // negative: let runtime handle
                _ => return None,
            };
            let s = target.to_string_value();
            let chars: Vec<char> = s.chars().collect();
            if start > chars.len() {
                return None; // out-of-range: let runtime handle (returns Failure)
            }
            Some(Ok(Value::str(chars[start..].iter().collect())))
        }
        "indent" => {
            let s = target.to_string_value();
            let (result, warning) = str_indent(&s, arg);
            if let Some(warn_msg) = warning {
                return Some(Err(crate::value::RuntimeError::warn_signal_with_resume(
                    warn_msg,
                    Value::str(result),
                )));
            }
            Some(Ok(Value::str(result)))
        }
        "AT-POS" => {
            let idx = match arg {
                Value::Int(i) if *i >= 0 => *i as usize,
                Value::Num(f) if *f >= 0.0 => *f as usize,
                _ => return Some(Ok(Value::Nil)),
            };
            if let Some(items) = target.as_list_items() {
                Some(Ok(items.get(idx).cloned().unwrap_or(Value::Nil)))
            } else {
                match target {
                    Value::Str(s) => {
                        let ch = s.chars().nth(idx).map(|c| Value::str(c.to_string()));
                        Some(Ok(ch.unwrap_or(Value::Nil)))
                    }
                    Value::Instance {
                        class_name,
                        attributes,
                        ..
                    } if class_name == "Match" => {
                        if let Some(Value::Array(positional, ..)) = attributes.get("list") {
                            return Some(Ok(positional.get(idx).cloned().unwrap_or(Value::Nil)));
                        }
                        Some(Ok(Value::Nil))
                    }
                    Value::Instance {
                        class_name,
                        attributes,
                        ..
                    } if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) => {
                        if let Some(Value::Array(bytes, ..)) = attributes.get("bytes") {
                            return Some(Ok(bytes.get(idx).cloned().unwrap_or(Value::Int(0))));
                        }
                        Some(Ok(Value::Int(0)))
                    }
                    _ => None,
                }
            }
        }
        "split" => {
            if let Value::Instance { class_name, .. } = target
                && class_name == "Supply"
            {
                return None;
            }
            super::split::native_split_method(target, std::slice::from_ref(arg))
        }
        "lines" => {
            if let Value::Instance { class_name, .. } = target
                && class_name == "Supply"
            {
                return None;
            }
            let s = target.to_string_value();
            if let Value::Pair(key, value) = arg {
                if key == "chomp" {
                    let lines: Vec<Value> =
                        crate::builtins::split_lines_with_chomp(&s, value.truthy())
                            .into_iter()
                            .map(Value::str)
                            .collect();
                    return Some(Ok(Value::Seq(std::sync::Arc::new(lines))));
                }
                return None;
            }

            let mut lines = crate::builtins::split_lines_chomped(&s);
            let limit = match arg {
                Value::Int(i) => Some((*i).max(0) as usize),
                Value::BigInt(bi) => {
                    use num_traits::ToPrimitive;
                    Some(bi.to_usize().unwrap_or(usize::MAX))
                }
                Value::Whatever => None,
                Value::Num(f) if f.is_infinite() && f.is_sign_positive() => None,
                Value::Num(f) if *f >= 0.0 => Some(*f as usize),
                Value::Rat(n, d) if *d == 0 && *n > 0 => None,
                _ => return None,
            };
            if let Some(n) = limit {
                lines.truncate(n);
            }
            let lines: Vec<Value> = lines.into_iter().map(Value::str).collect();
            Some(Ok(Value::Seq(std::sync::Arc::new(lines))))
        }
        "words" => {
            let s = target.to_string_value();
            let limit = match arg {
                Value::Int(i) => Some((*i).max(0) as usize),
                Value::BigInt(bi) => {
                    use num_traits::ToPrimitive;
                    Some(bi.to_usize().unwrap_or(usize::MAX))
                }
                Value::Whatever => None,
                Value::Num(f) if f.is_infinite() && f.is_sign_positive() => None,
                Value::Num(f) if *f >= 0.0 => Some(*f as usize),
                Value::Rat(n, d) if *d == 0 && *n > 0 => None,
                _ => return None,
            };
            let mut words: Vec<Value> = s
                .split_whitespace()
                .map(|w| Value::str(w.to_string()))
                .collect();
            if let Some(n) = limit {
                words.truncate(n);
            }
            Some(Ok(Value::Seq(std::sync::Arc::new(words))))
        }
        "join" => {
            // Shaped arrays: join over leaves
            if crate::runtime::utils::is_shaped_array(target) {
                let leaves = crate::runtime::utils::shaped_array_leaves(target);
                let sep = arg.to_string_value();
                let joined = leaves
                    .iter()
                    .map(|v| v.to_str_context())
                    .collect::<Vec<_>>()
                    .join(&sep);
                return Some(Ok(Value::str(joined)));
            }
            if let Some(items) = target.as_list_items() {
                // If any item is an Instance, fall through to runtime
                // so user-defined Str() methods can be called.
                if items.iter().any(|v| matches!(v, Value::Instance { .. })) {
                    return None;
                }
                let sep = arg.to_string_value();
                let joined = items
                    .iter()
                    .map(|v| v.to_str_context())
                    .collect::<Vec<_>>()
                    .join(&sep);
                return Some(Ok(Value::str(joined)));
            }
            match target {
                Value::Capture { positional, .. } => {
                    let sep = arg.to_string_value();
                    let joined = positional
                        .iter()
                        .map(|v| v.to_string_value())
                        .collect::<Vec<_>>()
                        .join(&sep);
                    Some(Ok(Value::str(joined)))
                }
                Value::Pair(k, v) => {
                    let sep = arg.to_string_value();
                    Some(Ok(Value::str(format!(
                        "{}{}{}",
                        k,
                        sep,
                        v.to_string_value()
                    ))))
                }
                Value::ValuePair(k, v) => {
                    let sep = arg.to_string_value();
                    Some(Ok(Value::str(format!(
                        "{}{}{}",
                        k.to_string_value(),
                        sep,
                        v.to_string_value()
                    ))))
                }
                Value::Hash(map) => {
                    let sep = arg.to_string_value();
                    let joined = map
                        .iter()
                        .map(|(k, v)| format!("{}\t{}", k, v.to_string_value()))
                        .collect::<Vec<_>>()
                        .join(&sep);
                    Some(Ok(Value::str(joined)))
                }
                // Scalar values: .join returns the value as a string
                Value::Str(_)
                | Value::Int(_)
                | Value::Num(_)
                | Value::Rat(..)
                | Value::Bool(_)
                | Value::Instance { .. }
                | Value::Nil => Some(Ok(Value::str(target.to_string_value()))),
                // Other types (LazyList, etc.) fall through to the runtime handler
                _ => None,
            }
        }
        "flat" => {
            if is_hammer_pair(arg) {
                return Some(Ok(flatten_target(target, None, true)));
            }
            if let Some(depth) = parse_flat_depth(arg) {
                return Some(Ok(flatten_target(target, Some(depth), false)));
            }
            None
        }
        "head" => {
            let n: i64 = match arg {
                Value::Int(i) => *i,
                Value::Rat(num, den) => {
                    if *den == 0 {
                        0
                    } else {
                        *num / *den
                    }
                }
                Value::Num(f) => *f as i64,
                Value::BigInt(bi) => {
                    // For very large BigInts that don't fit in i64:
                    // negative => treat as negative (returns empty), positive => clamp to MAX
                    bi.to_i64()
                        .unwrap_or(if bi.sign() == num_bigint::Sign::Minus {
                            -1
                        } else {
                            i64::MAX
                        })
                }
                _ => return None,
            };
            if n <= 0 {
                return Some(Ok(Value::array(vec![])));
            }
            let n = n as usize;
            match target {
                Value::Array(items, ..) => {
                    Some(Ok(Value::array(items[..n.min(items.len())].to_vec())))
                }
                Value::Range(a, b) => {
                    let items: Vec<Value> = (*a..=*b).take(n).map(Value::Int).collect();
                    Some(Ok(Value::array(items)))
                }
                _ => {
                    let items = runtime::value_to_list(target);
                    Some(Ok(Value::array(items[..n.min(items.len())].to_vec())))
                }
            }
        }
        "tail" => match target {
            Value::Array(items, ..) => {
                let n = match arg {
                    Value::Int(i) => *i as usize,
                    _ => return None,
                };
                let start = items.len().saturating_sub(n);
                Some(Ok(Value::array(items[start..].to_vec())))
            }
            Value::Instance { class_name, .. } if class_name == "Supply" => None,
            _ => {
                let n = match arg {
                    Value::Int(i) => *i as usize,
                    _ => return None,
                };
                let items = runtime::value_to_list(target);
                let start = items.len().saturating_sub(n);
                Some(Ok(Value::array(items[start..].to_vec())))
            }
        },
        "combinations" => {
            let items = target
                .as_list_items()
                .map(|items| items.to_vec())
                .unwrap_or_else(|| runtime::value_to_list(target));
            match arg {
                Value::Range(a, b) => Some(Ok(Value::Seq(
                    super::methods_0arg::collection::combinations_range(&items, *a, *b).into(),
                ))),
                Value::RangeExcl(a, b) => Some(Ok(Value::Seq(
                    super::methods_0arg::collection::combinations_range(&items, *a, *b - 1).into(),
                ))),
                Value::RangeExclStart(a, b) => Some(Ok(Value::Seq(
                    super::methods_0arg::collection::combinations_range(&items, *a + 1, *b).into(),
                ))),
                Value::RangeExclBoth(a, b) => Some(Ok(Value::Seq(
                    super::methods_0arg::collection::combinations_range(&items, *a + 1, *b - 1)
                        .into(),
                ))),
                Value::GenericRange {
                    start,
                    end,
                    excl_start,
                    excl_end,
                } => {
                    let mut lo = runtime::to_int(start);
                    let mut hi = runtime::to_int(end);
                    if *excl_start {
                        lo += 1;
                    }
                    if *excl_end {
                        hi -= 1;
                    }
                    Some(Ok(Value::Seq(
                        super::methods_0arg::collection::combinations_range(&items, lo, hi).into(),
                    )))
                }
                _ => {
                    let k = runtime::to_int(arg);
                    if k < 0 {
                        Some(Ok(Value::Seq(Vec::new().into())))
                    } else {
                        Some(Ok(Value::Seq(
                            super::methods_0arg::collection::combinations_k(&items, k as usize)
                                .into(),
                        )))
                    }
                }
            }
        }
        "batch" => {
            let n = match arg {
                Value::Int(i) => *i,
                _ => return None,
            };
            if n < 1 {
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("got".to_string(), Value::Int(n));
                attrs.insert(
                    "message".to_string(),
                    Value::str(format!("batch size must be at least 1, got {}", n)),
                );
                let ex = Value::make_instance(Symbol::intern("X::OutOfRange"), attrs);
                let mut err = RuntimeError::new(format!(
                    "X::OutOfRange: batch size must be at least 1, got {}",
                    n
                ));
                err.exception = Some(Box::new(ex));
                return Some(Err(err));
            }
            let n = n as usize;
            let items = runtime::value_to_list(target);
            let batches: Vec<Value> = items
                .chunks(n)
                .map(|chunk| Value::array(chunk.to_vec()))
                .collect();
            Some(Ok(Value::Seq(batches.into())))
        }
        "rindex" => {
            // Fall through to runtime dispatch for arrays (list of needles)
            // and type objects
            if matches!(arg, Value::Array(..) | Value::Package(_) | Value::Pair(..)) {
                return None;
            }
            let s = target.to_string_value();
            let needle = arg.to_string_value();
            match s.rfind(&needle) {
                Some(pos) => {
                    let char_pos = s[..pos].chars().count();
                    Some(Ok(Value::Int(char_pos as i64)))
                }
                None => Some(Ok(Value::Nil)),
            }
        }
        "fmt" => {
            let fmt = arg.to_string_value();
            if let Value::Hash(items) = target {
                // Hash.fmt(format): format each key-value pair, join with "\n"
                let rendered = items
                    .iter()
                    .map(|(k, v)| {
                        runtime::format_sprintf_args(&fmt, &[Value::str(k.to_string()), v.clone()])
                    })
                    .collect::<Vec<_>>()
                    .join("\n");
                Some(Ok(Value::str(rendered)))
            } else if let Value::Bag(items, _) = target {
                let rendered = items
                    .iter()
                    .map(|(k, v)| {
                        runtime::format_sprintf_args(&fmt, &[Value::str(k.clone()), Value::Int(*v)])
                    })
                    .collect::<Vec<_>>()
                    .join("\n");
                Some(Ok(Value::str(rendered)))
            } else if let Value::Set(items, _) = target {
                let rendered = items
                    .iter()
                    .map(|k| runtime::format_sprintf_args(&fmt, &[Value::str(k.clone())]))
                    .collect::<Vec<_>>()
                    .join("\n");
                Some(Ok(Value::str(rendered)))
            } else if let Value::Mix(items, _) = target {
                let rendered = items
                    .iter()
                    .map(|(k, v)| {
                        runtime::format_sprintf_args(&fmt, &[Value::str(k.clone()), Value::Num(*v)])
                    })
                    .collect::<Vec<_>>()
                    .join("\n");
                Some(Ok(Value::str(rendered)))
            } else if let Some((k, v)) = pair_key_value(target) {
                // Pair.fmt(format): format key and value
                let rendered = runtime::format_sprintf_args(&fmt, &[k, v]);
                Some(Ok(Value::str(rendered)))
            } else if fmt_joinable_target(target) {
                // Use as_list_items() to bypass itemization — methods like
                // .fmt still iterate over inner elements of $[...] / $(...).
                let items: Vec<Value> = if let Some(inner) = target.as_list_items() {
                    inner.to_vec()
                } else {
                    runtime::value_to_list(target)
                };
                let rendered = items
                    .into_iter()
                    .map(|item| fmt_single_or_pair(&fmt, &item))
                    .collect::<Vec<_>>()
                    .join(" ");
                Some(Ok(Value::str(rendered)))
            } else {
                let rendered = runtime::format_sprintf(&fmt, Some(target));
                Some(Ok(Value::str(rendered)))
            }
        }
        "sprintf" => {
            // Method form: '%f'.sprintf(value) — target is the format string
            let fmt = target.to_string_value();
            let rendered = runtime::format_sprintf(&fmt, Some(arg));
            Some(Ok(Value::str(rendered)))
        }
        "zprintf" => {
            // Method form: '%f'.zprintf(value) — like sprintf but with zprintf semantics
            let fmt = target.to_string_value();
            let rendered = runtime::format_zprintf(&fmt, Some(arg));
            Some(Ok(Value::str(rendered)))
        }
        "parse-base" => {
            let radix = match arg {
                Value::Int(n) => *n as u32,
                _ => return None,
            };
            let s = target.to_string_value();
            match i64::from_str_radix(&s, radix) {
                Ok(n) => Some(Ok(Value::Int(n))),
                Err(_) => Some(Err(RuntimeError::new(format!(
                    "Cannot parse '{}' as base {}",
                    s, radix
                )))),
            }
        }
        "base" => match target {
            Value::Int(i) => {
                let radix = match arg {
                    Value::Int(r) if (2..=36).contains(r) => *r as u32,
                    Value::Int(_) => {
                        return Some(Ok(out_of_range_failure("base requires radix 2..36")));
                    }
                    Value::Str(s) => match s.parse::<u32>() {
                        Ok(r) if (2..=36).contains(&r) => r,
                        _ => {
                            return Some(Ok(out_of_range_failure("base requires radix 2..36")));
                        }
                    },
                    _ => {
                        return Some(Ok(out_of_range_failure("base requires radix 2..36")));
                    }
                };
                let negative = *i < 0;
                let mut n = if negative { (-*i) as u64 } else { *i as u64 };
                if n == 0 {
                    return Some(Ok(Value::str_from("0")));
                }
                let digits = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
                let mut buf = Vec::new();
                while n > 0 {
                    buf.push(digits[(n % radix as u64) as usize]);
                    n /= radix as u64;
                }
                if negative {
                    buf.push(b'-');
                }
                buf.reverse();
                Some(Ok(Value::str(String::from_utf8(buf).unwrap())))
            }
            Value::BigInt(n) => {
                let radix = match arg {
                    Value::Int(r) if (2..=36).contains(r) => *r as u32,
                    Value::Int(_) => {
                        return Some(Ok(out_of_range_failure("base requires radix 2..36")));
                    }
                    Value::Str(s) => match s.parse::<u32>() {
                        Ok(r) if (2..=36).contains(&r) => r,
                        _ => {
                            return Some(Ok(out_of_range_failure("base requires radix 2..36")));
                        }
                    },
                    _ => {
                        return Some(Ok(out_of_range_failure("base requires radix 2..36")));
                    }
                };
                use num_traits::{Signed, Zero};
                let negative = n.is_negative();
                let mut val = if negative {
                    -n.as_ref().clone()
                } else {
                    n.as_ref().clone()
                };
                if val.is_zero() {
                    return Some(Ok(Value::str_from("0")));
                }
                let digits = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
                let radix_big = num_bigint::BigInt::from(radix);
                let mut buf = Vec::new();
                while !val.is_zero() {
                    let rem = &val % &radix_big;
                    use num_traits::ToPrimitive;
                    let digit_idx = rem.to_usize().unwrap_or(0);
                    buf.push(digits[digit_idx]);
                    val /= &radix_big;
                }
                if negative {
                    buf.push(b'-');
                }
                buf.reverse();
                Some(Ok(Value::str(String::from_utf8(buf).unwrap())))
            }
            Value::Num(f) => {
                if f.is_infinite() || f.is_nan() {
                    return Some(Err(RuntimeError::new(format!(
                        "X::Numeric::CannotConvert: Cannot convert {} to base",
                        if f.is_nan() {
                            "NaN"
                        } else if *f > 0.0 {
                            "Inf"
                        } else {
                            "-Inf"
                        },
                    ))));
                }
                let radix = match arg {
                    Value::Int(r) if (2..=36).contains(r) => *r as u32,
                    Value::Int(_) => {
                        return Some(Ok(out_of_range_failure("base requires radix 2..36")));
                    }
                    Value::Str(s) => match s.parse::<u32>() {
                        Ok(r) if (2..=36).contains(&r) => r,
                        _ => {
                            return Some(Ok(out_of_range_failure("base requires radix 2..36")));
                        }
                    },
                    _ => {
                        return Some(Ok(out_of_range_failure("base requires radix 2..36")));
                    }
                };
                // Convert Num to Rat for precise base conversion
                let (n, d) = f64_to_rat(*f);
                Some(Ok(Value::str(rat_to_base(n, d, radix, BaseDigits::Auto))))
            }
            Value::Rat(n, d) | Value::FatRat(n, d) => {
                let radix = match parse_radix_checked(arg)? {
                    Ok(r) => r,
                    Err(_) => return Some(Ok(out_of_range_failure("base requires radix 2..36"))),
                };
                Some(Ok(Value::str(rat_to_base(*n, *d, radix, BaseDigits::Auto))))
            }
            // Handle Instance types (Duration, Instant, etc.) by
            // extracting their numeric value
            Value::Instance { attributes, .. } => {
                let radix = match parse_radix_checked(arg)? {
                    Ok(r) => r,
                    Err(_) => return Some(Ok(out_of_range_failure("base requires radix 2..36"))),
                };
                if let Some(val) = attributes.get("value") {
                    match val {
                        Value::Int(i) => {
                            Some(Ok(Value::str(rat_to_base(*i, 1, radix, BaseDigits::Auto))))
                        }
                        Value::Rat(n, d) | Value::FatRat(n, d) => {
                            Some(Ok(Value::str(rat_to_base(*n, *d, radix, BaseDigits::Auto))))
                        }
                        Value::Num(f) => {
                            let (n, d) = f64_to_rat(*f);
                            Some(Ok(Value::str(rat_to_base(n, d, radix, BaseDigits::Auto))))
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            }
            _ => None,
        },
        "base-repeating" => {
            let radix = match parse_radix_checked(arg)? {
                Ok(r) => r,
                Err(e) => return Some(Err(e)),
            };
            let (n, d) = match target {
                Value::Int(i) => (*i, 1i64),
                Value::Rat(n, d) => (*n, *d),
                Value::Num(f) => f64_to_rat(*f),
                _ => return None,
            };
            let (non_repeating, repeating) = rat_base_repeating(n, d, radix);
            Some(Ok(Value::Array(
                Arc::new(vec![Value::str(non_repeating), Value::str(repeating)]),
                ArrayKind::List,
            )))
        }
        "round" => {
            // Unwrap allomorphic types (IntStr, NumStr, RatStr, ComplexStr)
            // to get the underlying numeric value for the scale
            let unwrapped_arg = match arg {
                Value::Mixin(inner, _) => inner.as_ref(),
                other => other,
            };
            // Determine the scale type category for return type selection
            // Int/IntStr -> Int, Num/NumStr/Complex/ComplexStr -> Num,
            // Rat/RatStr -> Rat
            #[derive(Clone, Copy)]
            enum RoundResult {
                Int,
                Num,
                Rat,
            }
            let scale_type = match unwrapped_arg {
                Value::Int(_) | Value::BigInt(_) => RoundResult::Int,
                Value::Num(_) => RoundResult::Num,
                Value::Rat(_, _) | Value::FatRat(_, _) | Value::BigRat(_, _) => RoundResult::Rat,
                Value::Complex(_, _) => RoundResult::Num,
                _ => return None,
            };
            let scale = match unwrapped_arg {
                Value::Int(i) => *i as f64,
                Value::Num(f) => *f,
                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                Value::FatRat(n, d) if *d != 0 => *n as f64 / *d as f64,
                Value::Complex(re, _) => *re,
                _ => return None,
            };
            fn raku_round(v: f64) -> f64 {
                (v + 0.5).floor()
            }
            fn round_real(x: f64, scale: f64) -> f64 {
                if scale == 0.0 {
                    raku_round(x)
                } else {
                    raku_round(x / scale) * scale
                }
            }
            // Unwrap target allomorphic types too
            let unwrapped_target = match target {
                Value::Mixin(inner, _) => inner.as_ref(),
                other => other,
            };
            // Handle Complex target separately — always returns Complex
            if let Value::Complex(re, im) = unwrapped_target {
                let rr = round_real(*re, scale);
                let ri = round_real(*im, scale);
                return Some(Ok(Value::Complex(rr, ri)));
            }
            let x = match unwrapped_target {
                Value::Int(i) => *i as f64,
                Value::BigInt(bi) => bi.to_f64().unwrap_or(0.0),
                Value::Num(f) => *f,
                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                Value::FatRat(n, d) if *d != 0 => *n as f64 / *d as f64,
                _ => return None,
            };
            let result = round_real(x, scale);
            // Return type depends on the scale type
            match scale_type {
                RoundResult::Int => {
                    let r = result.floor();
                    if r >= i64::MIN as f64 && r <= i64::MAX as f64 {
                        Some(Ok(Value::Int(r as i64)))
                    } else {
                        Some(Ok(Value::Num(r)))
                    }
                }
                RoundResult::Num => Some(Ok(Value::Num(result))),
                RoundResult::Rat => {
                    let (n, d) = f64_to_rat(result);
                    Some(Ok(Value::Rat(n, d)))
                }
            }
        }
        "pick" => {
            if matches!(target, Value::Mix(_, _)) {
                return Some(Err(RuntimeError::new(
                    "Cannot call .pick on a Mix (immutable)",
                )));
            }
            // Callable args (e.g. WhateverCode `* / 2`) need interpreter to invoke;
            // fall through to the runtime.
            if matches!(arg, Value::Sub(_) | Value::WeakSub(_)) {
                return None;
            }
            // For Bag, use weighted picking without expanding to a flat list
            if let Value::Bag(bag, _) = target {
                let total_items: i128 = bag.values().map(|c| *c as i128).sum();
                let count: i128 = match arg {
                    Value::Whatever => total_items,
                    Value::Num(f) if f.is_infinite() && f.is_sign_positive() => total_items,
                    Value::Int(n) => (*n).max(0) as i128,
                    Value::Num(f) => (*f as i64).max(0) as i128,
                    Value::Rat(n, d) if *d != 0 => (*n / *d).max(0) as i128,
                    _ => 0i128,
                };
                if count == 0 || bag.is_empty() {
                    return Some(Ok(Value::array(Vec::new())));
                }
                // Build a mutable copy of counts for without-replacement picking
                let mut counts: Vec<(String, i128)> = bag
                    .iter()
                    .filter(|(_, c)| **c > 0)
                    .map(|(k, c)| (k.clone(), *c as i128))
                    .collect();
                let mut total: i128 = counts.iter().map(|(_, c)| *c).sum();
                let pick_count = (count as usize).min(total as usize);
                let mut result = Vec::with_capacity(pick_count);
                for _ in 0..pick_count {
                    if total <= 0 {
                        break;
                    }
                    let needle_f = crate::builtins::rng::builtin_rand() * total as f64;
                    let mut needle = needle_f as i128;
                    if needle >= total {
                        needle = total - 1;
                    }
                    let mut picked_idx = counts.len() - 1;
                    let mut cum: i128 = 0;
                    for (i, (_, c)) in counts.iter().enumerate() {
                        cum += *c;
                        if needle < cum {
                            picked_idx = i;
                            break;
                        }
                    }
                    result.push(Value::str(counts[picked_idx].0.clone()));
                    counts[picked_idx].1 -= 1;
                    total -= 1;
                    if counts[picked_idx].1 == 0 {
                        counts.swap_remove(picked_idx);
                    }
                }
                return Some(Ok(Value::array(result)));
            }
            // For Set, pick returns keys (strings) without replacement
            if let Value::Set(set, _) = target {
                if let Value::Num(f) = arg
                    && f.is_nan()
                {
                    return Some(Err(RuntimeError::new("Cannot convert NaN to Int")));
                }
                let mut keys: Vec<String> = set.iter().cloned().collect();
                let count: usize = match arg {
                    Value::Whatever => keys.len(),
                    Value::Num(f) if f.is_infinite() && f.is_sign_positive() => keys.len(),
                    Value::Int(n) => (*n).max(0) as usize,
                    Value::Num(f) => (*f as i64).max(0) as usize,
                    Value::Rat(n, d) if *d != 0 => (*n / *d).max(0) as usize,
                    _ => 0,
                };
                let pick_count = count.min(keys.len());
                // Fisher-Yates shuffle for without-replacement
                let len = keys.len();
                for i in (1..len).rev() {
                    let j =
                        (crate::builtins::rng::builtin_rand() * (i + 1) as f64) as usize % (i + 1);
                    keys.swap(i, j);
                }
                keys.truncate(pick_count);
                return Some(Ok(Value::Seq(Arc::new(
                    keys.into_iter().map(Value::str).collect(),
                ))));
            }
            // Fast path for integer ranges — avoid materializing
            if let Some(result) = range_pick_n_fast(target, arg) {
                return Some(Ok(result));
            }
            // .pick(**) — lazy infinite shuffled cycles
            if matches!(arg, Value::HyperWhatever) {
                let pool = runtime::value_to_list(target);
                if pool.is_empty() {
                    return Some(Ok(Value::array(Vec::new())));
                }
                // Pre-generate several cycles of shuffled picks
                let num_cycles = 4;
                let mut cached = Vec::with_capacity(pool.len() * num_cycles);
                for _ in 0..num_cycles {
                    let mut cycle = pool.clone();
                    let len = cycle.len();
                    for i in (1..len).rev() {
                        let j = (crate::builtins::rng::builtin_rand() * (i + 1) as f64) as usize
                            % (i + 1);
                        cycle.swap(i, j);
                    }
                    cached.extend(cycle);
                }
                return Some(Ok(Value::LazyList(Arc::new(
                    crate::value::LazyList::new_cached(cached),
                ))));
            }
            let mut items = runtime::value_to_list(target);
            Some(Ok(match arg {
                Value::Whatever => {
                    // .pick(*) — Fisher-Yates shuffle
                    let len = items.len();
                    for i in (1..len).rev() {
                        let j = (crate::builtins::rng::builtin_rand() * (i + 1) as f64) as usize
                            % (i + 1);
                        items.swap(i, j);
                    }
                    Value::array(items)
                }
                Value::Num(f) if f.is_infinite() && f.is_sign_positive() => {
                    // .pick(Inf) — same as .pick(*)
                    let len = items.len();
                    for i in (1..len).rev() {
                        let j = (crate::builtins::rng::builtin_rand() * (i + 1) as f64) as usize
                            % (i + 1);
                        items.swap(i, j);
                    }
                    Value::array(items)
                }
                Value::Num(f) => {
                    // .pick(<num>) — truncate to int
                    let count = (*f as i64).max(0) as usize;
                    if count == 0 || items.is_empty() {
                        Value::array(Vec::new())
                    } else {
                        let mut result = Vec::with_capacity(count.min(items.len()));
                        for _ in 0..count.min(items.len()) {
                            let idx = (crate::builtins::rng::builtin_rand() * items.len() as f64)
                                as usize
                                % items.len();
                            result.push(items.swap_remove(idx));
                        }
                        Value::array(result)
                    }
                }
                Value::Rat(n, d) if *d != 0 => {
                    // .pick(<rat>) — truncate to int
                    let count = (*n / *d).max(0) as usize;
                    if count == 0 || items.is_empty() {
                        Value::array(Vec::new())
                    } else {
                        let mut result = Vec::with_capacity(count.min(items.len()));
                        for _ in 0..count.min(items.len()) {
                            let idx = (crate::builtins::rng::builtin_rand() * items.len() as f64)
                                as usize
                                % items.len();
                            result.push(items.swap_remove(idx));
                        }
                        Value::array(result)
                    }
                }
                Value::Int(n) => {
                    let count = (*n).max(0) as usize;
                    if count == 0 || items.is_empty() {
                        Value::array(Vec::new())
                    } else {
                        let mut result = Vec::with_capacity(count.min(items.len()));
                        for _ in 0..count.min(items.len()) {
                            let idx = (crate::builtins::rng::builtin_rand() * items.len() as f64)
                                as usize
                                % items.len();
                            result.push(items.swap_remove(idx));
                        }
                        Value::array(result)
                    }
                }
                Value::Str(s) => {
                    let count = s.trim().parse::<i64>().unwrap_or(0).max(0) as usize;
                    if count == 0 || items.is_empty() {
                        Value::array(Vec::new())
                    } else {
                        let mut result = Vec::with_capacity(count.min(items.len()));
                        for _ in 0..count.min(items.len()) {
                            let idx = (crate::builtins::rng::builtin_rand() * items.len() as f64)
                                as usize
                                % items.len();
                            result.push(items.swap_remove(idx));
                        }
                        Value::array(result)
                    }
                }
                _ => return None,
            }))
        }
        "pickpairs" => {
            if let Value::Bag(bag, _) = target {
                let count = match arg {
                    Value::Whatever => bag.len(),
                    Value::Int(n) => (*n).max(0) as usize,
                    Value::Num(f) if f.is_infinite() && f.is_sign_positive() => bag.len(),
                    Value::Num(f) => (*f as i64).max(0) as usize,
                    Value::Rat(n, d) if *d != 0 => (*n / *d).max(0) as usize,
                    _ => return None,
                };
                if count == 0 || bag.is_empty() {
                    return Some(Ok(Value::array(Vec::new())));
                }
                let mut pairs: Vec<(String, i64)> =
                    bag.iter().map(|(k, v)| (k.clone(), *v)).collect();
                let pick_count = count.min(pairs.len());
                let mut result = Vec::with_capacity(pick_count);
                for _ in 0..pick_count {
                    if pairs.is_empty() {
                        break;
                    }
                    let idx = (crate::builtins::rng::builtin_rand() * pairs.len() as f64) as usize
                        % pairs.len();
                    let (key, count) = pairs.swap_remove(idx);
                    result.push(Value::Pair(key, Box::new(Value::Int(count))));
                }
                return Some(Ok(Value::array(result)));
            }
            None
        }
        "grab" | "grabpairs" => match target {
            Value::Bag(_, false) => Some(Err(RuntimeError::immutable("Bag", method))),
            Value::Set(_, false) => Some(Err(RuntimeError::immutable("Set", method))),
            Value::Set(_, true) => Some(Err(RuntimeError::immutable("SetHash", method))),
            Value::Mix(_, false) => Some(Err(RuntimeError::immutable("Mix", method))),
            _ => None,
        },
        "roll" => {
            if matches!(target, Value::Package(_)) {
                return None;
            }
            let count = match arg {
                Value::Int(i) if *i > 0 => Some(*i as usize),
                Value::Int(_) => Some(0),
                Value::Num(f) if f.is_nan() => {
                    return Some(Err(RuntimeError::new("Cannot convert NaN to Int")));
                }
                Value::Num(f) if f.is_infinite() && f.is_sign_positive() => None,
                Value::Num(f) if *f < 0.0 => Some(0),
                Value::Num(f) => Some(*f as usize),
                Value::Whatever => None,
                Value::Str(s) => {
                    let parsed = s.trim().parse::<i64>().ok()?;
                    Some(parsed.max(0) as usize)
                }
                _ => return None,
            };
            if let Value::Mix(items, _) = target {
                if count.is_none() {
                    let generated = 131_072usize;
                    let mut out = Vec::with_capacity(generated);
                    for _ in 0..generated {
                        if let Some(v) = sample_weighted_mix_key(items) {
                            out.push(v);
                        }
                    }
                    return Some(Ok(Value::LazyList(Arc::new(
                        crate::value::LazyList::new_cached(out),
                    ))));
                }
                let count = count.unwrap_or(0);
                if count == 0 {
                    return Some(Ok(Value::array(Vec::new())));
                }
                let mut result = Vec::with_capacity(count);
                for _ in 0..count {
                    if let Some(v) = sample_weighted_mix_key(items) {
                        result.push(v);
                    }
                }
                return Some(Ok(Value::array(result)));
            }
            if let Value::Bag(items, _) = target {
                if count.is_none() {
                    let generated = 131_072usize;
                    let mut out = Vec::with_capacity(generated);
                    for _ in 0..generated {
                        if let Some(v) = sample_weighted_bag_key(items) {
                            out.push(v);
                        }
                    }
                    return Some(Ok(Value::LazyList(Arc::new(
                        crate::value::LazyList::new_cached(out),
                    ))));
                }
                let count = count.unwrap_or(0);
                if count == 0 {
                    return Some(Ok(Value::array(Vec::new())));
                }
                let mut result = Vec::with_capacity(count);
                for _ in 0..count {
                    if let Some(v) = sample_weighted_bag_key(items) {
                        result.push(v);
                    }
                }
                return Some(Ok(Value::array(result)));
            }
            if let Value::Set(items, _) = target {
                let keys: Vec<&String> = items.iter().collect();
                if keys.is_empty() {
                    return Some(Ok(Value::Seq(Arc::new(Vec::new()))));
                }
                if count.is_none() {
                    let generated = 131_072usize;
                    let mut out = Vec::with_capacity(generated);
                    for _ in 0..generated {
                        let mut idx =
                            (crate::builtins::rng::builtin_rand() * keys.len() as f64) as usize;
                        if idx >= keys.len() {
                            idx = keys.len() - 1;
                        }
                        out.push(Value::str(keys[idx].clone()));
                    }
                    return Some(Ok(Value::LazyList(Arc::new(
                        crate::value::LazyList::new_cached(out),
                    ))));
                }
                let count = count.unwrap_or(0);
                if count == 0 {
                    return Some(Ok(Value::Seq(Arc::new(Vec::new()))));
                }
                let mut result = Vec::with_capacity(count);
                for _ in 0..count {
                    let mut idx =
                        (crate::builtins::rng::builtin_rand() * keys.len() as f64) as usize;
                    if idx >= keys.len() {
                        idx = keys.len() - 1;
                    }
                    result.push(Value::str(keys[idx].clone()));
                }
                return Some(Ok(Value::array(result)));
            }
            let sample_from_range = |range: &Value| -> Option<Value> {
                let random_i64 = |lo: i64, hi: i64| -> Value {
                    use crate::builtins::methods_0arg::dispatch_core_range::range_pick_one_i64_pub;
                    if hi <= lo {
                        return Value::Int(lo);
                    }
                    range_pick_one_i64_pub(lo, hi)
                };
                match range {
                    Value::Range(start, end) => Some(random_i64(*start, *end)),
                    Value::RangeExcl(start, end) => {
                        if *start >= *end {
                            Some(Value::Nil)
                        } else {
                            Some(random_i64(*start, end.saturating_sub(1)))
                        }
                    }
                    Value::RangeExclStart(start, end) => {
                        if *start >= *end {
                            Some(Value::Nil)
                        } else {
                            Some(random_i64(start.saturating_add(1), *end))
                        }
                    }
                    Value::RangeExclBoth(start, end) => {
                        if start.saturating_add(1) >= *end {
                            Some(Value::Nil)
                        } else {
                            Some(random_i64(start.saturating_add(1), end.saturating_sub(1)))
                        }
                    }
                    Value::GenericRange {
                        start,
                        end,
                        excl_start,
                        excl_end,
                    } => {
                        // Try BigInt-based fast path for integer ranges
                        if let Some(result) = crate::builtins::methods_0arg::dispatch_core_range::generic_range_pick_one_pub(start, end, *excl_start, *excl_end) {
                            return Some(result);
                        }
                        if let (Some(s), Some(e)) =
                            (runtime::to_float_value(start), runtime::to_float_value(end))
                            && s.is_finite()
                            && e.is_finite()
                        {
                            let mut vals = Vec::new();
                            let mut cur = if *excl_start { s + 1.0 } else { s };
                            let limit = 10_000usize;
                            while vals.len() < limit {
                                if (*excl_end && cur >= e) || (!*excl_end && cur > e) {
                                    break;
                                }
                                vals.push(Value::Num(cur));
                                cur += 1.0;
                            }
                            if !vals.is_empty() {
                                let idx = (crate::builtins::rng::builtin_rand() * vals.len() as f64)
                                    as usize
                                    % vals.len();
                                return Some(vals[idx].clone());
                            }
                        }
                        Some(start.as_ref().clone())
                    }
                    _ => None,
                }
            };

            let items = if target.is_range() {
                Vec::new()
            } else {
                runtime::value_to_list(target)
            };
            if count.is_none() {
                if !target.is_range() && items.is_empty() {
                    return Some(Ok(Value::array(Vec::new())));
                }
                let generated = 1024usize;
                let mut out = Vec::with_capacity(generated);
                for _ in 0..generated {
                    if target.is_range() {
                        if let Some(v) = sample_from_range(target) {
                            out.push(v);
                        }
                    } else {
                        let mut idx =
                            (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize;
                        if idx >= items.len() {
                            idx = items.len() - 1;
                        }
                        out.push(items[idx].clone());
                    }
                }
                return Some(Ok(Value::LazyList(Arc::new(
                    crate::value::LazyList::new_cached(out),
                ))));
            }
            let count = count.unwrap_or(0);
            if count == 0 || (!target.is_range() && items.is_empty()) {
                return Some(Ok(Value::array(Vec::new())));
            }
            let mut result = Vec::with_capacity(count);
            for _ in 0..count {
                if target.is_range() {
                    if let Some(v) = sample_from_range(target) {
                        result.push(v);
                    }
                } else {
                    let mut idx =
                        (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize;
                    if idx >= items.len() {
                        idx = items.len() - 1;
                    }
                    result.push(items[idx].clone());
                }
            }
            Some(Ok(Value::array(result)))
        }
        "log" => {
            let base_complex = match arg {
                Value::Int(i) => Some((*i as f64, 0.0)),
                Value::Num(f) => Some((*f, 0.0)),
                Value::Rat(n, d) if *d != 0 => Some((*n as f64 / *d as f64, 0.0)),
                Value::Complex(r, i) => Some((*r, *i)),
                _ => None,
            };
            let target_complex = match target {
                Value::Int(i) => Some((*i as f64, 0.0)),
                Value::Num(f) => Some((*f, 0.0)),
                Value::Rat(n, d) if *d != 0 => Some((*n as f64 / *d as f64, 0.0)),
                Value::Complex(r, i) => Some((*r, *i)),
                _ => None,
            };
            match (target_complex, base_complex) {
                (Some((xr, xi)), Some((br, bi))) => {
                    if bi == 0.0 && xi == 0.0 {
                        if br.is_finite() && br > 0.0 && br != 1.0 && xr > 0.0 {
                            return Some(Ok(Value::Num(xr.ln() / br.ln())));
                        }
                        return Some(Ok(Value::Num(f64::NAN)));
                    }
                    let ln_x_mag = (xr * xr + xi * xi).sqrt().ln();
                    let ln_x_arg = xi.atan2(xr);
                    let ln_b_mag = (br * br + bi * bi).sqrt().ln();
                    let ln_b_arg = bi.atan2(br);
                    let denom = ln_b_mag * ln_b_mag + ln_b_arg * ln_b_arg;
                    if denom == 0.0 {
                        return Some(Ok(Value::Num(f64::NAN)));
                    }
                    let re = (ln_x_mag * ln_b_mag + ln_x_arg * ln_b_arg) / denom;
                    let im = (ln_x_arg * ln_b_mag - ln_x_mag * ln_b_arg) / denom;
                    Some(Ok(Value::Complex(re, im)))
                }
                _ => None,
            }
        }
        "exp" => {
            // $x.exp($base) = $base ** $x
            // Get base as real or complex
            let (base_r, base_i) = match arg {
                Value::Int(i) => (*i as f64, 0.0),
                Value::Num(f) => (*f, 0.0),
                Value::Rat(n, d) if *d != 0 => (*n as f64 / *d as f64, 0.0),
                Value::Complex(r, i) => (*r, *i),
                _ => return None,
            };
            // Get exponent as real or complex
            let (exp_r, exp_i) = match target {
                Value::Int(i) => (*i as f64, 0.0),
                Value::Num(f) => (*f, 0.0),
                Value::Rat(n, d) if *d != 0 => (*n as f64 / *d as f64, 0.0),
                Value::Complex(r, i) => (*r, *i),
                _ => return None,
            };
            // Compute base^exp via exp(exp * ln(base))
            // ln(base) for complex: ln(|base|) + i*arg(base)
            let ln_r = (base_r * base_r + base_i * base_i).sqrt().ln();
            let ln_i = base_i.atan2(base_r);
            // exp * ln(base): (exp_r + exp_i*i) * (ln_r + ln_i*i)
            let prod_r = exp_r * ln_r - exp_i * ln_i;
            let prod_i = exp_r * ln_i + exp_i * ln_r;
            // exp(prod_r + prod_i*i)
            let ea = prod_r.exp();
            let result_r = ea * prod_i.cos();
            let result_i = ea * prod_i.sin();
            if result_i.abs() < 1e-15 && base_i == 0.0 && exp_i == 0.0 {
                Some(Ok(Value::Num(result_r)))
            } else {
                Some(Ok(Value::Complex(result_r, result_i)))
            }
        }
        "unpolar" => {
            // $magnitude.unpolar($angle) = $magnitude * cis($angle)
            let mag = match target {
                Value::Int(i) => *i as f64,
                Value::Num(f) => *f,
                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                _ => return None,
            };
            let angle = match arg {
                Value::Int(i) => *i as f64,
                Value::Num(f) => *f,
                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                _ => return None,
            };
            Some(Ok(Value::Complex(mag * angle.cos(), mag * angle.sin())))
        }
        "roots" => {
            // Instance types need runtime coercion via .Numeric
            if matches!(target, Value::Instance { .. }) {
                return None;
            }
            Some(Ok(compute_roots(target, arg)))
        }
        "atan2" => {
            // User-defined types need runtime coercion via .Numeric/.Bridge
            if matches!(target, Value::Instance { .. }) || matches!(arg, Value::Instance { .. }) {
                return None;
            }
            let y = runtime::to_float_value(target).unwrap_or(0.0);
            let x = runtime::to_float_value(arg).unwrap_or(0.0);
            Some(Ok(Value::Num(y.atan2(x))))
        }
        // Buf/Blob read-num methods (1 arg: offset, uses NativeEndian)
        "read-num32" | "read-num64" => {
            if let Value::Package(type_name) = target {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller {}({}:U)",
                    method, type_name,
                ))));
            }
            let bytes = buf_get_bytes(target)?;
            let offset_i64 = to_int_val(arg);
            let size: usize = if method == "read-num32" { 4 } else { 8 };
            if offset_i64 < 0
                || (offset_i64 as usize)
                    .checked_add(size)
                    .is_none_or(|end| end > bytes.len())
            {
                return Some(Err(RuntimeError::new(format!(
                    "read from out of range. Is: {}, should be in 0..{}",
                    offset_i64,
                    bytes.len()
                ))));
            }
            let offset = offset_i64 as usize;
            let result = if size == 4 {
                read_f32_ne(&bytes[offset..offset + 4])
            } else {
                read_f64_ne(&bytes[offset..offset + 8])
            };
            Some(Ok(Value::Num(result)))
        }
        // Buf/Blob read-int/uint methods (1 arg: offset, uses NativeEndian)
        "read-uint8" | "read-int8" | "read-uint16" | "read-int16" | "read-uint32"
        | "read-int32" | "read-uint64" | "read-int64" | "read-uint128" | "read-int128" => {
            if let Value::Package(type_name) = target {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller {}({}:U)",
                    method, type_name,
                ))));
            }
            let bytes = buf_get_bytes(target)?;
            let offset_i64 = to_int_val(arg);
            let (size, signed) = read_int_method_info(method);
            if offset_i64 < 0
                || (offset_i64 as usize)
                    .checked_add(size)
                    .is_none_or(|end| end > bytes.len())
            {
                return Some(Err(RuntimeError::new(format!(
                    "read from out of range. Is: {}, should be in 0..{}",
                    offset_i64,
                    bytes.len()
                ))));
            }
            let offset = offset_i64 as usize;
            // NativeEndian default
            let endian: i64 = if cfg!(target_endian = "little") { 1 } else { 2 };
            Some(Ok(read_int_value(
                &bytes[offset..offset + size],
                size,
                signed,
                endian,
            )))
        }
        "EXISTS-KEY" => match target {
            Value::Hash(map) => {
                let key = arg.to_string_value();
                Some(Ok(Value::Bool(map.contains_key(&key))))
            }
            _ => None,
        },
        "subbuf" => {
            if !is_buf_like(target) {
                return None;
            }
            let items = buf_get_int_items(target)?;
            let cn = buf_class_name(target);
            // subbuf(Range)
            if let Some((start, end)) = range_bounds(arg) {
                let len = items.len() as i64;
                if end <= start || start >= len {
                    return Some(Ok(make_buf_from_int_items(&cn, &[])));
                }
                let s = start.max(0) as usize;
                let e = (end as usize).min(items.len());
                return Some(Ok(make_buf_from_int_items(&cn, &items[s..e])));
            }
            // subbuf(start) - from start to end
            let start = resolve_buf_index(arg, items.len());
            if start < 0 || start as usize > items.len() {
                return Some(Err(out_of_range_error(start, 0, items.len() as i64)));
            }
            Some(Ok(make_buf_from_int_items(&cn, &items[start as usize..])))
        }
        "isa" => {
            // Instance, Mixin(Instance), and Package values need interpreter
            // access for user-defined class hierarchies, role checks, and
            // subset type resolution, so fall through to the runtime handler.
            let needs_interpreter = match target {
                Value::Instance { .. } | Value::Package(_) => true,
                Value::Mixin(inner, _) => matches!(inner.as_ref(), Value::Instance { .. }),
                _ => false,
            };
            if needs_interpreter {
                return None;
            }
            let type_name = match arg {
                Value::Package(name) => name.resolve(),
                Value::Str(name) => name.to_string(),
                Value::Instance { class_name, .. } => class_name.resolve(),
                other => {
                    // For defined values, extract the type name (e.g., 3.isa(4) checks Int)
                    // This matches Raku's behavior where .isa on a defined value
                    // uses the value's type, not its string representation.
                    crate::value::types::what_type_name(other)
                }
            };
            Some(Ok(Value::Bool(target.isa_check(&type_name))))
        }
        _ => None,
    }
}

fn is_buf_like(val: &Value) -> bool {
    if let Value::Instance { class_name, .. } = val {
        let cn = class_name.resolve();
        cn == "Buf"
            || cn == "Blob"
            || cn == "utf8"
            || cn == "utf16"
            || cn.starts_with("Buf[")
            || cn.starts_with("Blob[")
            || cn.starts_with("buf")
            || cn.starts_with("blob")
    } else {
        false
    }
}

fn buf_class_name(val: &Value) -> String {
    if let Value::Instance { class_name, .. } = val {
        class_name.resolve().to_string()
    } else {
        "Buf".to_string()
    }
}

fn buf_get_int_items(target: &Value) -> Option<Vec<Value>> {
    if let Value::Instance { attributes, .. } = target
        && let Some(Value::Array(items, ..)) = attributes.get("bytes")
    {
        Some(items.to_vec())
    } else {
        None
    }
}

fn make_buf_from_int_items(class_name: &str, items: &[Value]) -> Value {
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("bytes".to_string(), Value::array(items.to_vec()));
    Value::make_instance(Symbol::intern(class_name), attrs)
}

fn eval_whatever_code(sub_data: &std::sync::Arc<crate::value::SubData>, arg: i64) -> i64 {
    let param = sub_data
        .params
        .first()
        .map(|s: &String| s.as_str())
        .unwrap_or("_");
    let mut sub_env = sub_data.env.clone();
    sub_env.insert(param.to_string(), Value::Int(arg));
    let mut interpreter = crate::runtime::Interpreter::new();
    *interpreter.env_mut() = sub_env;
    if let Ok(result) = interpreter.eval_block_value(&sub_data.body) {
        match result {
            Value::Int(n) => n,
            Value::Num(f) => f as i64,
            _ => 0,
        }
    } else {
        0
    }
}

fn resolve_buf_index(arg: &Value, len: usize) -> i64 {
    match arg {
        Value::Int(n) => *n,
        Value::Num(f) => *f as i64,
        Value::Rat(n, d) => {
            if *d != 0 {
                *n / *d
            } else {
                0
            }
        }
        Value::Sub(data) => eval_whatever_code(data, len as i64),
        Value::Whatever => len as i64,
        _ => 0,
    }
}

fn resolve_buf_len(arg: &Value, total_len: usize, start: usize) -> i64 {
    match arg {
        Value::Int(n) => *n,
        Value::Num(f) => {
            if f.is_infinite() && *f > 0.0 {
                (total_len - start) as i64
            } else {
                *f as i64
            }
        }
        Value::Rat(n, d) => {
            if *d != 0 {
                *n / *d
            } else {
                0
            }
        }
        Value::Whatever => (total_len - start) as i64,
        Value::Sub(data) => {
            // WhateverCode receives total_len and returns an end index (inclusive).
            // Length = max(0, end_index - start + 1)
            let end_idx = eval_whatever_code(data, total_len as i64);
            let len = end_idx - start as i64 + 1;
            if len < 0 { 0 } else { len }
        }
        _ => 0,
    }
}

fn range_bounds(arg: &Value) -> Option<(i64, i64)> {
    match arg {
        Value::Range(start, end) => Some((*start, *end + 1)),
        Value::RangeExcl(start, end) => Some((*start, *end)),
        _ => None,
    }
}

fn out_of_range_error(got: i64, min: i64, max: i64) -> RuntimeError {
    let mut attrs = std::collections::HashMap::new();
    let msg = format!(
        "Index out of range. Is: {}, should be in {}..{}",
        got, min, max
    );
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    attrs.insert("got".to_string(), Value::Int(got));
    attrs.insert("range".to_string(), Value::str(format!("{}..{}", min, max)));
    let ex = Value::make_instance(Symbol::intern("X::OutOfRange"), attrs);
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

/// Extract byte array from a Buf/Blob instance.
fn buf_get_bytes(target: &Value) -> Option<Vec<u8>> {
    if let Value::Instance {
        class_name,
        attributes,
        ..
    } = target
        && crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve())
        && let Some(Value::Array(items, ..)) = attributes.get("bytes")
    {
        return Some(
            items
                .iter()
                .map(|v| match v {
                    Value::Int(i) => *i as u8,
                    _ => 0,
                })
                .collect(),
        );
    }
    None
}

fn to_int_val(v: &Value) -> i64 {
    match v {
        Value::Int(i) => *i,
        Value::Num(f) => *f as i64,
        _ => 0,
    }
}

fn read_ubits_from_bytes(bytes: &[u8], from: usize, bits: usize) -> BigInt {
    let mut acc = BigInt::ZERO;
    for i in 0..bits {
        let bit_index = from + i;
        let byte = bytes[bit_index / 8];
        let bit = (byte >> (7 - (bit_index % 8))) & 1;
        acc = (acc << 1) + BigInt::from(bit);
    }
    acc
}

fn bigint_to_value(value: BigInt) -> Value {
    if let Some(i) = value.to_i64() {
        Value::Int(i)
    } else {
        Value::bigint(value)
    }
}

fn read_f32_ne(bytes: &[u8]) -> f64 {
    let arr: [u8; 4] = [bytes[0], bytes[1], bytes[2], bytes[3]];
    f32::from_ne_bytes(arr) as f64
}

fn read_f64_ne(bytes: &[u8]) -> f64 {
    let arr: [u8; 8] = [
        bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
    ];
    f64::from_ne_bytes(arr)
}

fn read_f32_endian(bytes: &[u8], endian_val: i64) -> f64 {
    let arr: [u8; 4] = [bytes[0], bytes[1], bytes[2], bytes[3]];
    match endian_val {
        1 => f32::from_le_bytes(arr) as f64, // LittleEndian
        2 => f32::from_be_bytes(arr) as f64, // BigEndian
        _ => f32::from_ne_bytes(arr) as f64, // NativeEndian
    }
}

fn read_f64_endian(bytes: &[u8], endian_val: i64) -> f64 {
    let arr: [u8; 8] = [
        bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
    ];
    match endian_val {
        1 => f64::from_le_bytes(arr), // LittleEndian
        2 => f64::from_be_bytes(arr), // BigEndian
        _ => f64::from_ne_bytes(arr), // NativeEndian
    }
}

/// Returns (byte_size, is_signed) for a read-int/uint method name.
fn read_int_method_info(method: &str) -> (usize, bool) {
    match method {
        "read-uint8" => (1, false),
        "read-int8" => (1, true),
        "read-uint16" => (2, false),
        "read-int16" => (2, true),
        "read-uint32" => (4, false),
        "read-int32" => (4, true),
        "read-uint64" => (8, false),
        "read-int64" => (8, true),
        "read-uint128" => (16, false),
        "read-int128" => (16, true),
        _ => unreachable!(),
    }
}

/// Read an integer value from a byte slice with the given endianness.
/// endian_val: 0=NativeEndian, 1=LittleEndian, 2=BigEndian
fn read_int_value(bytes: &[u8], size: usize, signed: bool, endian_val: i64) -> Value {
    // Determine effective endianness: NativeEndian resolves to LE or BE
    let is_le = match endian_val {
        1 => true,                           // LittleEndian
        2 => false,                          // BigEndian
        _ => cfg!(target_endian = "little"), // NativeEndian
    };

    // Build unsigned value from bytes
    let unsigned = if is_le {
        // Little-endian: first byte is least significant
        let mut acc = BigInt::ZERO;
        for &b in bytes[..size].iter().rev() {
            acc = (acc << 8) | BigInt::from(b);
        }
        acc
    } else {
        // Big-endian: first byte is most significant
        let mut acc = BigInt::ZERO;
        for &b in &bytes[..size] {
            acc = (acc << 8) | BigInt::from(b);
        }
        acc
    };

    if signed {
        let bits = size * 8;
        let sign_bit = BigInt::from(1u8) << (bits - 1);
        let result = if unsigned >= sign_bit {
            unsigned - (BigInt::from(1u8) << bits)
        } else {
            unsigned
        };
        bigint_to_value(result)
    } else {
        bigint_to_value(unsigned)
    }
}

// ── 2-arg method dispatch ────────────────────────────────────────────
/// Try to dispatch a 2-argument method call on a Value.
pub(crate) fn native_method_2arg(
    target: &Value,
    method_sym: Symbol,
    arg1: &Value,
    arg2: &Value,
) -> Option<Result<Value, RuntimeError>> {
    let method = method_sym.resolve();
    let method = method.as_str();

    if let Value::Scalar(inner) = target {
        return native_method_2arg(inner, method_sym, arg1, arg2);
    }
    if method == "flat" {
        let (depth, hammer) = if let Some(depth) = parse_flat_depth(arg1) {
            (Some(depth), is_hammer_pair(arg2))
        } else if let Some(depth) = parse_flat_depth(arg2) {
            (Some(depth), is_hammer_pair(arg1))
        } else {
            (None, false)
        };
        if let Some(depth) = depth {
            if hammer {
                return Some(Ok(flatten_target(target, Some(depth), true)));
            }
            return Some(Ok(flatten_target(target, Some(depth), false)));
        }
        return None;
    }

    if method == "split" {
        if let Value::Instance { class_name, .. } = target
            && class_name == "Supply"
        {
            return None;
        }
        return super::split::native_split_method(target, &[arg1.clone(), arg2.clone()]);
    }

    match method {
        "expmod" => Some(crate::builtins::expmod(target, arg1, arg2)),
        "unimatch" => {
            // target.unimatch(prop_value, prop_name)
            let prop_value = arg1.to_string_value();
            let prop_name = arg2.to_string_value();
            match target {
                Value::Int(i) => {
                    let cp = *i as u32;
                    Some(Ok(crate::builtins::uniprop::unimatch_for_codepoint(
                        cp,
                        &prop_value,
                        Some(&prop_name),
                    )))
                }
                _ => {
                    let s = target.to_string_value();
                    if s.is_empty() {
                        return Some(Ok(Value::Nil));
                    }
                    let ch = s.chars().next().unwrap();
                    Some(Ok(Value::Bool(crate::builtins::uniprop::unimatch(
                        ch,
                        &prop_value,
                        Some(&prop_name),
                    ))))
                }
            }
        }
        "fmt" => {
            let fmt_str = arg1.to_string_value();
            let sep = arg2.to_string_value();
            if let Value::Hash(items) = target {
                // Hash.fmt(format, separator)
                let rendered = items
                    .iter()
                    .map(|(k, v)| {
                        runtime::format_sprintf_args(
                            &fmt_str,
                            &[Value::str(k.to_string()), v.clone()],
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(&sep);
                Some(Ok(Value::str(rendered)))
            } else if let Value::Bag(items, _) = target {
                let rendered = items
                    .iter()
                    .map(|(k, v)| {
                        runtime::format_sprintf_args(
                            &fmt_str,
                            &[Value::str(k.clone()), Value::Int(*v)],
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(&sep);
                Some(Ok(Value::str(rendered)))
            } else if let Value::Set(items, _) = target {
                let rendered = items
                    .iter()
                    .map(|k| runtime::format_sprintf_args(&fmt_str, &[Value::str(k.clone())]))
                    .collect::<Vec<_>>()
                    .join(&sep);
                Some(Ok(Value::str(rendered)))
            } else if let Value::Mix(items, _) = target {
                let rendered = items
                    .iter()
                    .map(|(k, v)| {
                        runtime::format_sprintf_args(
                            &fmt_str,
                            &[Value::str(k.clone()), Value::Num(*v)],
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(&sep);
                Some(Ok(Value::str(rendered)))
            } else if fmt_joinable_target(target) {
                let items: Vec<Value> = if let Some(inner) = target.as_list_items() {
                    inner.to_vec()
                } else {
                    runtime::value_to_list(target)
                };
                let rendered = items
                    .into_iter()
                    .map(|item| fmt_single_or_pair(&fmt_str, &item))
                    .collect::<Vec<_>>()
                    .join(&sep);
                Some(Ok(Value::str(rendered)))
            } else {
                Some(Err(RuntimeError::new(
                    "Too many positionals passed; expected 1 or 2 arguments but got 3",
                )))
            }
        }
        "substr" => {
            let start = match arg1 {
                Value::Int(i) if *i >= 0 => *i as usize,
                Value::Int(_) => return None, // negative: let runtime handle
                _ => return None,
            };
            let len = match arg2 {
                Value::Int(i) if *i >= 0 => *i as usize,
                Value::Int(_) => return None,
                _ => return None,
            };
            let s = target.to_string_value();
            let chars: Vec<char> = s.chars().collect();
            if start > chars.len() {
                return None; // out-of-range: let runtime handle (returns Failure)
            }
            let end = (start + len).min(chars.len());
            Some(Ok(Value::str(chars[start..end].iter().collect())))
        }
        "base" => {
            let radix = match arg1 {
                Value::Int(r) if (2..=36).contains(r) => *r as u32,
                Value::Str(s) => match s.parse::<u32>() {
                    Ok(r) if (2..=36).contains(&r) => r,
                    _ => {
                        return Some(Ok(out_of_range_failure("base requires radix 2..36")));
                    }
                },
                _ => {
                    return Some(Ok(out_of_range_failure("base requires radix 2..36")));
                }
            };
            let digits_mode = match arg2 {
                Value::Int(d) if *d < 0 => {
                    return Some(Ok(out_of_range_failure("digits must be non-negative")));
                }
                Value::Int(d) => BaseDigits::Fixed(*d as u32),
                Value::Whatever => BaseDigits::Whatever,
                _ => None?,
            };
            match target {
                Value::Int(i) => Some(Ok(Value::str(rat_to_base(*i, 1, radix, digits_mode)))),
                Value::Num(f) => {
                    let (n, d) = f64_to_rat(*f);
                    Some(Ok(Value::str(rat_to_base(n, d, radix, digits_mode))))
                }
                Value::Rat(n, d) | Value::FatRat(n, d) => {
                    Some(Ok(Value::str(rat_to_base(*n, *d, radix, digits_mode))))
                }
                Value::Instance { attributes, .. } => {
                    if let Some(val) = attributes.get("value") {
                        match val {
                            Value::Int(i) => {
                                Some(Ok(Value::str(rat_to_base(*i, 1, radix, digits_mode))))
                            }
                            Value::Rat(n, d) | Value::FatRat(n, d) => {
                                Some(Ok(Value::str(rat_to_base(*n, *d, radix, digits_mode))))
                            }
                            Value::Num(f) => {
                                let (n, d) = f64_to_rat(*f);
                                Some(Ok(Value::str(rat_to_base(n, d, radix, digits_mode))))
                            }
                            _ => None,
                        }
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }
        "read-ubits" | "read-bits" => {
            let bytes = buf_get_bytes(target)?;
            let from = runtime::to_int(arg1);
            let bits = runtime::to_int(arg2);
            if from < 0 || bits < 0 {
                return Some(Err(RuntimeError::new(
                    "bit offset/length must be non-negative",
                )));
            }
            let from = from as usize;
            let bits = bits as usize;
            let total_bits = bytes.len().saturating_mul(8);
            if from.checked_add(bits).is_none_or(|end| end > total_bits) {
                return Some(Err(RuntimeError::new(format!(
                    "read from out of range. Is: {}, should be in 0..{}",
                    from, total_bits
                ))));
            }
            let unsigned = read_ubits_from_bytes(&bytes, from, bits);
            if method == "read-ubits" || bits == 0 {
                return Some(Ok(bigint_to_value(unsigned)));
            }
            let sign_bit = BigInt::from(1u8) << (bits - 1);
            let signed = if (&unsigned & &sign_bit).is_zero() {
                unsigned
            } else {
                unsigned - (BigInt::from(1u8) << bits)
            };
            Some(Ok(bigint_to_value(signed)))
        }
        // Buf/Blob read-num methods (2 args: offset + endian)
        "read-num32" | "read-num64" => {
            if let Value::Package(type_name) = target {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller {}({}:U)",
                    method, type_name,
                ))));
            }
            let bytes = buf_get_bytes(target)?;
            let offset_i64 = to_int_val(arg1);
            let endian_val = match arg2 {
                Value::Enum { value, .. } => value.as_i64(),
                Value::Int(i) => *i,
                _ => 0, // NativeEndian
            };
            let size: usize = if method == "read-num32" { 4 } else { 8 };
            if offset_i64 < 0
                || (offset_i64 as usize)
                    .checked_add(size)
                    .is_none_or(|end| end > bytes.len())
            {
                return Some(Err(RuntimeError::new(format!(
                    "read from out of range. Is: {}, should be in 0..{}",
                    offset_i64,
                    bytes.len()
                ))));
            }
            let offset = offset_i64 as usize;
            let result = if size == 4 {
                read_f32_endian(&bytes[offset..offset + 4], endian_val)
            } else {
                read_f64_endian(&bytes[offset..offset + 8], endian_val)
            };
            Some(Ok(Value::Num(result)))
        }
        // Buf/Blob read-int/uint methods (2 args: offset + endian)
        "read-uint8" | "read-int8" | "read-uint16" | "read-int16" | "read-uint32"
        | "read-int32" | "read-uint64" | "read-int64" | "read-uint128" | "read-int128" => {
            if let Value::Package(type_name) = target {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller {}({}:U)",
                    method, type_name,
                ))));
            }
            let bytes = buf_get_bytes(target)?;
            let offset_i64 = to_int_val(arg1);
            let endian_val = match arg2 {
                Value::Enum { value, .. } => value.as_i64(),
                Value::Int(i) => *i,
                _ => 0, // NativeEndian
            };
            let (size, signed) = read_int_method_info(method);
            if offset_i64 < 0
                || (offset_i64 as usize)
                    .checked_add(size)
                    .is_none_or(|end| end > bytes.len())
            {
                return Some(Err(RuntimeError::new(format!(
                    "read from out of range. Is: {}, should be in 0..{}",
                    offset_i64,
                    bytes.len()
                ))));
            }
            let offset = offset_i64 as usize;
            Some(Ok(read_int_value(
                &bytes[offset..offset + size],
                size,
                signed,
                endian_val,
            )))
        }
        "subbuf" => {
            if !is_buf_like(target) {
                return None;
            }
            let items = buf_get_int_items(target)?;
            let cn = buf_class_name(target);
            let len = items.len();
            let start = resolve_buf_index(arg1, len);
            if start < 0 {
                return Some(Err(out_of_range_error(start, 0, len as i64)));
            }
            if start as usize > len {
                return Some(Err(out_of_range_error(start, 0, len as i64)));
            }
            // arg2 is the length
            let sub_len = resolve_buf_len(arg2, len, start as usize);
            if sub_len < 0 {
                return Some(Err(out_of_range_error(sub_len, 0, len as i64)));
            }
            let s = start as usize;
            let available = len - s;
            let take = (sub_len as usize).min(available);
            Some(Ok(make_buf_from_int_items(&cn, &items[s..s + take])))
        }
        "subbuf-rw" => {
            if !is_buf_like(target) {
                return None;
            }
            let items = buf_get_int_items(target)?;
            let cn = buf_class_name(target);
            let len = items.len();
            let start = resolve_buf_index(arg1, len);
            if start < 0 || start as usize > len {
                return Some(Err(out_of_range_error(start, 0, len as i64)));
            }
            let sub_len = resolve_buf_len(arg2, len, start as usize);
            if sub_len < 0 {
                return Some(Err(out_of_range_error(sub_len, 0, len as i64)));
            }
            let s = start as usize;
            let available = len - s;
            let take = (sub_len as usize).min(available);
            Some(Ok(make_buf_from_int_items(&cn, &items[s..s + take])))
        }
        _ => None,
    }
}

fn parse_radix_checked(arg: &Value) -> Option<Result<u32, RuntimeError>> {
    match arg {
        Value::Int(r) => {
            if (2..=36).contains(r) {
                Some(Ok(*r as u32))
            } else {
                Some(Err(RuntimeError::new(
                    "X::OutOfRange: base requires radix 2..36",
                )))
            }
        }
        Value::Str(s) => match s.parse::<u32>() {
            Ok(r) if (2..=36).contains(&r) => Some(Ok(r)),
            Ok(_) => Some(Err(RuntimeError::new(
                "X::OutOfRange: base requires radix 2..36",
            ))),
            _ => None,
        },
        _ => None,
    }
}

/// Specifies how many fractional digits to produce in base conversion.
enum BaseDigits {
    /// Auto-detect: scale to denominator size, minimum 6 (default for 1-arg .base)
    Auto,
    /// Exact number of digits (from explicit integer argument)
    Fixed(u32),
    /// Whatever: produce digits until remainder is 0 (no rounding)
    Whatever,
}

/// Convert a rational number n/d to a string in the given base.
fn rat_to_base(n: i64, d: i64, radix: u32, digits_mode: BaseDigits) -> String {
    if d == 0 {
        return if n > 0 {
            "Inf".to_string()
        } else if n < 0 {
            "-Inf".to_string()
        } else {
            "NaN".to_string()
        };
    }
    let negative = (n < 0) != (d < 0);
    let mut num = (n as i128).unsigned_abs();
    let den = (d as i128).unsigned_abs();
    let radix = radix as u128;

    let int_part = num / den;
    num %= den;

    let mut result = if negative {
        "-".to_string()
    } else {
        String::new()
    };
    result.push_str(&int_to_base(int_part as u64, radix as u32));

    if num == 0 {
        if let BaseDigits::Fixed(digits) = digits_mode
            && digits > 0
        {
            result.push('.');
            for _ in 0..digits {
                result.push('0');
            }
        }
        return result;
    }

    // Whatever mode: produce exact digits until remainder is 0
    if matches!(digits_mode, BaseDigits::Whatever) {
        result.push('.');
        let digit_chars = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        // Safety limit to prevent infinite loops for non-terminating fractions
        for _ in 0..100000 {
            num *= radix;
            let digit = num / den;
            result.push(digit_chars[digit as usize] as char);
            num %= den;
            if num == 0 {
                break;
            }
        }
        return result;
    }

    let limit = match digits_mode {
        BaseDigits::Fixed(d) => d,
        BaseDigits::Auto => {
            // For Rat without explicit digits, scale to denominator size with minimum of 6
            let log_den = (den as f64).log(radix as f64).ceil() as u32;
            std::cmp::max(6, log_den)
        }
        BaseDigits::Whatever => unreachable!(),
    };
    if limit == 0 {
        // Round integer part: if fractional part >= 0.5, round up
        if num * 2 >= den {
            // Increment the integer part and rebuild
            let rounded_int = int_part + 1;
            let mut rounded_result = if negative {
                "-".to_string()
            } else {
                String::new()
            };
            rounded_result.push_str(&int_to_base(rounded_int as u64, radix as u32));
            return rounded_result;
        }
        return result;
    }

    result.push('.');
    let digit_chars = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    let mut frac_digits = Vec::new();
    let is_auto = matches!(digits_mode, BaseDigits::Auto);
    for _ in 0..limit {
        num *= radix;
        let digit = num / den;
        frac_digits.push(digit as u8);
        num %= den;
        if num == 0 && is_auto {
            break;
        }
    }
    // Round last digit if we have a remainder
    let mut int_carry = false;
    if num > 0 && !frac_digits.is_empty() && num * 2 >= den {
        let mut carry = true;
        for d in frac_digits.iter_mut().rev() {
            if carry {
                *d += 1;
                if *d >= radix as u8 {
                    *d = 0;
                } else {
                    carry = false;
                }
            }
        }
        if carry {
            // Carry propagated past all fractional digits (e.g., 0.9 -> 1.0)
            int_carry = true;
        }
    }
    if int_carry {
        // Rebuild result with incremented integer part
        let rounded_int = int_part + 1;
        result.clear();
        if negative {
            result.push('-');
        }
        result.push_str(&int_to_base(rounded_int as u64, radix as u32));
        result.push('.');
        for _ in 0..frac_digits.len() {
            result.push('0');
        }
    } else {
        for d in &frac_digits {
            result.push(digit_chars[*d as usize] as char);
        }
    }
    result
}

fn int_to_base(mut n: u64, radix: u32) -> String {
    if n == 0 {
        return "0".to_string();
    }
    let digits = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    let mut buf = Vec::new();
    while n > 0 {
        buf.push(digits[(n % radix as u64) as usize]);
        n /= radix as u64;
    }
    buf.reverse();
    String::from_utf8(buf).unwrap()
}

/// Convert f64 to a rational approximation (numerator, denominator).
fn f64_to_rat(f: f64) -> (i64, i64) {
    if f.is_nan() {
        return (0, 0);
    }
    if f.is_infinite() {
        return if f > 0.0 { (1, 0) } else { (-1, 0) };
    }
    // Multiply by increasing powers of 10 to find exact fraction
    let negative = f < 0.0;
    let f = f.abs();
    let mut den: i64 = 1;
    let mut num = f;
    for _ in 0..18 {
        if (num - num.round()).abs() < 1e-10 {
            break;
        }
        num *= 10.0;
        den *= 10;
    }
    let n = num.round() as i64;
    // Simplify
    let g = gcd_u64(n.unsigned_abs(), den.unsigned_abs());
    let n = n / g as i64;
    let d = den / g as i64;
    if negative { (-n, d) } else { (n, d) }
}

fn gcd_u64(mut a: u64, mut b: u64) -> u64 {
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

/// Compute base-repeating for a rational number.
/// Returns (non_repeating_part, repeating_part) as strings.
fn rat_base_repeating(n: i64, d: i64, radix: u32) -> (String, String) {
    use std::collections::HashMap;

    if d == 0 {
        return (
            if n > 0 {
                "Inf".to_string()
            } else if n < 0 {
                "-Inf".to_string()
            } else {
                "NaN".to_string()
            },
            String::new(),
        );
    }
    let negative = (n < 0) != (d < 0);
    let mut num = (n as i128).unsigned_abs();
    let den = (d as i128).unsigned_abs();
    let radix128 = radix as u128;

    let int_part = num / den;
    num %= den;

    let mut prefix = if negative {
        "-".to_string()
    } else {
        String::new()
    };
    prefix.push_str(&int_to_base(int_part as u64, radix));

    if num == 0 {
        return (prefix, String::new());
    }

    prefix.push('.');
    let digit_chars = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

    // Track remainders to find the cycle
    let mut remainder_positions: HashMap<u128, usize> = HashMap::new();
    let mut frac_digits = Vec::new();

    loop {
        if num == 0 {
            // Exact representation, no repeating part
            let non_rep: String = frac_digits
                .iter()
                .map(|&d| digit_chars[d] as char)
                .collect();
            return (format!("{}{}", prefix, non_rep), String::new());
        }
        if let Some(&pos) = remainder_positions.get(&num) {
            // Found a cycle
            let non_rep: String = frac_digits[..pos]
                .iter()
                .map(|&d| digit_chars[d] as char)
                .collect();
            let rep: String = frac_digits[pos..]
                .iter()
                .map(|&d| digit_chars[d] as char)
                .collect();
            return (format!("{}{}", prefix, non_rep), rep);
        }
        remainder_positions.insert(num, frac_digits.len());
        num *= radix128;
        let digit = (num / den) as usize;
        frac_digits.push(digit);
        num %= den;
    }
}

/// Fast path for `.pick(n)` on integer Range types.
/// Returns None if target is not an integer range (caller should fall back).
fn range_pick_n_fast(target: &Value, arg: &Value) -> Option<Value> {
    use crate::builtins::methods_0arg::dispatch_core_range::{
        generic_range_pick_n, range_pick_n_i64,
    };

    // Determine effective inclusive bounds
    let (start, end, is_generic, generic_start, generic_end, excl_start, excl_end) = match target {
        Value::Range(a, b) => (*a, *b, false, None, None, false, false),
        Value::RangeExcl(a, b) => (*a, *b - 1, false, None, None, false, false),
        Value::RangeExclStart(a, b) => (*a + 1, *b, false, None, None, false, false),
        Value::RangeExclBoth(a, b) => (*a + 1, *b - 1, false, None, None, false, false),
        Value::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => (
            0,
            0,
            true,
            Some(start.clone()),
            Some(end.clone()),
            *excl_start,
            *excl_end,
        ),
        _ => return None,
    };

    // Determine count from arg
    let is_whatever = matches!(arg, Value::Whatever)
        || matches!(arg, Value::Num(f) if f.is_infinite() && f.is_sign_positive());

    if is_generic {
        let gs = generic_start.as_ref().unwrap();
        let ge = generic_end.as_ref().unwrap();
        // Check if endpoints are integer-like
        if !matches!(
            gs.as_ref(),
            Value::Int(_) | Value::BigInt(_) | Value::Bool(_)
        ) || !matches!(
            ge.as_ref(),
            Value::Int(_) | Value::BigInt(_) | Value::Bool(_)
        ) {
            return None;
        }

        if is_whatever {
            // pick(*) on a huge range — cannot materialize, return what we can
            // Actually for GenericRange with BigInt endpoints, pick(*) means return all
            // elements shuffled. For very large ranges this is impossible, so fall back.
            return None;
        }

        let count = match arg {
            Value::Int(n) => (*n).max(0) as usize,
            Value::Num(f) => (*f as i64).max(0) as usize,
            Value::Rat(n, d) if *d != 0 => (*n / *d).max(0) as usize,
            Value::Str(s) => s.trim().parse::<i64>().unwrap_or(0).max(0) as usize,
            _ => return None,
        };

        if count == 0 {
            return Some(Value::array(Vec::new()));
        }

        let items = generic_range_pick_n(gs, ge, excl_start, excl_end, count)?;
        Some(Value::array(items))
    } else {
        if end < start {
            return Some(Value::array(Vec::new()));
        }

        if is_whatever {
            // pick(*) — return all elements shuffled
            // Only feasible for ranges that fit in memory
            let range_size = (end as i128) - (start as i128) + 1;
            if range_size > 100_000_000 {
                // Too large to materialize, fall back
                return None;
            }
            let items = range_pick_n_i64(start, end, range_size as usize);
            return Some(Value::array(items));
        }

        let count = match arg {
            Value::Int(n) => (*n).max(0) as usize,
            Value::Num(f) => (*f as i64).max(0) as usize,
            Value::Rat(n, d) if *d != 0 => (*n / *d).max(0) as usize,
            Value::Str(s) => s.trim().parse::<i64>().unwrap_or(0).max(0) as usize,
            _ => return None,
        };

        if count == 0 {
            return Some(Value::array(Vec::new()));
        }

        let items = range_pick_n_i64(start, end, count);
        Some(Value::array(items))
    }
}
