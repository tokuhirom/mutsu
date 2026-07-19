use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{ArrayKind, EnumValue, RuntimeError, Value, ValueView};
use num_traits::{Signed, ToPrimitive, Zero};
use unicode_normalization::UnicodeNormalization;

use super::rng::builtin_rand;

pub(crate) mod coercion;
pub(crate) mod collection;
pub(crate) mod complex_math;
mod dispatch_core_coerce;
mod dispatch_core_list;
mod dispatch_core_math;
mod dispatch_core_numeric;
pub(crate) mod dispatch_core_range;
mod dispatch_core_repr;
mod dispatch_core_str;
mod dispatch_core_unicode;
pub(crate) mod match_helpers;
pub(crate) mod raku_repr;
pub(crate) mod temporal;
pub(crate) mod temporal_dispatch;

use std::collections::HashMap;

/// Create an X::Multi::NoMatch error for a method called on a type object.
fn make_no_match_error(method_name: &str) -> RuntimeError {
    let msg = format!("Cannot resolve caller {}", method_name);
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Multi::NoMatch"), attrs);
    let mut err = RuntimeError::new(&msg);
    err.exception = Some(Box::new(ex));
    err
}

///// Raku-style rounding: round half toward positive infinity (ceiling).
fn raku_round(x: f64) -> f64 {
    (x + 0.5).floor()
}

/// Raku-style rounding that returns a Value, using BigInt for large values.
fn raku_round_to_value(f: f64) -> Value {
    let rounded = raku_round(f);
    if rounded >= i64::MIN as f64 && rounded <= i64::MAX as f64 {
        Value::int(rounded as i64)
    } else {
        use num_bigint::BigInt;
        use num_traits::ToPrimitive;
        let s = format!("{:.0}", rounded);
        if let Ok(bi) = s.parse::<BigInt>() {
            if let Some(i) = bi.to_i64() {
                Value::int(i)
            } else {
                Value::bigint(bi)
            }
        } else {
            Value::num(rounded)
        }
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
    let mut needle = builtin_rand() * total;
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

fn sample_weighted_bag_key(items: &HashMap<String, num_bigint::BigInt>) -> Option<Value> {
    use crate::runtime::utils::bigint_to_i128_sat;
    let mut total: i128 = 0;
    for count in items.values() {
        let count = bigint_to_i128_sat(count);
        if count > 0 {
            total = total.saturating_add(count);
        }
    }
    if total <= 0 {
        return None;
    }
    let needle_f = builtin_rand() * total as f64;
    let mut needle = needle_f as i128;
    if needle >= total {
        needle = total - 1;
    }
    for (key, count) in items {
        let count = bigint_to_i128_sat(count);
        if count <= 0 {
            continue;
        }
        if needle < count {
            return Some(Value::str(key.clone()));
        }
        needle -= count;
    }
    items
        .iter()
        .find_map(|(key, count)| count.is_positive().then(|| Value::str(key.clone())))
}

/// Normalize Unicode Nd (decimal digit) characters to their ASCII equivalents.
/// Returns `None` if any non-sign, non-digit character is found.
fn normalize_unicode_digits(s: &str) -> Option<String> {
    let mut result = String::with_capacity(s.len());
    let mut has_unicode = false;
    for ch in s.chars() {
        if ch.is_ascii_digit() || ch == '-' || ch == '+' || ch == '_' || ch == '.' {
            result.push(ch);
        } else if ch == '\u{2212}' {
            result.push('-');
            has_unicode = true;
        } else if let Some(d) = crate::builtins::unicode::unicode_decimal_digit_value(ch) {
            result.push(char::from_digit(d, 10).unwrap());
            has_unicode = true;
        } else {
            return None;
        }
    }
    if has_unicode { Some(result) } else { None }
}

fn parse_raku_int_from_str(s: &str) -> Option<Value> {
    let trimmed = s.trim();
    if trimmed.is_empty() {
        return None;
    }
    // Try normalizing Unicode digits to ASCII before parsing
    if let Some(ascii) = normalize_unicode_digits(trimmed) {
        return parse_raku_int_from_str(&ascii);
    }
    let normalized = trimmed.replace('\u{2212}', "-");
    let (sign, body) = if let Some(rest) = normalized.strip_prefix('-') {
        (-1_i32, rest)
    } else if let Some(rest) = normalized.strip_prefix('+') {
        (1_i32, rest)
    } else {
        (1_i32, normalized.as_str())
    };
    let body_no_underscores = body.replace('_', "");
    if body_no_underscores.is_empty() {
        return None;
    }
    if let Some((radix, digits)) = body_no_underscores
        .strip_prefix("0x")
        .or_else(|| body_no_underscores.strip_prefix("0X"))
        .map(|digits| (16_u32, digits))
        .or_else(|| {
            body_no_underscores
                .strip_prefix("0o")
                .or_else(|| body_no_underscores.strip_prefix("0O"))
                .map(|digits| (8_u32, digits))
        })
        .or_else(|| {
            body_no_underscores
                .strip_prefix("0b")
                .or_else(|| body_no_underscores.strip_prefix("0B"))
                .map(|digits| (2_u32, digits))
        })
        .or_else(|| {
            body_no_underscores
                .strip_prefix("0d")
                .or_else(|| body_no_underscores.strip_prefix("0D"))
                .map(|digits| (10_u32, digits))
        })
    {
        if digits.is_empty() {
            return None;
        }
        let mut n = num_bigint::BigInt::parse_bytes(digits.as_bytes(), radix)?;
        if sign < 0 {
            n = -n;
        }
        return Some(Value::from_bigint(n));
    }

    let signed_no_underscores = if sign < 0 {
        format!("-{}", body_no_underscores)
    } else {
        body_no_underscores
    };
    if let Ok(n) = signed_no_underscores.parse::<num_bigint::BigInt>() {
        return Some(Value::from_bigint(n));
    }

    if let Ok(f) = signed_no_underscores.parse::<f64>()
        && f.is_finite()
    {
        let truncated = f.trunc();
        let digits = format!("{:.0}", truncated);
        if let Ok(n) = digits.parse::<num_bigint::BigInt>() {
            return Some(Value::from_bigint(n));
        }
    }
    None
}

fn int_lsb_value(target: &Value) -> Option<Value> {
    match target.view() {
        ValueView::Int(i) => {
            if i == 0 {
                Some(Value::NIL)
            } else {
                Some(Value::int(i.unsigned_abs().trailing_zeros() as i64))
            }
        }
        ValueView::BigInt(n) => {
            if n.is_zero() {
                return Some(Value::NIL);
            }
            let one = num_bigint::BigInt::from(1u8);
            let mut x = n.as_ref().abs();
            let mut pos = 0_i64;
            while (&x & &one).is_zero() {
                x >>= 1;
                pos += 1;
            }
            Some(Value::int(pos))
        }
        _ => None,
    }
}

fn int_msb_value(target: &Value) -> Option<Value> {
    match target.view() {
        ValueView::Int(i) => {
            if i == 0 {
                return Some(Value::NIL);
            }
            if i > 0 {
                return Some(Value::int((63 - i.leading_zeros()) as i64));
            }
            if i == -1 {
                return Some(Value::int(0));
            }
            let m = i.unsigned_abs().saturating_sub(1);
            let bitlen = (64 - m.leading_zeros()) as i64;
            Some(Value::int(bitlen))
        }
        ValueView::BigInt(n) => {
            if n.is_zero() {
                return Some(Value::NIL);
            }
            if n.sign() == num_bigint::Sign::Minus {
                if **n == num_bigint::BigInt::from(-1i8) {
                    return Some(Value::int(0));
                }
                let mut x = n.as_ref().abs() - num_bigint::BigInt::from(1u8);
                let mut bitlen = 0_i64;
                while !x.is_zero() {
                    x >>= 1;
                    bitlen += 1;
                }
                return Some(Value::int(bitlen));
            }
            let mut x = n.as_ref().clone();
            let mut msb = -1_i64;
            while !x.is_zero() {
                x >>= 1;
                msb += 1;
            }
            Some(Value::int(msb))
        }
        _ => None,
    }
}

/// Format a single item for 0-arg `.fmt()` on lists.
/// Pairs format as "%s\t%s", other values as "%s".
fn fmt_0arg_item(item: &Value) -> String {
    match item.view() {
        ValueView::Pair(k, v) => {
            runtime::format_sprintf_args("%s\t%s", &[Value::str(k.to_string()), v.clone()])
        }
        ValueView::ValuePair(k, v) => {
            runtime::format_sprintf_args("%s\t%s", &[k.clone(), v.clone()])
        }
        _ => runtime::format_sprintf("%s", Some(item)),
    }
}

// ── 0-arg method dispatch ────────────────────────────────────────────
/// Try to dispatch a 0-argument method call on a Value.
/// Returns `Some(Ok(..))` / `Some(Err(..))` when handled, `None` to fall through.
pub(crate) fn native_method_0arg(
    target: &Value,
    method_sym: Symbol,
) -> Option<Result<Value, RuntimeError>> {
    let method = method_sym.resolve();
    let method = method.as_str();

    // Scalar containers are transparent for method dispatch (except .VAR and
    // .raku/.perl). `.raku`/`.perl` must see the `Scalar` wrapper so an itemized
    // aggregate shows its `$` sigil (`${a=>1}.raku` → `${:a(1)}`); decontainer-
    // izing first would strip it. `.gist` never shows the sigil, so delegating
    // to the inner value is already correct.
    if let ValueView::Scalar(inner) = target.view() {
        if method == "VAR" {
            return Some(Ok(Value::package(crate::symbol::Symbol::intern("Scalar"))));
        }
        if method == "raku" || method == "perl" {
            return Some(Ok(Value::str(raku_repr::raku_value(target))));
        }
        return native_method_0arg(inner, method_sym);
    }

    // `.dynamic` on a container VALUE — a literal `[1,2,3]`/`{a=>1}`, or an
    // Array/Hash flowing as a value — is always False: only a dynamic *variable*
    // (`@*a`/`%*h`) is dynamic, and that case is rewritten to `.VAR.dynamic` at
    // compile time (`compile_expr_method_on_var`). Array and Hash carry a
    // container descriptor and so define `.dynamic`; List/Int/Str/... do not
    // (raku throws "No such method" there), so restrict this to real Array kinds
    // (not `List`) and Hash and let everything else fall through.
    if method == "dynamic" {
        match target.view() {
            ValueView::Array(_, kind) if !matches!(kind, crate::value::ArrayKind::List) => {
                return Some(Ok(Value::FALSE));
            }
            ValueView::Hash(_) => return Some(Ok(Value::FALSE)),
            _ => {}
        }
    }

    // Seq consumed/cached state checks.
    // Only handle operations that are fully dispatched here in native_method_0arg.
    // Do NOT pre-check methods that fall through to the runtime (like "iterator"),
    // because native_method_0arg is called from both the VM and the interpreter,
    // and consuming twice would throw.
    if let ValueView::Seq(items) = target.view() {
        if method == "cache" {
            // .cache marks as cached; handled fully here (the actual cache impl is
            // in the per-method handler below, this just marks state).
            crate::value::seq_mark_cached(&items);
        } else if method == "is-lazy" && crate::value::seq_is_consumed(&items) {
            // Read-only check: throws on consumed Seq but does NOT consume.
            return Some(Err(crate::value::seq_consumed_error()));
        }
    }

    // Instance with __baggy_data__: delegate Bag-like methods to the inner Bag/Set
    // so that subclasses of Bag/Set (e.g. `my class MyBag is Bag {}`) work correctly.
    if let ValueView::Instance { attributes, .. } = target.view()
        && let Some(inner) = attributes.as_map().get("__baggy_data__")
        && !matches!(
            method,
            "WHAT" | "WHICH" | "raku" | "gist" | "Str" | "perl" | "isa" | "^name"
        )
    {
        return native_method_0arg(inner, method_sym);
    }

    // Version introspection: `.parts` (list of Int/Str/Whatever parts),
    // `.plus` (trailing `+`), `.whatever` (any `*` part). Used by zef's
    // DependencySpecification version matching.
    if let ValueView::Version { parts, plus, .. } = target.view() {
        match method {
            "parts" => {
                let items: Vec<Value> = parts
                    .iter()
                    .map(|p| match p {
                        crate::value::VersionPart::Num(n) => Value::int(*n),
                        crate::value::VersionPart::Str(s) => Value::str_from(s.as_str()),
                        crate::value::VersionPart::Whatever => Value::WHATEVER,
                    })
                    .collect();
                return Some(Ok(Value::array_with_kind(
                    crate::gc::Gc::new(crate::value::ArrayData::new(items)),
                    crate::value::ArrayKind::List,
                )));
            }
            "plus" => {
                return Some(Ok(if plus { Value::TRUE } else { Value::FALSE }));
            }
            "whatever" => {
                let any_star = parts
                    .iter()
                    .any(|p| matches!(p, crate::value::VersionPart::Whatever));
                return Some(Ok(if any_star { Value::TRUE } else { Value::FALSE }));
            }
            _ => {}
        }
    }

    // $!.pending returns a list of all tracked Failure values (S04 spec).
    if method == "pending" {
        let failures = crate::value::get_pending_failures();
        return Some(Ok(Value::array_with_kind(
            crate::gc::Gc::new(crate::value::ArrayData::new(failures)),
            crate::value::ArrayKind::List,
        )));
    }

    // Nil absorber for common methods: Nil.message, Nil.payload, etc.
    // In Raku, calling most methods on Nil returns Nil.
    if target.is_nil()
        && matches!(
            method,
            "message" | "payload" | "backtrace" | "exception" | "handled" | "line" | "file"
        )
    {
        return Some(Ok(Value::NIL));
    }

    // Failure safe accessors: `.exception` (the stored exception object) and
    // `.handled` (read of the global handled flag). Both are in the
    // interpreter's no-explode safe list, so dispatching them natively is
    // correct — a *non*-safe method (e.g. `.message`) is not handled here, so it
    // returns `None` and reaches the interpreter, which explodes an unhandled
    // Failure. (The `.handled = ...` setter is the 1-arg form, handled
    // elsewhere; this only covers the 0-arg read.)
    if let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
        && class_name == "Failure"
    {
        match method {
            "exception" => {
                return Some(Ok(attributes
                    .as_map()
                    .get("exception")
                    .cloned()
                    .unwrap_or(Value::NIL)));
            }
            "handled" => {
                return Some(Ok(Value::truth(target.is_failure_handled())));
            }
            _ => {}
        }
    }

    // For Mixin values, handle Bool/WHICH method specially, then delegate to inner.
    if let ValueView::Mixin(inner, mixins) = target.view() {
        if method == "Bool"
            && let Some(bool_val) = mixins.get("Bool")
        {
            return Some(Ok(bool_val.clone()));
        }
        if (method == "Str" || method == "~")
            && let Some(str_val) = mixins.get("Str")
        {
            return Some(Ok(str_val.clone()));
        }
        // A Bool with a mixed-in Bool override (`True but False`) renders its
        // Str/gist/raku from the *effective* boolean — `Bool.Str` is
        // `self ?? 'True' !! 'False'`, so it follows `.Bool`, not the base bool.
        if matches!(inner.as_ref().view(), ValueView::Bool(_))
            && mixins.get("Str").is_none()
            && let Some(ValueView::Bool(b)) = mixins.get("Bool").map(Value::view)
        {
            match method {
                "Str" | "~" | "gist" => {
                    return Some(Ok(Value::str(if b { "True" } else { "False" }.to_string())));
                }
                "raku" | "perl" => {
                    return Some(Ok(Value::str(
                        if b { "Bool::True" } else { "Bool::False" }.to_string(),
                    )));
                }
                _ => {}
            }
        }
        // An allomorph (IntStr/NumStr/…) gists as its preserved source string,
        // not the inner numeric's gist: `<1e3>.gist` → `1e3` (not `1000`). Only
        // allomorphs; a general `but`-mixin gists via its inner value (below).
        if method == "gist"
            && crate::value::types::allomorph_type_name(inner, mixins).is_some()
            && let Some(str_val) = mixins.get("Str")
        {
            return Some(Ok(str_val.clone()));
        }
        if method == "WHICH"
            && let Some(allo_name) = crate::value::types::allomorph_type_name(inner, mixins)
        {
            let inner_which = match inner.as_ref().view() {
                ValueView::Int(n) => format!("Int|{}", n),
                ValueView::BigInt(n) => format!("Int|{}", *n),
                ValueView::Num(n) => format!("Num|{}", n),
                ValueView::Rat(n, d) => format!("Rat|{}/{}", n, d),
                ValueView::FatRat(n, d) => format!("FatRat|{}/{}", n, d),
                ValueView::Complex(r, i) => format!("Complex|{}+{}i", r, i),
                _ => format!("{:?}", inner),
            };
            let str_part = mixins
                .get("Str")
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            let which_str = format!("{}|{}|Str|{}", allo_name, inner_which, str_part);
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("WHICH".to_string(), Value::str(which_str));
            return Some(Ok(Value::make_instance(
                crate::symbol::Symbol::intern("ValueObjAt"),
                attrs,
            )));
        }
        // For allomorphic types, string-oriented methods should use the Str part.
        if let Some(str_val) = mixins.get("Str") {
            match method {
                "comb" | "chars" | "codes" | "words" | "lines" | "chomp" | "chop" | "trim"
                | "trim-leading" | "trim-trailing" | "uc" | "lc" | "tc" | "tclc" | "fc"
                | "flip" | "samemark" | "samespace" | "uniname" | "uninames" | "unival"
                | "univals" | "uniprop" | "uniprops" | "uniparse" | "parse-names" | "NFC"
                | "NFD" | "NFKC" | "NFKD" | "encode" => {
                    return native_method_0arg(str_val, method_sym);
                }
                _ => {}
            }
        }
        // An allomorph (IntStr/RatStr/NumStr/ComplexStr) renders its `.raku` /
        // `.perl` as `TypeStr.new(<numeric>, "<string>")`, NOT the bare inner
        // value's `.raku`. (`.gist`/`.Str` keep the source-string form, handled
        // above; a general `but`-mixin falls through to the inner delegation.)
        if matches!(method, "raku" | "perl")
            && crate::value::types::allomorph_type_name(inner, mixins).is_some()
        {
            return Some(Ok(Value::str(raku_repr::raku_value(target))));
        }
        // A `.^set_name`-renamed anonymous mixin reports its friendly name for
        // `.^name` (e.g. `Foo.new but role {...}` then `.^set_name('X')` — used
        // by zef's plugin loader). Without this the caret-name would delegate to
        // the inner value and report the un-renamed base type.
        if method == "^name"
            && let Some(name_val) = mixins.get("__mutsu_type_name__")
        {
            return Some(Ok(name_val.clone()));
        }
        // A role-mixed value (`5 but Foo::Bar`, `$x does R`) reports its base type
        // with a `+{Role,...}` suffix (`Int+{Foo::Bar}`). Without this, `^name`
        // delegates to the inner value below and drops the mixed-in role names.
        if method == "^name" && crate::value::role_mixin_suffix(mixins).is_some() {
            return Some(Ok(Value::str(crate::value::what_type_name(target))));
        }
        // `.clone` on a mixed-in value must PRESERVE the mixin: `(5 but False).clone`
        // stays `Int+{...}` (Bool=False), and a Match — which is modelled as a
        // string carrying its match state as a mixin — must stay a Match. Delegating
        // to the inner value's clone drops the wrapper (and, now that scalar `.clone`
        // is a real method, returns the bare inner instead of falling through to the
        // slow path). Clone the inner and re-apply the mixins here. Role mixins with
        // `:attr(val)` overrides are left to the slow-path handler
        // (`methods_mixin_dispatch`), which threads the override args.
        if method == "clone"
            && !mixins
                .keys()
                .any(|k| k.starts_with("__mutsu_role__") || k.starts_with("__mutsu_attr__"))
        {
            let inner_clone = match native_method_0arg(inner, method_sym) {
                Some(Ok(v)) => v,
                Some(Err(e)) => return Some(Err(e)),
                None => inner.as_ref().clone(),
            };
            return Some(Ok(Value::mixin(inner_clone, mixins.as_ref().clone())));
        }
        // Check for mixin key matching the method name (e.g. "Array", "List", "Int", etc.)
        // This handles `True but [1, 2]` where `.Array` should return the mixed-in array.
        if let Some(mixin_val) = mixins.get(method) {
            return Some(Ok(mixin_val.clone()));
        }
        return native_method_0arg(inner, method_sym);
    }
    // Cool numeric coercion: when a Str calls a numeric method, coerce to numeric first.
    // In Raku, Cool types (including Str) coerce to Numeric for numeric operations.
    if let ValueView::Str(s) = target.view() {
        match method {
            "abs" | "sign" | "exp" | "log" | "log2" | "log10" | "sqrt" | "ceiling" | "floor"
            | "truncate" | "round" | "conj" | "cis" | "rand" | "sin" | "cos" | "tan" | "asin"
            | "acos" | "atan" | "sinh" | "cosh" | "tanh" | "sec" | "cosec" | "cotan" | "asec"
            | "acosec" | "acotan" | "sech" | "cosech" | "cotanh" | "asech" | "acosech"
            | "acotanh" | "atan2" | "narrow" | "polymod" | "base" | "chr" | "expmod" | "lsb"
            | "msb" | "is-int" | "re" | "im" => {
                let coerced = if let Ok(i) = s.parse::<i64>() {
                    Value::int(i)
                } else if let Ok(f) = s.parse::<f64>() {
                    Value::num(f)
                } else if let Some(v) = crate::runtime::str_numeric::parse_raku_str_to_numeric(&s) {
                    // A Str that numifies to a Complex/Rat (`"6+8i"`, `"1/2"`)
                    // must coerce fully before the numeric method runs, so
                    // `abs "6+8i"` is 10 and `"1+2i".conj` is 1-2i.
                    v
                } else {
                    parse_raku_int_from_str(&s)?
                };
                return native_method_0arg(&coerced, method_sym);
            }
            _ => {}
        }
    }
    // Any.nl-out returns the default newline separator "\n"
    if method == "nl-out" {
        return Some(Ok(Value::str_from("\n")));
    }
    // Native int coercer methods (.byte(), .int8(), .uint16(), etc.)
    if runtime::native_types::is_native_int_type(method) {
        return Some(raku_repr::native_int_coerce_method(target, method));
    }
    // Uni types: override .chars, .codes, .comb to work on codepoints
    if let ValueView::Uni(u) = target.view() {
        let text = &u.text;
        match method {
            "chars" | "codes" => {
                return Some(Ok(Value::int(text.chars().count() as i64)));
            }
            "comb" => {
                let parts: Vec<Value> = text.chars().map(|c| Value::str(c.to_string())).collect();
                return Some(Ok(Value::seq(parts)));
            }
            "Str" => {
                use unicode_normalization::UnicodeNormalization;
                return Some(Ok(Value::str(text.nfc().collect::<String>())));
            }
            "Int" | "Numeric" => {
                return Some(Ok(Value::int(text.chars().count() as i64)));
            }
            "list" => {
                let codepoints: Vec<Value> = text.chars().map(|c| Value::int(c as i64)).collect();
                return Some(Ok(Value::array(codepoints)));
            }
            "elems" => {
                return Some(Ok(Value::int(text.chars().count() as i64)));
            }
            "raku" | "perl" => {
                let codepoints: Vec<String> = text
                    .chars()
                    .map(|c| format!("0x{:04X}", c as u32))
                    .collect();
                // A normalization form appends the form, e.g. Uni.new(...).NFKC
                let suffix = if u.form.is_empty() {
                    String::new()
                } else {
                    format!(".{}", u.form)
                };
                return Some(Ok(Value::str(format!(
                    "Uni.new({}){}",
                    codepoints.join(", "),
                    suffix
                ))));
            }
            "gist" => {
                let codepoints: Vec<String> =
                    text.chars().map(|c| format!("{:04X}", c as u32)).collect();
                let form = if u.form.is_empty() {
                    "Uni"
                } else {
                    u.form.as_str()
                };
                return Some(Ok(Value::str(format!(
                    "{}:0x<{}>",
                    form,
                    codepoints.join(" ")
                ))));
            }
            "NFC" | "NFD" | "NFKC" | "NFKD" => {
                let normalized: String = match method {
                    "NFC" => text.nfc().collect(),
                    "NFD" => text.nfd().collect(),
                    "NFKC" => text.nfkc().collect(),
                    _ => text.nfkd().collect(),
                };
                return Some(Ok(Value::uni(method.to_string(), normalized)));
            }
            _ => {}
        }
    }
    // CompUnit::DependencySpecification methods
    if let ValueView::CompUnitDepSpec { short_name } = target.view() {
        return match method {
            "short-name" => Some(Ok(Value::str(short_name.resolve()))),
            "version-matcher" => Some(Ok(Value::TRUE)),
            "auth-matcher" => Some(Ok(Value::TRUE)),
            "api-matcher" => Some(Ok(Value::TRUE)),
            "Str" | "gist" => Some(Ok(Value::str(short_name.resolve()))),
            _ => None,
        };
    }
    // Capture methods
    if let ValueView::Capture { positional, named } = target.view()
        && let result @ Some(_) = dispatch_capture(positional, named, method)
    {
        return result;
    }
    // Try core string/numeric/array methods first
    if let result @ Some(_) = dispatch_core(target, method) {
        return result;
    }
    // Then collection methods (keys, values, kv, pairs, etc.)
    if let result @ Some(_) = collection::dispatch(target, method) {
        return result;
    }
    // Then type coercion and specialized methods
    coercion::dispatch(target, method)
}

fn dispatch_capture(
    positional: &[Value],
    named: &std::collections::HashMap<String, Value>,
    method: &str,
) -> Option<Result<Value, RuntimeError>> {
    match method {
        "hash" | "Hash" => {
            let mut map = std::collections::HashMap::new();
            for (k, v) in named {
                map.insert(k.clone(), v.clone());
            }
            // Capture.hash returns an immutable Map, not a mutable Hash.
            let mut data = crate::value::HashData::new(map);
            data.declared_type = Some("Map".to_string());
            Some(Ok(Value::hash_with_data(crate::gc::Gc::new(data))))
        }
        "list" => Some(Ok(Value::array(positional.to_vec()))),
        "elems" => Some(Ok(Value::int(positional.len() as i64))),
        "is-lazy" => Some(Ok(Value::FALSE)),
        "keys" => Some(Ok(Value::array(
            named.keys().map(|k| Value::str(k.clone())).collect(),
        ))),
        "values" => Some(Ok(Value::array(named.values().cloned().collect()))),
        "pairs" => Some(Ok(Value::array(
            named
                .iter()
                .map(|(k, v)| Value::pair(k.clone(), v.clone()))
                .collect(),
        ))),
        "raku" | "perl" => {
            let mut parts = Vec::new();
            for v in positional {
                match v.view() {
                    ValueView::Pair(k, val) => {
                        parts.push(format!(
                            "{} => {}",
                            raku_repr::raku_value(&Value::str(k.clone())),
                            raku_repr::raku_value(val)
                        ));
                    }
                    ValueView::ValuePair(k, val) => {
                        parts.push(format!(
                            "{} => {}",
                            raku_repr::raku_value(k),
                            raku_repr::raku_value(val)
                        ));
                    }
                    _ => parts.push(raku_repr::raku_value(v)),
                }
            }
            let mut named_keys: Vec<&String> = named.keys().collect();
            named_keys.sort();
            for k in named_keys {
                let v = &named[k];
                if let ValueView::Bool(true) = v.view() {
                    parts.push(format!(":{}", k));
                } else if let ValueView::Bool(false) = v.view() {
                    parts.push(format!(":!{}", k));
                } else {
                    parts.push(format!(":{}({})", k, raku_repr::raku_value(v)));
                }
            }
            Some(Ok(Value::str(format!("\\({})", parts.join(", ")))))
        }
        "gist" | "Str" => {
            let target = Value::capture(positional.to_vec(), named.clone());
            Some(Ok(Value::str(target.to_string_value())))
        }
        "Bool" => Some(Ok(Value::truth(
            !positional.is_empty() || !named.is_empty(),
        ))),
        "WHAT" => Some(Ok(Value::package(Symbol::intern("Capture")))),
        "flat" => {
            let cap = Value::capture(positional.to_vec(), named.clone());
            Some(Ok(Value::seq(vec![cap])))
        }
        "Seq" | "List" => {
            let cap = Value::capture(positional.to_vec(), named.clone());
            Some(Ok(Value::seq(vec![cap])))
        }
        _ => None,
    }
}

/// Create a X::Cannot::Lazy Failure for .elems on an infinite range.
fn range_elems_lazy_failure(action: &str) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(crate::runtime::utils::cannot_lazy_failure(action)))
}

/// True for a value whose element count cannot be reported, so numeric/count
/// coercions (`.elems`/`.Int`/`.Numeric`/`.end`/prefix `+`) throw
/// `X::Cannot::Lazy`: a lazy-backed Array or a genuinely-lazy `LazyList`
/// (infinite sequence/closure/pipe or a `lazy`-marked gather).
pub(crate) fn is_lazy_count_source(target: &Value) -> bool {
    match target.view() {
        ValueView::Array(_, kind) => kind.is_lazy(),
        ValueView::LazyList(ll) => ll.is_genuinely_lazy(),
        _ => false,
    }
}

fn is_infinite_endpoint(v: &Value) -> bool {
    match v.view() {
        ValueView::Whatever | ValueView::HyperWhatever => true,
        ValueView::Num(n) => n.is_infinite(),
        ValueView::Rat(n, d) => d == 0 && n != 0,
        ValueView::FatRat(n, d) => d == 0 && n != 0,
        _ => {
            let n = v.to_f64();
            n.is_infinite()
        }
    }
}

/// f64 value of a Range *start* endpoint for emptiness checks. A `Whatever`
/// start is an open lower bound (`*..1` is `-Inf..1`), so it maps to -Inf.
fn range_start_f64(v: &Value) -> f64 {
    match v.view() {
        ValueView::Whatever | ValueView::HyperWhatever => f64::NEG_INFINITY,
        _ => v.to_f64(),
    }
}

/// f64 value of a Range *end* endpoint for emptiness checks. A `Whatever` end
/// is an open upper bound (`1..*` is `1..Inf`), so it maps to +Inf.
fn range_end_f64(v: &Value) -> f64 {
    match v.view() {
        ValueView::Whatever | ValueView::HyperWhatever => f64::INFINITY,
        _ => v.to_f64(),
    }
}

fn is_infinite_range(value: &Value) -> bool {
    match value.view() {
        ValueView::Range(start, end)
        | ValueView::RangeExcl(start, end)
        | ValueView::RangeExclStart(start, end)
        | ValueView::RangeExclBoth(start, end) => end == i64::MAX || start == i64::MIN,
        ValueView::GenericRange { start, end, .. } => {
            if !(is_infinite_endpoint(start) || is_infinite_endpoint(end)) {
                return false;
            }
            // A range whose start strictly exceeds its end is empty (e.g.
            // `Inf..0`, `1..-Inf`), not infinite. NaN endpoints make this
            // comparison false, so NaN ranges stay "infinite" (they iterate
            // their NaN/-Inf start ad infinitum).
            let s = range_start_f64(start);
            let e = range_end_f64(end);
            // Empty (not infinite) only when start strictly exceeds end. A NaN
            // endpoint is unordered, so `partial_cmp` is `None` -> not empty ->
            // infinite (NaN ranges iterate their NaN start forever).
            !matches!(s.partial_cmp(&e), Some(std::cmp::Ordering::Greater))
        }
        _ => false,
    }
}

pub(crate) fn is_value_lazy(value: &Value) -> bool {
    matches!(value.view(), ValueView::LazyList(_))
        || matches!(value.view(), ValueView::Array(_, kind) if kind.is_lazy())
        || is_infinite_range(value)
        || matches!(value.view(), ValueView::Seq(items) if crate::value::seq_is_lazy(&items))
}

/// Format a range endpoint for display, converting i64::MAX to Inf and i64::MIN to -Inf.
fn range_endpoint_display(v: i64) -> String {
    if v == i64::MAX {
        "Inf".to_string()
    } else if v == i64::MIN {
        "-Inf".to_string()
    } else {
        v.to_string()
    }
}

/// Return the gist (compact display) representation of a Range value.
fn range_gist_string(value: &Value) -> String {
    // Range.gist is identical to Range.raku in Rakudo: numeric endpoints render
    // plainly, string endpoints are quoted (`"a".."c"`), `i64::MAX`/Whatever
    // endpoints render as `Inf`/`-Inf`, and `0..^N` uses the `^N` short form.
    // Delegate to the raku renderer so both stay in sync.
    if value.is_range() {
        raku_repr::raku_value(value)
    } else {
        value.to_string_value()
    }
}

fn gist_array_wrap(inner: &str, kind: ArrayKind) -> String {
    // `.gist` never shows the `$` itemization marker — at any nesting level
    // (only `.raku` does). So an itemized array/list gists exactly like its
    // non-itemized counterpart: `$[1,2].gist` → `[1 2]`, `$(1,2).gist` → `(1 2)`.
    match kind {
        ArrayKind::Array | ArrayKind::Shaped | ArrayKind::Lazy | ArrayKind::ItemArray => {
            format!("[{}]", inner)
        }
        ArrayKind::List | ArrayKind::ItemList => format!("({})", inner),
    }
}

/// Format a numeric value for Instant/Duration .raku output.
/// Always includes a decimal point (e.g. "42.0", "-400.2").
fn format_temporal_num(f: f64) -> String {
    if f.is_nan() {
        return "NaN".to_string();
    }
    if f.is_infinite() {
        return if f > 0.0 {
            "Inf".to_string()
        } else {
            "-Inf".to_string()
        };
    }
    // For values outside the safe i64 Rat range, emit scientific notation
    // so that Str -> Num literal round-trips through the parser instead of
    // overflowing the Rat literal parser.
    if f.is_finite() && f.abs() >= 1e18 {
        return format!("{:e}", f);
    }
    let s = format!("{}", f);
    if s.contains('.') {
        s
    } else {
        format!("{}.0", s)
    }
}

/// Render a `Backtrace::Frame` instance as its `.Str`/`.gist` text.
/// Shared by the frame's `.Str` handler and by `Backtrace.concise`/`.summary`
/// so the natively-computed strings stay byte-identical to a `.grep(...).join`.
fn backtrace_frame_str(attributes: &crate::gc::Gc<crate::value::InstanceAttrs>) -> String {
    let map = attributes.as_map();
    let subname = map
        .get("subname")
        .map(|v| v.to_string_value())
        .unwrap_or_default();
    let file = map
        .get("file")
        .map(|v| v.to_string_value())
        .unwrap_or_default();
    let line = map
        .get("line")
        .map(|v| v.to_string_value())
        .unwrap_or_else(|| "0".to_string());
    if subname == "<unit>" {
        format!("  in block <unit> at {} line {}", file, line)
    } else {
        format!("  in sub {} at {} line {}", subname, file, line)
    }
}

/// A `Backtrace::Frame` is a "routine" frame when it has a real subname
/// (not the synthetic `<unit>` bottom frame and not an anonymous block).
fn backtrace_frame_is_routine(attributes: &crate::gc::Gc<crate::value::InstanceAttrs>) -> bool {
    let subname = attributes
        .as_map()
        .get("subname")
        .map(|v| v.to_string_value())
        .unwrap_or_default();
    !subname.is_empty() && subname != "<unit>"
}

/// Re-export raku_value for backward compatibility.
pub use raku_repr::raku_value;

/// Re-export complex_trig for external use.
pub(crate) use complex_math::complex_trig;

/// Re-export the X::Str::Numeric Failure builder for the VM's prefix-`+` op.
pub(crate) use dispatch_core_coerce::str_numeric_failure;

/// Unicode case folding for `.fc` and `fc()`.
pub(crate) fn unicode_foldcase(s: &str) -> String {
    // Unicode full case folding (CaseFolding.txt, statuses C + F). Most
    // characters fold to their simple lowercase (`char::to_lowercase`); the
    // characters whose full fold expands to several codepoints (ligatures, ß,
    // the Greek iota-subscript vowels, ...) come from `full_case_fold`. A plain
    // NFKD would be wrong here — it also decomposes non-cased compatibility
    // characters (NBSP, superscripts, Roman numerals, circled/fullwidth
    // letters), which do not case-fold to their decomposition.
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match crate::builtins::unicode::full_case_fold(c as u32) {
            Some(folded) => out.push_str(folded),
            None => out.extend(c.to_lowercase()),
        }
    }
    out
}

fn dispatch_core(target: &Value, method: &str) -> Option<Result<Value, RuntimeError>> {
    fn has_date_attrs(attributes: &crate::gc::Gc<crate::value::InstanceAttrs>) -> bool {
        attributes.contains_key("year")
            && attributes.contains_key("month")
            && attributes.contains_key("day")
    }
    fn has_datetime_attrs(attributes: &crate::gc::Gc<crate::value::InstanceAttrs>) -> bool {
        has_date_attrs(attributes)
            && attributes.contains_key("hour")
            && attributes.contains_key("minute")
            && attributes.contains_key("second")
            && attributes.contains_key("timezone")
    }

    // Date/DateTime 0-arg methods
    match target.view() {
        ValueView::Instance { attributes, .. } if has_datetime_attrs(&attributes) => {
            if let Some(result) =
                temporal_dispatch::datetime_method_0arg(&(attributes).as_map(), method)
            {
                return Some(result);
            }
        }
        ValueView::Instance { attributes, .. } if has_date_attrs(&attributes) => {
            if let Some(result) =
                temporal_dispatch::date_method_0arg(&(attributes).as_map(), method)
            {
                return Some(result);
            }
        }
        _ => {}
    }

    // `Str.AST` — parse the string as Raku source and return its RakuAST tree
    // (ADR-0010). Applies to string values only.
    // `Str.AST` — parse the string as Raku source and return its RakuAST tree.
    // Non-string invocants fall through to normal dispatch.
    if let ValueView::Str(s) = target.view()
        && method == "AST"
    {
        return Some(crate::rakuast::str_dot_ast(&s));
    };

    // RakuAST node accessors (Phase 3): `.condition`, `.expression`, `.args`,
    // `.statements`, etc. return the node's field values. `.gist`/`.raku`/`.^name`
    // are handled elsewhere, so a non-accessor method name falls through.
    if let ValueView::RakuAst(node) = target.view()
        && let Some(v) = crate::rakuast::node_accessor(node, method)
    {
        return Some(Ok(v));
    }

    // Instant.Instant returns self (identity coercion)
    if method == "Instant" {
        match target.view() {
            ValueView::Instance { class_name, .. } if class_name == "Instant" => {
                return Some(Ok(target.clone()));
            }
            ValueView::Package(name) if name == "Instant" => {
                return Some(Ok(target.clone()));
            }
            _ => {}
        }
    }

    // Instant methods: to-posix, DateTime, Date, tai
    if let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
    {
        if class_name == "Instant" {
            use crate::builtins::methods_0arg::temporal;
            match method {
                "to-posix" => {
                    let val = attributes
                        .as_map()
                        .get("value")
                        .cloned()
                        .unwrap_or(Value::int(0));
                    let tai = crate::runtime::to_float_value(&val).unwrap_or(0.0);
                    let tai_int = tai.floor() as i64;
                    let mut is_leap = false;
                    for &(threshold, cumulative) in temporal::LEAP_SECONDS.iter().skip(1) {
                        if tai_int == threshold + (cumulative - 1) {
                            is_leap = true;
                            break;
                        }
                    }
                    let posix = temporal::instant_to_posix(tai);
                    let posix_val = if posix == posix.floor() {
                        Value::int(posix as i64)
                    } else {
                        Value::num(posix)
                    };
                    return Some(Ok(Value::array(vec![posix_val, Value::truth(is_leap)])));
                }
                "DateTime" => {
                    let val = attributes
                        .as_map()
                        .get("value")
                        .cloned()
                        .unwrap_or(Value::int(0));
                    let (tai_int, tai_frac) = match val.view() {
                        ValueView::Rat(n, d) if d != 0 => (n / d, (n % d) as f64 / d as f64),
                        _ => {
                            let f = crate::runtime::to_float_value(&val).unwrap_or(0.0);
                            (f.floor() as i64, f - f.floor())
                        }
                    };
                    let (y, m, d, h, mi, s) =
                        temporal::instant_to_datetime_leap_aware_parts(tai_int, tai_frac, 0);
                    return Some(Ok(temporal::make_datetime(y, m, d, h, mi, s, 0)));
                }
                "Date" => {
                    let val = attributes
                        .as_map()
                        .get("value")
                        .cloned()
                        .unwrap_or(Value::int(0));
                    let tai = crate::runtime::to_float_value(&val).unwrap_or(0.0);
                    let posix = temporal::instant_to_posix(tai);
                    let (y, m, d) = temporal::epoch_days_to_civil((posix / 86400.0).floor() as i64);
                    return Some(Ok(temporal::make_date(y, m, d)));
                }
                "tai" => {
                    return Some(Ok(attributes
                        .as_map()
                        .get("value")
                        .cloned()
                        .unwrap_or(Value::int(0))));
                }
                _ => {}
            }
        }
        if class_name == "Duration" {
            match method {
                "narrow" => {
                    let val = attributes
                        .as_map()
                        .get("value")
                        .cloned()
                        .unwrap_or(Value::num(0.0));
                    // Delegate narrow to the inner numeric value
                    match val.view() {
                        ValueView::Num(f) if f.is_finite() => {
                            let rounded = f.round();
                            if (f - rounded).abs() <= f.abs().max(rounded.abs()) * 1e-15
                                || (f == 0.0 && rounded == 0.0)
                            {
                                return Some(Ok(Value::int(rounded as i64)));
                            }
                            // Convert Num to Rat for narrow
                            let s = format!("{}", f);
                            if let Some(dot) = s.find('.') {
                                let dec = s.len() - dot - 1;
                                let mut den = 1i64;
                                for _ in 0..dec {
                                    den *= 10;
                                }
                                let num_s: String = s.chars().filter(|c| *c != '.').collect();
                                if let Ok(num) = num_s.parse::<i64>() {
                                    let g = {
                                        let (mut a, mut b) = (num.abs(), den);
                                        while b != 0 {
                                            let t = b;
                                            b = a % b;
                                            a = t;
                                        }
                                        a.max(1)
                                    };
                                    return Some(Ok(Value::rat_raw(num / g, den / g)));
                                }
                            }
                            return Some(Ok(Value::num(f)));
                        }
                        ValueView::Num(f) => return Some(Ok(Value::num(f))),
                        ValueView::Rat(n, d) if d != 0 && n % d == 0 => {
                            return Some(Ok(Value::int(n / d)));
                        }
                        ValueView::Rat(n, d) => return Some(Ok(Value::rat_raw(n, d))),
                        ValueView::Int(i) => return Some(Ok(Value::int(i))),
                        _ => return Some(Ok(val.clone())),
                    }
                }
                "tai" => {
                    return Some(Ok(attributes
                        .as_map()
                        .get("value")
                        .cloned()
                        .unwrap_or(Value::num(0.0))));
                }
                _ => {}
            }
        }
    }

    // Buf/Blob.Str throws X::Buf::AsStr
    if (method == "Str" || method == "Stringy")
        && let ValueView::Instance { class_name, .. } = target.view()
        && crate::runtime::Interpreter::is_buf_value(target)
    {
        let cn = class_name.resolve();
        let mut err = RuntimeError::new(format!(
            "Cannot use a {cn} as a Str. You can use .decode to convert to Str.",
        ));
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("method".to_string(), Value::str(method.to_string()));
        attrs.insert("payload".to_string(), target.clone());
        err.exception = Some(Box::new(Value::make_instance(
            Symbol::intern("X::Buf::AsStr"),
            attrs,
        )));
        return Some(Err(err));
    }

    // Buf/Blob .values and .list return the byte values as integers
    if (method == "values" || method == "list")
        && let ValueView::Instance { attributes, .. } = target.view()
        && crate::runtime::Interpreter::is_buf_value(target)
    {
        if let Some(ValueView::Array(bytes, ..)) = attributes.as_map().get("bytes").map(Value::view)
        {
            return Some(Ok(Value::array(bytes.to_vec())));
        }
        return Some(Ok(Value::array(Vec::new())));
    }

    // CX::Warn methods: message, resume
    if let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
        && class_name == "CX::Warn"
    {
        match method {
            "message" => {
                return Some(Ok(attributes
                    .as_map()
                    .get("message")
                    .cloned()
                    .unwrap_or(Value::str(String::new()))));
            }
            "resume" => return Some(Err(RuntimeError::resume_signal())),
            "gist" | "Str" => {
                return Some(Ok(attributes
                    .as_map()
                    .get("message")
                    .cloned()
                    .unwrap_or(Value::str(String::new()))));
            }
            _ => {}
        }
    }

    // Distribution methods: meta, Str, gist, defined
    if let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
        && class_name == "Distribution"
    {
        match method {
            "meta" => {
                return Some(Ok(attributes.as_map().get("$!meta").cloned().unwrap_or(
                    Value::hash_with_data(Value::hash_arc(std::collections::HashMap::new())),
                )));
            }
            "Str" | "gist" => {
                return Some(Ok(Value::str(format!("Distribution<{}>", class_name))));
            }
            "defined" => return Some(Ok(Value::TRUE)),
            _ => {}
        }
    }

    // Exception/X:: methods: gist, Str, message
    if let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
    {
        let cn = class_name.resolve();
        if cn == "Exception" || cn.starts_with("X::") || cn.starts_with("CX::") {
            match method {
                "gist" => {
                    let bt = attributes
                        .as_map()
                        .get("backtrace")
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    let append_bt = |msg: String| -> String {
                        if bt.is_empty() {
                            msg
                        } else {
                            format!("{}\n{}", msg, bt)
                        }
                    };
                    if let Some(msg) = attributes.as_map().get("message") {
                        let msg_str = msg.to_string_value();
                        if !msg_str.is_empty() {
                            return Some(Ok(Value::str(append_bt(msg_str))));
                        }
                    }
                    if cn == "Exception" {
                        return Some(Ok(Value::str(append_bt(
                            "Unthrown Exception with no message".to_string(),
                        ))));
                    }
                    if cn == "X::AdHoc" {
                        if let Some(payload) = attributes.as_map().get("payload") {
                            let payload_str = payload.to_string_value();
                            if !payload_str.is_empty() {
                                return Some(Ok(Value::str(append_bt(payload_str))));
                            }
                        }
                        return Some(Ok(Value::str(append_bt("Unexplained error".to_string()))));
                    }
                    // Construct message from typed exception attributes
                    if let Some(formatted) =
                        crate::builtins::exception_message::format_exception_message(
                            &cn,
                            &(attributes).as_map(),
                        )
                    {
                        return Some(Ok(Value::str(append_bt(formatted))));
                    }
                    return Some(Ok(Value::str(append_bt(format!("{} with no message", cn)))));
                }
                "Str" => {
                    if let Some(msg) = attributes.as_map().get("message") {
                        let msg_str = msg.to_string_value();
                        if !msg_str.is_empty() {
                            return Some(Ok(Value::str(msg_str)));
                        }
                    }
                    if cn == "Exception" {
                        return Some(Ok(Value::str(format!("Something went wrong in ({})", cn))));
                    }
                    // X::AdHoc carries its text in `payload`, not `message`
                    // (`die "..."` builds one). `.Str` returns that payload,
                    // mirroring `.gist`/`.message`.
                    if cn == "X::AdHoc" {
                        if let Some(payload) = attributes.as_map().get("payload") {
                            let payload_str = payload.to_string_value();
                            if !payload_str.is_empty() {
                                return Some(Ok(Value::str(payload_str)));
                            }
                        }
                        return Some(Ok(Value::str("Unexplained error".to_string())));
                    }
                    // Construct message from typed exception attributes
                    if let Some(formatted) =
                        crate::builtins::exception_message::format_exception_message(
                            &cn,
                            &(attributes).as_map(),
                        )
                    {
                        return Some(Ok(Value::str(formatted)));
                    }
                    return Some(Ok(Value::str(format!("{} with no message", cn))));
                }
                "message" => {
                    if let Some(msg) = attributes.as_map().get("message") {
                        return Some(Ok(msg.clone()));
                    }
                    if cn == "X::AdHoc"
                        && let Some(payload) = attributes.as_map().get("payload")
                    {
                        return Some(Ok(payload.clone()));
                    }
                    // Construct message from typed exception attributes
                    if let Some(formatted) =
                        crate::builtins::exception_message::format_exception_message(
                            &cn,
                            &(attributes).as_map(),
                        )
                    {
                        return Some(Ok(Value::str(formatted)));
                    }
                    return Some(Ok(Value::str(String::new())));
                }
                "line" => {
                    if let Some(line) = attributes.as_map().get("line") {
                        return Some(Ok(line.clone()));
                    }
                    return Some(Ok(Value::NIL));
                }
                "file" => {
                    if let Some(file) = attributes.as_map().get("file") {
                        return Some(Ok(file.clone()));
                    }
                    return Some(Ok(Value::NIL));
                }
                "backtrace" => {
                    if let Some(bt) = attributes.as_map().get("backtrace") {
                        return Some(Ok(bt.clone()));
                    }
                    return Some(Ok(Value::str(String::new())));
                }
                _ => {}
            }
        }
    }

    // Backtrace methods: .Str, .gist, .list, .elems
    if let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
    {
        let cn = class_name.resolve();
        if cn == "Backtrace" {
            match method {
                "Str" | "Stringy" => {
                    let text = attributes
                        .as_map()
                        .get("text")
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    return Some(Ok(Value::str(text)));
                }
                "gist" => {
                    let count = attributes
                        .as_map()
                        .get("frames")
                        .map(|v| crate::runtime::utils::value_to_list(v).len())
                        .unwrap_or(0);
                    let noun = if count == 1 { "frame" } else { "frames" };
                    return Some(Ok(Value::str(format!("Backtrace({} {})", count, noun))));
                }
                "full" => {
                    // .full renders every frame (mutsu tracks no hidden/setting
                    // frames, so this is the frame list verbatim).
                    let frames = attributes
                        .as_map()
                        .get("frames")
                        .map(crate::runtime::utils::value_to_list)
                        .unwrap_or_default();
                    let mut out = String::new();
                    for frame in &frames {
                        if let ValueView::Instance { attributes: fa, .. } = frame.view() {
                            out.push_str(&backtrace_frame_str(&fa));
                        }
                    }
                    return Some(Ok(Value::str(out)));
                }
                "list" | "List" | "flat" | "Seq" => {
                    if let Some(frames) = attributes.as_map().get("frames") {
                        return Some(Ok(frames.clone()));
                    }
                    return Some(Ok(Value::array(vec![])));
                }
                "concise" | "summary" => {
                    let frames = attributes
                        .as_map()
                        .get("frames")
                        .map(crate::runtime::utils::value_to_list)
                        .unwrap_or_default();
                    let want_summary = method == "summary";
                    let mut out = String::new();
                    for frame in &frames {
                        if let ValueView::Instance { attributes: fa, .. } = frame.view() {
                            let is_routine = backtrace_frame_is_routine(&fa);
                            // is-hidden / is-setting are always false here (mutsu
                            // does not track hidden or CORE-setting frames).
                            // concise: only non-hidden, non-setting routines.
                            // summary: non-hidden items that are routines or
                            // non-setting (i.e. everything here).
                            let keep = if want_summary { true } else { is_routine };
                            if keep {
                                out.push_str(&backtrace_frame_str(&fa));
                            }
                        }
                    }
                    return Some(Ok(Value::str(out)));
                }
                "elems" => {
                    if let Some(frames) = attributes.as_map().get("frames") {
                        let count = crate::runtime::utils::value_to_list(frames).len();
                        return Some(Ok(Value::int(count as i64)));
                    }
                    return Some(Ok(Value::int(0)));
                }
                _ => {}
            }
        } else if cn == "Backtrace::Frame" {
            match method {
                "subname" => {
                    return Some(Ok(attributes
                        .as_map()
                        .get("subname")
                        .cloned()
                        .unwrap_or(Value::str(String::new()))));
                }
                "file" => {
                    return Some(Ok(attributes
                        .as_map()
                        .get("file")
                        .cloned()
                        .unwrap_or(Value::str(String::new()))));
                }
                "line" => {
                    return Some(Ok(attributes
                        .as_map()
                        .get("line")
                        .cloned()
                        .unwrap_or(Value::int(0))));
                }
                "Str" | "gist" => {
                    return Some(Ok(Value::str(backtrace_frame_str(&attributes))));
                }
                "code" => {
                    // The Code object for this frame. mutsu does not retain the
                    // actual routine, so synthesize a Routine carrying the name
                    // (`.code.name` is the documented use).
                    let subname = attributes
                        .as_map()
                        .get("subname")
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    return Some(Ok(Value::routine_parts(
                        Symbol::intern("GLOBAL"),
                        Symbol::intern(&subname),
                        false,
                    )));
                }
                "name" => {
                    return Some(Ok(attributes
                        .as_map()
                        .get("subname")
                        .cloned()
                        .unwrap_or(Value::str(String::new()))));
                }
                "is-routine" => {
                    return Some(Ok(Value::truth(backtrace_frame_is_routine(&attributes))));
                }
                "is-hidden" | "is-setting" => {
                    // mutsu does not track hidden or CORE-setting frames.
                    return Some(Ok(Value::FALSE));
                }
                _ => {}
            }
        }
    }

    // .resume on exception objects
    if method == "resume"
        && let ValueView::Instance { class_name, .. } = target.view()
    {
        let cn = class_name.resolve();
        if cn == "Exception" || cn.starts_with("X::") || cn == "Failure" || cn == "CX::Warn" {
            return Some(Err(RuntimeError::resume_signal()));
        }
    }

    // .throw on exception objects
    if method == "throw"
        && let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
    {
        let cn = class_name.resolve();
        // Only the core exception classes use the fast path. For CX::* we
        // fall through to the slow path, which can inspect the class's
        // composed roles (e.g. X::Control) to decide whether the throw
        // should raise a control exception.
        if cn == "Exception" || cn.starts_with("X::") || cn == "Failure" {
            // Derive the human message the same way `.message`/`.gist`/`.Str`
            // do — an X::AdHoc carries its text in `payload` (not `message`),
            // and a typed exception's message is built from its attributes —
            // rather than the type repr (`X::AdHoc()`) that
            // `target.to_string_value()` would yield.
            let msg = attributes
                .as_map()
                .get("message")
                .map(|v| v.to_string_value())
                .filter(|s| !s.is_empty())
                .or_else(|| {
                    if cn == "X::AdHoc" {
                        attributes
                            .as_map()
                            .get("payload")
                            .map(|v| v.to_string_value())
                            .filter(|s| !s.is_empty())
                    } else {
                        None
                    }
                })
                .or_else(|| {
                    crate::builtins::exception_message::format_exception_message(
                        &cn,
                        &attributes.as_map(),
                    )
                })
                .unwrap_or_else(|| target.to_string_value());
            let mut err = RuntimeError::new(&msg);
            err.exception = Some(Box::new(target.clone()));
            return Some(Err(err));
        }
    }

    // Array of Match objects: .to/.from/.ast
    if let ValueView::Array(arr, _) = target.view() {
        match method {
            "to" | "pos" => {
                if let Some(last) = arr.last() {
                    return native_method_0arg(last, Symbol::intern(method));
                }
                return Some(Ok(Value::int(0)));
            }
            "from" => {
                if let Some(first) = arr.first() {
                    return native_method_0arg(first, Symbol::intern(method));
                }
                return Some(Ok(Value::int(0)));
            }
            "ast" => {
                if let Some(last) = arr.last() {
                    return native_method_0arg(last, Symbol::intern("ast"));
                }
                return Some(Ok(Value::NIL));
            }
            _ => {}
        }
    }

    // IO::Path::Parts — .hash returns attributes as a Hash
    if let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
        && class_name.resolve() == "IO::Path::Parts"
        && (method == "hash" || method == "Hash")
    {
        let map: HashMap<String, Value> = attributes
            .as_map()
            .iter()
            .map(|(k, v)| (k.resolve(), v.clone()))
            .collect();
        return Some(Ok(Value::hash(map)));
    }

    // Match object methods
    if let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
        && class_name == "Match"
    {
        match method {
            "from" => {
                return Some(Ok(attributes
                    .as_map()
                    .get("from")
                    .cloned()
                    .unwrap_or(Value::int(0))));
            }
            "to" | "pos" => {
                return Some(Ok(attributes
                    .as_map()
                    .get("to")
                    .cloned()
                    .unwrap_or(Value::int(0))));
            }
            "gist" => {
                // Full Match gist: corner-quoted text plus positional/named
                // sub-captures, ordered by position and nested recursively.
                let gist = crate::runtime::utils::match_gist(&(attributes).as_map(), 0);
                return Some(Ok(Value::str(gist)));
            }
            "Str" => {
                return Some(Ok(attributes
                    .as_map()
                    .get("str")
                    .cloned()
                    .unwrap_or(Value::str(String::new()))));
            }
            "Bool" => return Some(Ok(Value::TRUE)),
            "orig" => {
                return Some(Ok(attributes
                    .as_map()
                    .get("orig")
                    .cloned()
                    .unwrap_or(Value::str(String::new()))));
            }
            "raku" | "perl" => {
                return Some(Ok(Value::str(match_helpers::match_raku_repr(
                    &(attributes).as_map(),
                ))));
            }
            "list" | "Array" => {
                return Some(Ok(attributes
                    .as_map()
                    .get("list")
                    .cloned()
                    .unwrap_or_else(|| Value::array(Vec::new()))));
            }
            "hash" | "Hash" => {
                return Some(Ok(attributes
                    .as_map()
                    .get("named")
                    .cloned()
                    .unwrap_or_else(|| Value::hash(HashMap::new()))));
            }
            "keys" => {
                let mut keys = Vec::new();
                if let Some(ValueView::Array(list, _)) =
                    attributes.as_map().get("list").map(Value::view)
                {
                    for i in 0..list.len() {
                        keys.push(Value::int(i as i64));
                    }
                }
                if let Some(ValueView::Hash(named)) =
                    attributes.as_map().get("named").map(Value::view)
                {
                    let mut sorted: Vec<&String> = named.keys().collect();
                    sorted.sort();
                    for k in sorted {
                        keys.push(Value::str(k.clone()));
                    }
                }
                return Some(Ok(Value::array(keys)));
            }
            "values" => {
                let mut vals = Vec::new();
                if let Some(ValueView::Array(list, _)) =
                    attributes.as_map().get("list").map(Value::view)
                {
                    vals.extend(list.iter().cloned());
                }
                if let Some(ValueView::Hash(named)) =
                    attributes.as_map().get("named").map(Value::view)
                {
                    let mut sorted: Vec<(&String, &Value)> = named.iter().collect();
                    sorted.sort_by_key(|(k, _)| (*k).clone());
                    for (_, v) in sorted {
                        vals.push(v.clone());
                    }
                }
                return Some(Ok(Value::array(vals)));
            }
            "pairs" => {
                let mut pairs = Vec::new();
                if let Some(ValueView::Array(list, _)) =
                    attributes.as_map().get("list").map(Value::view)
                {
                    for (i, v) in list.iter().enumerate() {
                        pairs.push(Value::pair(i.to_string(), v.clone()));
                    }
                }
                if let Some(ValueView::Hash(named)) =
                    attributes.as_map().get("named").map(Value::view)
                {
                    let mut sorted: Vec<(&String, &Value)> = named.iter().collect();
                    sorted.sort_by_key(|(k, _)| (*k).clone());
                    for (k, v) in sorted {
                        pairs.push(Value::pair(k.clone(), v.clone()));
                    }
                }
                return Some(Ok(Value::array(pairs)));
            }
            "kv" => {
                let mut kv = Vec::new();
                if let Some(ValueView::Array(list, _)) =
                    attributes.as_map().get("list").map(Value::view)
                {
                    for (i, v) in list.iter().enumerate() {
                        kv.push(Value::int(i as i64));
                        kv.push(v.clone());
                    }
                }
                if let Some(ValueView::Hash(named)) =
                    attributes.as_map().get("named").map(Value::view)
                {
                    let mut sorted: Vec<(&String, &Value)> = named.iter().collect();
                    sorted.sort_by_key(|(k, _)| (*k).clone());
                    for (k, v) in sorted {
                        kv.push(Value::str(k.clone()));
                        kv.push(v.clone());
                    }
                }
                return Some(Ok(Value::array(kv)));
            }
            "elems" => {
                let count = match attributes.as_map().get("list").map(Value::view) {
                    Some(ValueView::Array(list, _)) => list.len(),
                    _ => 0,
                };
                return Some(Ok(Value::int(count as i64)));
            }
            "ast" | "made" => {
                return Some(Ok(attributes
                    .as_map()
                    .get("ast")
                    .cloned()
                    .unwrap_or(Value::NIL)));
            }
            "prematch" => {
                if let Some(orig_val) = attributes.as_map().get("orig") {
                    let orig = orig_val.to_string_value();
                    let from = match attributes.as_map().get("from").map(Value::view) {
                        Some(ValueView::Int(n)) => n as usize,
                        _ => 0,
                    };
                    let chars: Vec<char> = orig.chars().collect();
                    let pre: String = chars[..from.min(chars.len())].iter().collect();
                    return Some(Ok(Value::str(pre)));
                }
                return Some(Ok(Value::str(String::new())));
            }
            "postmatch" => {
                if let Some(orig_val) = attributes.as_map().get("orig") {
                    let orig = orig_val.to_string_value();
                    let to = match attributes.as_map().get("to").map(Value::view) {
                        Some(ValueView::Int(n)) => n as usize,
                        _ => 0,
                    };
                    let chars: Vec<char> = orig.chars().collect();
                    let post: String = chars[to.min(chars.len())..].iter().collect();
                    return Some(Ok(Value::str(post)));
                }
                return Some(Ok(Value::str(String::new())));
            }
            "actions" => {
                return Some(Ok(attributes
                    .as_map()
                    .get("actions")
                    .cloned()
                    .unwrap_or(Value::NIL)));
            }
            "caps" => {
                return Some(Ok(match_helpers::match_caps(&(attributes).as_map())));
            }
            "chunks" => {
                return Some(Ok(match_helpers::match_chunks(&(attributes).as_map())));
            }
            "Capture" => {
                // Match.Capture returns self
                return Some(Ok(target.clone()));
            }
            "clone" => {
                // A Match clones to a Match — it must NOT delegate to its Str
                // (the Cool `_` coercion below), which would drop the match
                // structure and return the bare matched string. A Match is
                // immutable, so a value clone (sharing the attribute storage) is
                // a correct clone that keeps positional/named captures. (Before
                // scalar `.clone` became a real method this happened to work via
                // `Str.clone` falling through to the slow path.)
                return Some(Ok(target.clone()));
            }
            _ => {
                let str_val = Value::str(target.to_string_value());
                return native_method_0arg(&str_val, Symbol::intern(method));
            }
        }
    }

    // Numeric type object .Range methods
    if let ValueView::Package(name) = target.view()
        && method == "Range"
        && matches!(
            name.resolve().as_str(),
            "Real" | "Num" | "Rational" | "Rat" | "FatRat" | "BigRat"
        )
    {
        return Some(Ok(Value::generic_range(
            Value::num(f64::NEG_INFINITY),
            Value::num(f64::INFINITY),
            false,
            false,
        )));
    }
    if let ValueView::Package(name) = target.view()
        && name.resolve() == "Int"
        && method == "Range"
    {
        return Some(Ok(Value::generic_range(
            Value::num(f64::NEG_INFINITY),
            Value::num(f64::INFINITY),
            true,
            true,
        )));
    }
    if let ValueView::Package(name) = target.view()
        && crate::runtime::native_types::is_native_int_type(&name.resolve())
        && method == "Range"
        && let Some((min_big, max_big)) =
            crate::runtime::native_types::native_int_bounds(&name.resolve())
    {
        let min_i64 = min_big.to_i64();
        let max_i64 = max_big.to_i64();
        if let (Some(min_v), Some(max_v)) = (min_i64, max_i64) {
            return Some(Ok(Value::range(min_v, max_v)));
        } else {
            let min_val = min_i64
                .map(Value::int)
                .unwrap_or_else(|| Value::bigint(min_big));
            let max_val = max_i64
                .map(Value::int)
                .unwrap_or_else(|| Value::bigint(max_big));
            return Some(Ok(Value::generic_range(min_val, max_val, false, false)));
        }
    }
    // .int-bounds on Range values
    // Note: i64 Range variants use i64::MIN/MAX as a sentinel for -Inf/Inf
    // (e.g. `1..Inf`, `1..*`, `-Inf..1`). An Inf endpoint has no integer bound,
    // so `.int-bounds` must throw — matching raku, where both `(1..*).int-bounds`
    // and `(1..Inf).int-bounds` fail with "Cannot determine integer bounds".
    // GenericRange handles true Inf/NaN endpoints below.
    if method == "int-bounds" {
        // The i64::MIN/MAX sentinel marks an OPEN end (`1..Inf`, `1..*`) only
        // when it appears alone: an open-above range has `end == i64::MAX` with
        // a finite start, an open-below range `start == i64::MIN` with a finite
        // end. When BOTH extremes are present the range is the genuine full-i64
        // bound (`int64.Range` is `-9223372036854775808..9223372036854775807`),
        // so it has concrete bounds and must NOT throw. Hence the XOR.
        if let ValueView::Range(s, e)
        | ValueView::RangeExcl(s, e)
        | ValueView::RangeExclStart(s, e)
        | ValueView::RangeExclBoth(s, e) = target.view()
            && ((s == i64::MIN) ^ (e == i64::MAX))
        {
            let range_repr = crate::runtime::utils::gist_value(target);
            return Some(Err(crate::value::RuntimeError::new(format!(
                "Cannot determine integer bounds of {range_repr}"
            ))));
        }
        match target.view() {
            ValueView::Range(start, end) => {
                return Some(Ok(Value::array(vec![Value::int(start), Value::int(end)])));
            }
            ValueView::RangeExcl(start, end) => {
                return Some(Ok(Value::array(vec![
                    Value::int(start),
                    Value::int(end - 1),
                ])));
            }
            ValueView::RangeExclStart(start, end) => {
                return Some(Ok(Value::array(vec![
                    Value::int(start + 1),
                    Value::int(end),
                ])));
            }
            ValueView::RangeExclBoth(start, end) => {
                return Some(Ok(Value::array(vec![
                    Value::int(start + 1),
                    Value::int(end - 1),
                ])));
            }
            ValueView::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                // Check if endpoints contain Inf, -Inf, or NaN — these cannot
                // have integer bounds.
                let has_non_int_endpoint = |v: &Value| -> bool {
                    match v.view() {
                        ValueView::Num(f) => f.is_infinite() || f.is_nan(),
                        ValueView::Str(_) => true,
                        _ => false,
                    }
                };
                if has_non_int_endpoint(start.as_ref()) || has_non_int_endpoint(end.as_ref()) {
                    let range_repr = crate::runtime::utils::gist_value(target);
                    return Some(Err(crate::value::RuntimeError::new(format!(
                        "Cannot determine integer bounds of {range_repr}"
                    ))));
                }
                let s = if excl_start {
                    match start.as_ref().view() {
                        ValueView::Int(n) => Value::int(n + 1),
                        ValueView::BigInt(n) => Value::bigint(n.as_ref() + 1),
                        ValueView::Rat(n, d) => {
                            Value::int(((n as f64 / d as f64).floor() as i64) + 1)
                        }
                        _ => Value::int(start.as_ref().to_f64() as i64 + 1),
                    }
                } else {
                    match start.as_ref().view() {
                        ValueView::Int(_) | ValueView::BigInt(_) => start.as_ref().clone(),
                        ValueView::Rat(n, d) => {
                            let f = n as f64 / d as f64;
                            Value::int(f.ceil() as i64)
                        }
                        _ => Value::int(start.as_ref().to_f64().ceil() as i64),
                    }
                };
                let e = if excl_end {
                    match end.as_ref().view() {
                        ValueView::Int(n) => Value::int(n - 1),
                        ValueView::BigInt(n) => Value::bigint(n.as_ref() - 1),
                        ValueView::Rat(n, d) => {
                            Value::int(((n as f64 / d as f64).ceil() as i64) - 1)
                        }
                        _ => Value::int(end.as_ref().to_f64() as i64 - 1),
                    }
                } else {
                    match end.as_ref().view() {
                        ValueView::Int(_) | ValueView::BigInt(_) => end.as_ref().clone(),
                        ValueView::Rat(n, d) => {
                            let f = n as f64 / d as f64;
                            Value::int(f.floor() as i64)
                        }
                        _ => Value::int(end.as_ref().to_f64().floor() as i64),
                    }
                };
                return Some(Ok(Value::array(vec![s, e])));
            }
            _ => {}
        }
    }
    // Kernel type object methods
    if let ValueView::Package(name) = target.view()
        && name == "Kernel"
        && method == "endian"
    {
        return Some(Ok(Value::enum_parts(
            Symbol::intern("Endian"),
            Symbol::intern(if cfg!(target_endian = "little") {
                "LittleEndian"
            } else {
                "BigEndian"
            }),
            EnumValue::Int(if cfg!(target_endian = "little") { 1 } else { 2 }),
            if cfg!(target_endian = "little") { 1 } else { 2 },
        )));
    }

    // Delegate to sub-dispatch functions.
    // Each returns Option<Option<Result<..>>> where:
    //   None = method not handled, try next
    //   Some(inner) = method matched, return inner
    macro_rules! try_dispatch {
        ($module:ident) => {
            if let Some(result) = $module::dispatch(target, method) {
                return result;
            }
        };
    }

    try_dispatch!(dispatch_core_coerce);
    try_dispatch!(dispatch_core_unicode);
    try_dispatch!(dispatch_core_numeric);
    try_dispatch!(dispatch_core_list);
    try_dispatch!(dispatch_core_str);
    try_dispatch!(dispatch_core_repr);
    try_dispatch!(dispatch_core_range);
    try_dispatch!(dispatch_core_math);

    None
}
