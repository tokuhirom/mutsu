use crate::value::{ArrayKind, RuntimeError, Value, ValueView};
use num_traits::{Signed, Zero};

use super::range_endpoint_display;

/// Escape a string the way Rakudo's `Str.raku` does: wrap in double quotes,
/// backslash the sigil/interpolation metacharacters, map the named control
/// escapes (`\0 \b \t \n \r`), and render every other control character
/// (Unicode category Cc: U+0000-U+001F, U+007F-U+009F) as `\x[HEX]` with
/// upper-case, no-leading-zero hex. Non-control characters pass through as-is.
pub(crate) fn escape_raku_str(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for c in s.chars() {
        match c {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '$' => out.push_str("\\$"),
            '@' => out.push_str("\\@"),
            '%' => out.push_str("\\%"),
            '&' => out.push_str("\\&"),
            '{' => out.push_str("\\{"),
            '\0' => out.push_str("\\0"),
            '\u{8}' => out.push_str("\\b"),
            '\t' => out.push_str("\\t"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            c if c.is_control() => out.push_str(&format!("\\x[{:X}]", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

/// Format a BigRat with a terminating decimal as an exact decimal string.
/// Assumes denominator is a power of 2 and 5 (verified by caller).
fn format_bigrat_decimal_exact(n: &num_bigint::BigInt, d: &num_bigint::BigInt) -> String {
    let negative = n.is_negative() ^ d.is_negative();
    let n_abs = n.abs();
    let d_abs = d.abs();
    let quotient = &n_abs / &d_abs;
    let remainder = &n_abs % &d_abs;
    if remainder.is_zero() {
        let prefix = if negative { "-" } else { "" };
        return format!("{}{}.0", prefix, quotient);
    }
    // Compute fractional digits by multiplying remainder by 10 repeatedly
    let mut frac_digits = String::new();
    let mut rem = remainder;
    let ten = num_bigint::BigInt::from(10u8);
    while !rem.is_zero() {
        rem *= &ten;
        let digit = &rem / &d_abs;
        rem %= &d_abs;
        frac_digits.push_str(&digit.to_string());
    }
    let prefix = if negative { "-" } else { "" };
    format!("{}{}.{}", prefix, quotient, frac_digits)
}

/// Format a finite f64 in Raku's Num.raku style: always includes 'e'.
/// Raku's approach: use the shortest natural string representation,
/// then ensure 'e' is present (append 'e0' if not).
/// Examples: 42e0 → "42e0", 1.5e0 → "1.5e0", 0.1e0 → "0.1e0",
///           2.026887777243374e-48 → "2.026887777243374e-48"
pub(crate) fn format_num_raku(f: f64) -> String {
    if f == 0.0 {
        return if f.is_sign_negative() {
            "-0e0".to_string()
        } else {
            "0e0".to_string()
        };
    }

    // Use format_num_str to get the natural Raku string form,
    // then ensure 'e' is present.
    let s = format_num_str(f);
    if s.contains('e') || s.contains('E') {
        s.replace('E', "e")
    } else {
        format!("{}e0", s)
    }
}

/// Format an f64 as Raku's Num.Str would: shortest representation that
/// round-trips, using scientific notation only for very large/small values.
pub(crate) fn format_num_str(f: f64) -> String {
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
    if f == 0.0 {
        return if f.is_sign_negative() {
            "-0".to_string()
        } else {
            "0".to_string()
        };
    }
    let abs_f = f.abs();
    let sign = if f < 0.0 { "-" } else { "" };

    // For numbers that are naturally expressible without scientific notation
    // (the half-open magnitude window [1e-4, 1e15)), use fixed-point
    // representation. For others, use scientific notation — matching `Num.Str`,
    // so `1e15` prints `1e+15` while `9e14` stays `900000000000000`.
    if (1e-4..1e15).contains(&abs_f) {
        // Find shortest fixed-point round-trip representation
        for prec in 0..=20 {
            let candidate = format!("{sign}{abs_f:.prec$}");
            if let Ok(parsed) = candidate.parse::<f64>()
                && parsed == f
            {
                // Trim trailing zeros after decimal point, but keep at least the integer
                if candidate.contains('.') {
                    let trimmed = candidate.trim_end_matches('0').trim_end_matches('.');
                    return trimmed.to_string();
                }
                return candidate;
            }
        }
    }

    // Use scientific notation for very large/small numbers
    // Find shortest round-trip in scientific notation
    for prec in 0..=17 {
        let candidate = format!("{:.prec$e}", abs_f);
        if let Ok(parsed) = candidate.parse::<f64>()
            && parsed == abs_f
        {
            // Clean up: trim trailing zeros in mantissa
            let e_pos = candidate.find('e').unwrap();
            let mantissa = &candidate[..e_pos];
            let exp_str = &candidate[e_pos + 1..];
            let mantissa = if mantissa.contains('.') {
                mantissa.trim_end_matches('0').trim_end_matches('.')
            } else {
                mantissa
            };
            // Raku always writes the exponent with an explicit sign and a
            // 2-digit minimum (`1e-05`, `1e+20`, `1e-100`), matching `Num.Str`.
            let exp_num: i32 = exp_str.parse().unwrap_or(0);
            let exp_fmt = if exp_num < 0 {
                format!("-{:02}", exp_num.unsigned_abs())
            } else {
                format!("+{:02}", exp_num)
            };
            return format!("{sign}{mantissa}e{exp_fmt}");
        }
    }

    // Fallback: full precision
    format!("{}", f)
}

/// Helper for .raku representation of a value
fn is_self_array_ref_marker(v: &Value) -> bool {
    matches!(v.view(), ValueView::Pair(name, _) if name == "__mutsu_self_array_ref")
}

/// Whether a string key may be rendered in the adverbial pair form
/// `:key(value)`. Mirrors Raku's `<.ident>`: the key must start with a letter
/// or `_`, continue with word chars, and may contain `-`/`'` only when each is
/// immediately followed by another word char. A digit-leading key (`"1"`), a
/// dotted key (`"1.5"`), or a key with a trailing/doubled `-`/`'` (`"x-"`,
/// `"a--b"`) is NOT an identifier and renders as `"key" => value`.
pub(crate) fn is_adverbial_pair_key(s: &str) -> bool {
    let mut chars = s.chars().peekable();
    match chars.next() {
        Some(c) if c.is_alphabetic() || c == '_' => {}
        _ => return false,
    }
    while let Some(c) = chars.next() {
        if c.is_alphanumeric() || c == '_' {
            continue;
        }
        if c == '-' || c == '\'' {
            match chars.peek() {
                Some(&n) if n.is_alphanumeric() || n == '_' => continue,
                _ => return false,
            }
        }
        return false;
    }
    true
}

/// Whether a single element warrants a trailing comma when it is the sole
/// element of a real (`@`-sigil) array's `.raku`: `[1..5,]`, `[(1, 2),]`,
/// `[{:x(1)},]`. Raku adds the comma when the element is itself an Iterable
/// whose bare `.raku` literal would otherwise flatten/merge into the array
/// (Range, List/Array, Seq, Hash/Map). Pair, Set/Bag/Mix (rendered as
/// constructor calls), scalars, and type objects do NOT get a comma.
fn element_needs_trailing_comma(v: &Value) -> bool {
    match v.view() {
        ValueView::ContainerRef(cell) => {
            let inner = cell.lock().unwrap().clone();
            element_needs_trailing_comma(&inner)
        }
        ValueView::Scalar(inner) => element_needs_trailing_comma(inner),
        ValueView::Range(..)
        | ValueView::RangeExcl(..)
        | ValueView::RangeExclStart(..)
        | ValueView::RangeExclBoth(..)
        | ValueView::GenericRange { .. }
        | ValueView::Array(..)
        | ValueView::Seq(..)
        | ValueView::Hash(_) => true,
        _ => false,
    }
}

fn raku_array_wrap_counted(
    inner: &str,
    kind: ArrayKind,
    count: usize,
    single_listy: bool,
) -> String {
    // A real array with a single Iterable element renders with a trailing comma
    // so the round-trip does not flatten it: `[1..5,]`, `$[(1, 2),]`.
    let array_comma = if count == 1 && single_listy { "," } else { "" };
    match kind {
        ArrayKind::Array | ArrayKind::Shaped | ArrayKind::Lazy => {
            format!("[{}{}]", inner, array_comma)
        }
        ArrayKind::List => {
            if count == 1 {
                format!("({},)", inner)
            } else {
                format!("({})", inner)
            }
        }
        ArrayKind::ItemArray => format!("$[{}{}]", inner, array_comma),
        ArrayKind::ItemList => {
            if inner.is_empty() {
                "$( )".to_string()
            } else if count == 1 {
                format!("$({},)", inner)
            } else {
                format!("$({})", inner)
            }
        }
    }
}

fn raku_array_wrap(inner: &str, kind: ArrayKind) -> String {
    // Fallback without count (used for self-referencing snapshot rendering)
    raku_array_wrap_counted(inner, kind, 2, false) // count=2 to avoid trailing comma
}

/// Render a value for `.raku` but strip itemization (Scalar container).
/// In Raku, `@a.raku` renders itemized elements without the `$` prefix:
///   my @a = $[1,2,3]; @a.raku  # [[1, 2, 3],]  (not $[1, 2, 3])
/// Render a value that is stored as a *hash value* for `.raku`/`.perl`.
///
/// In Raku, every hash value lives in a `Scalar` container, so an aggregate
/// (Array/List/Hash/Seq) value is itemized: `{:a($[1, 2])}`, `{:a(${:b(1)})}`,
/// `{:a($(1, 2, 3))}`, `{:a($((1, 2).Seq))}`. Scalars (Int/Str/Range/Pair/Set…)
/// are rendered as-is. Values whose own repr already carries the `$` sigil
/// (e.g. an explicitly itemized `$[1, 2]`) are not double-itemized.
fn raku_hash_value(v: &Value) -> String {
    let base = raku_value(v);
    // Don't double-itemize a value whose repr already carries a sigil: an
    // explicitly itemized `$[...]`, or a cycle-reference placeholder for a
    // recursive structure (`%hash_<ptr>` / `@Array_<ptr>`).
    if base.starts_with(['$', '@', '%']) {
        return base;
    }
    // A `:=`-bound hash element holds a `ContainerRef` cell; itemize based on the
    // aggregate it *holds* so it renders like a plain hash value — `{:a($[1, 2])}`,
    // not `{:a([1, 2])}` (Phase 5 leak hardening, `docs/element-element-bind-plan.md`).
    let effective = match v.view() {
        ValueView::ContainerRef(cell) => cell.lock().unwrap().clone(),
        _ => v.clone(),
    };
    match effective.view() {
        ValueView::Array(..) | ValueView::Hash(_) => {
            // A bracket/paren literal (`{...}`, `[...]`, or the typed-hash
            // `(my Int %{Int} = ...)` form) takes the bare sigil: `${...}`,
            // `$[...]`, `$(...)`. Anything else — a `Map.new(...)` / shaped
            // `Array.new(...)` repr — must be paren-wrapped so the itemization
            // stays valid Raku: `$(Map.new(...))`, not `$Map.new(...)`.
            if base.starts_with(['{', '[', '(']) {
                format!("${base}")
            } else {
                format!("$({base})")
            }
        }
        ValueView::Seq(_) => format!("$({base})"),
        _ => base,
    }
}

fn raku_value_as_element(v: &Value) -> String {
    match v.view() {
        ValueView::Array(items, kind) => {
            let decontainerized = match kind {
                ArrayKind::ItemArray => ArrayKind::Array,
                ArrayKind::ItemList => ArrayKind::List,
                other => other,
            };
            // Re-wrap the value with de-itemized kind for rendering
            let decontainerized_val = Value::array_with_kind(items.clone(), decontainerized);
            raku_value(&decontainerized_val)
        }
        _ => raku_value(v),
    }
}

/// Render array contents for `.raku`, extracted from `raku_value` so that
/// cycle detection can happen before entering the rendering loop.
fn raku_value_array(items: &[Value], kind: ArrayKind, v: &Value) -> String {
    // De-containerize elements only for real arrays (@-sigiled), not for Lists.
    // In Raku, `my @a = $[1,2]; @a.raku` strips the `$` from elements,
    // but `($[1,2], $[3,4]).raku` preserves `$` on itemized elements.
    let is_real_array = kind.is_real_array();
    let render_element = |item: &Value| -> String {
        if is_real_array {
            raku_value_as_element(item)
        } else {
            raku_value(item)
        }
    };

    // Shaped arrays: Array.new(:shape(d1, d2), [row1], [row2])
    // For 1D: Array.new(:shape(N,), [elem1, elem2, ...])
    // Only render shaped prefix for the top-level shaped array, not sub-arrays.
    thread_local! {
        static IN_SHAPED_RAKU: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
    }
    let already_in_shaped = IN_SHAPED_RAKU.with(|f| f.get());
    if !already_in_shaped
        && kind == crate::value::ArrayKind::Shaped
        && let Some(shape) = crate::runtime::utils::shaped_array_shape(v)
        && !shape.is_empty()
    {
        IN_SHAPED_RAKU.with(|f| f.set(true));
        let shape_str = if shape.len() == 1 {
            // Trailing comma for 1D shapes: :shape(5,)
            format!("{},", shape[0])
        } else {
            shape
                .iter()
                .map(|d| d.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        };
        let content = if shape.len() == 1 {
            // 1D shaped array: wrap all elements in a single [...]
            let inner = items
                .iter()
                .map(&render_element)
                .collect::<Vec<_>>()
                .join(", ");
            format!("[{}]", inner)
        } else {
            // Multi-dim: each top-level element is a row
            items
                .iter()
                .map(&render_element)
                .collect::<Vec<_>>()
                .join(", ")
        };
        IN_SHAPED_RAKU.with(|f| f.set(false));
        return format!("Array.new(:shape({}), {})", shape_str, content);
    }
    let snapshot = |k: ArrayKind| {
        let inner = items
            .iter()
            .filter(|item| !is_self_array_ref_marker(item))
            .map(&render_element)
            .collect::<Vec<_>>()
            .join(", ");
        raku_array_wrap(&inner, k)
    };
    let rendered: Vec<_> = items
        .iter()
        .map(|item| {
            if is_self_array_ref_marker(item) {
                snapshot(kind)
            } else {
                render_element(item)
            }
        })
        .collect();
    let count = rendered.len();
    let inner = rendered.join(", ");
    // For a real (`@`-sigil) array, a single Iterable element needs a trailing
    // comma (`[1..5,]`); a self-referential marker element does not.
    let single_listy = count == 1
        && kind.is_real_array()
        && !is_self_array_ref_marker(&items[0])
        && element_needs_trailing_comma(&items[0]);
    raku_array_wrap_counted(&inner, kind, count, single_listy)
}

pub fn raku_value(v: &Value) -> String {
    // Cycle detection for recursive data structures: track Arc pointers
    // that we're currently rendering. If we encounter the same pointer
    // again, emit a placeholder instead of recursing infinitely.
    thread_local! {
        static SEEN_PTRS: std::cell::RefCell<Vec<(usize, String)>> = const { std::cell::RefCell::new(Vec::new()) };
        static ARRAY_CYCLE_FOUND: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
    }
    match v.view() {
        // A `:=`-bound element holds a `ContainerRef` cell; render the held
        // value so a bound element inside a hash/array renders like a plain one
        // (Phase 5 leak hardening).
        ValueView::ContainerRef(cell) => {
            let inner = cell.lock().unwrap().clone();
            raku_value(&inner)
        }
        ValueView::Array(items, kind) => {
            // Lazy arrays should not be materialized
            if kind == crate::value::ArrayKind::Lazy {
                return "[...]".to_string();
            }
            let ptr = crate::gc::Gc::as_ptr(&items) as usize;
            let var_name = format!("@Array_{}", ptr);
            // Check for cycle
            let cycle_var = SEEN_PTRS.with(|seen| {
                seen.borrow()
                    .iter()
                    .find(|(p, _)| *p == ptr)
                    .map(|(_, name)| name.clone())
            });
            if let Some(name) = cycle_var {
                ARRAY_CYCLE_FOUND.with(|f| f.set(true));
                return name;
            }
            let is_top = SEEN_PTRS.with(|seen| seen.borrow().is_empty());
            SEEN_PTRS.with(|seen| seen.borrow_mut().push((ptr, var_name.clone())));
            if is_top {
                ARRAY_CYCLE_FOUND.with(|f| f.set(false));
            }
            let result = raku_value_array(&items, kind, v);
            let had_cycle = ARRAY_CYCLE_FOUND.with(|f| f.get());
            SEEN_PTRS.with(|seen| {
                let mut s = seen.borrow_mut();
                if let Some(pos) = s.iter().rposition(|(p, _)| *p == ptr) {
                    s.remove(pos);
                }
            });
            if is_top && had_cycle {
                format!("((my {}) = {})", var_name, result)
            } else {
                result
            }
        }
        ValueView::Seq(items) => {
            // A consumed Seq is represented as Seq.new() so that EVALing it
            // produces a pre-consumed Seq (matching Raku's behavior).
            if crate::value::seq_is_consumed(&items) {
                return "Seq.new()".to_string();
            }
            let inner = items.iter().map(raku_value).collect::<Vec<_>>().join(", ");
            if items.len() == 1 {
                format!("({},).Seq", inner)
            } else {
                format!("({}).Seq", inner)
            }
        }
        ValueView::Slip(items) => {
            let inner = items.iter().map(raku_value).collect::<Vec<_>>().join(", ");
            format!("slip({})", inner)
        }
        ValueView::Str(s) => escape_raku_str(&s),
        ValueView::Int(i) => i.to_string(),
        ValueView::Rat(n, d) => {
            if d == 0 {
                if n == 0 {
                    "NaN".to_string()
                } else if n > 0 {
                    "Inf".to_string()
                } else {
                    "-Inf".to_string()
                }
            } else if n % d == 0 {
                format!("{}.0", n / d)
            } else {
                // Non-integer rat: check if it's a simple decimal
                let whole = n as f64 / d as f64;
                let mut dd = d.abs();
                while dd % 2 == 0 {
                    dd /= 2;
                }
                while dd % 5 == 0 {
                    dd /= 5;
                }
                if dd == 1 {
                    let s = format!("{}", whole);
                    if s.contains('.') {
                        s
                    } else {
                        format!("{}.0", whole)
                    }
                } else {
                    format!("<{}/{}>", n, d)
                }
            }
        }
        ValueView::BigRat(n, d) => {
            if d == &num_bigint::BigInt::from(0) {
                if n == &num_bigint::BigInt::from(0) {
                    "NaN".to_string()
                } else if n > &num_bigint::BigInt::from(0) {
                    "Inf".to_string()
                } else {
                    "-Inf".to_string()
                }
            } else if (n % d) == num_bigint::BigInt::from(0) {
                format!("{}.0", n / d)
            } else {
                // If denominator doesn't fit in i64, use fraction notation
                // (matches Raku behavior: standard Rats with uint64 denominator
                // get decimal form, but larger denominators get fraction form).
                use num_traits::ToPrimitive;
                if d.abs().to_i64().is_none() {
                    return format!("<{}/{}>", n, d);
                }
                // Check if denominator is a power of 2 and 5 (terminating decimal)
                let mut dd = d.abs();
                while (&dd % 2u8) == num_bigint::BigInt::from(0) {
                    dd /= 2u8;
                }
                while (&dd % 5u8) == num_bigint::BigInt::from(0) {
                    dd /= 5u8;
                }
                if dd == num_bigint::BigInt::from(1u8) {
                    // Terminating decimal: compute exact representation using BigInt
                    format_bigrat_decimal_exact(n, d)
                } else {
                    format!("<{}/{}>", n, d)
                }
            }
        }
        ValueView::FatRat(n, d) => format!("FatRat.new({}, {})", n, d),
        ValueView::Bool(b) => if b { "Bool::True" } else { "Bool::False" }.to_string(),
        ValueView::Num(f) => {
            if f.is_nan() {
                "NaN".to_string()
            } else if f.is_infinite() {
                if f > 0.0 {
                    "Inf".to_string()
                } else {
                    "-Inf".to_string()
                }
            } else {
                // Num.raku must include 'e' so it round-trips as Num, not Rat.
                // Use Raku's format: significand + 'e' + exponent.
                format_num_raku(f)
            }
        }
        ValueView::Complex(r, i) => format!("<{}>", crate::value::format_complex(r, i)),
        ValueView::Pair(key, value) => {
            let ident_like = is_adverbial_pair_key(key);
            if ident_like {
                match value.view() {
                    ValueView::Bool(true) => format!(":{}", key),
                    ValueView::Bool(false) => format!(":!{}", key),
                    _ => format!(":{}({})", key, raku_value(value)),
                }
            } else {
                format!(
                    "{} => {}",
                    raku_value(&Value::str(key.clone())),
                    raku_value(value)
                )
            }
        }
        ValueView::ValuePair(key, value) => {
            if let ValueView::Str(key_str) = key.view() {
                let ident_like = is_adverbial_pair_key(&key_str);
                if ident_like {
                    return match value.view() {
                        ValueView::Bool(true) => format!(":{}", *key_str),
                        ValueView::Bool(false) => format!(":!{}", *key_str),
                        _ => format!(":{}({})", *key_str, raku_value(value)),
                    };
                }
            }
            let key_repr = match key.view() {
                // Non-string keys that would be ambiguous as barewords need parens.
                // A Bool key is NOT wrapped: `Bool::True` already renders
                // unambiguously (raku prints `Bool::True => "a"`, not `(...)`).
                ValueView::Pair(_, _)
                | ValueView::ValuePair(_, _)
                | ValueView::Package(_)
                | ValueView::Nil => {
                    format!("({})", raku_value(key))
                }
                _ => raku_value(key),
            };
            format!("{} => {}", key_repr, raku_value(value))
        }
        ValueView::Hash(map) => {
            // A `$`-scalar-itemized hash renders its inner (de-itemized) repr
            // wrapped by the itemization sigil, uniformly across every hash
            // form (`{...}` / typed `(my Int %{Int} = ...)` / `Map.new(...)`):
            // a bracket/paren literal takes the bare `$` (`${...}`, `$[...]`,
            // `$(...)`), anything else is paren-wrapped (`$(Map.new(...))`).
            if v.hash_is_itemized() {
                let base = raku_value(&v.clone().with_hash_itemized(false));
                return if base.starts_with(['{', '[', '(']) {
                    format!("${base}")
                } else {
                    format!("$({base})")
                };
            }
            // An immutable Map renders as `Map.new((:k(v), ...))`.
            if map.declared_type.as_deref() == Some("Map") {
                let mut sorted_keys: Vec<&String> = map.keys().collect();
                sorted_keys.sort();
                let parts: Vec<String> = sorted_keys
                    .iter()
                    .map(|k| {
                        let v = &map[*k];
                        let repr = if v.is_nil() {
                            "Any".to_string()
                        } else {
                            raku_hash_value(v)
                        };
                        let typed = map.typed_key(k);
                        match typed.view() {
                            ValueView::Str(s) if is_adverbial_pair_key(&s) => {
                                format!(":{}({})", *s, repr)
                            }
                            _ => format!("{} => {}", raku_value(&typed), repr),
                        }
                    })
                    .collect();
                return format!("Map.new(({}))", parts.join(","));
            }
            // Cycle detection for recursive hash structures.
            // When a self-referencing hash is found, produce Raku-style output:
            //   ((my %Hash_<ptr>) = {:a(42), :b(%Hash_<ptr>)})
            thread_local! {
                static SEEN_HASH_PTRS: std::cell::RefCell<Vec<(usize, String)>> = const { std::cell::RefCell::new(Vec::new()) };
                static HASH_CYCLE_FOUND: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
            }
            let ptr = crate::gc::Gc::as_ptr(&map) as usize;
            let var_name = format!("%hash_{}", ptr);
            let cycle_var = SEEN_HASH_PTRS.with(|seen| {
                seen.borrow()
                    .iter()
                    .find(|(p, _)| *p == ptr)
                    .map(|(_, name)| name.clone())
            });
            if let Some(name) = cycle_var {
                HASH_CYCLE_FOUND.with(|f| f.set(true));
                return name;
            }
            let is_top = SEEN_HASH_PTRS.with(|seen| seen.borrow().is_empty());
            SEEN_HASH_PTRS.with(|seen| seen.borrow_mut().push((ptr, var_name.clone())));
            if is_top {
                HASH_CYCLE_FOUND.with(|f| f.set(false));
            }
            let mut sorted_keys: Vec<&String> = map.keys().collect();
            sorted_keys.sort();
            let parts: Vec<String> = sorted_keys
                .iter()
                .map(|k| {
                    let v = &map[*k];
                    // Object hashes store `.WHICH` string keys (e.g. `"Int|1"`);
                    // serialize the original typed key. A `Str` typed key keeps
                    // the colon-pair / ident logic; a non-`Str` key (object hash)
                    // uses the general `key => value` form.
                    let typed = map.typed_key(k);
                    let key_str: Option<String> = match typed.view() {
                        ValueView::Str(s) => Some((**s).clone()),
                        _ => None,
                    };
                    let is_ident = key_str.as_deref().is_some_and(is_adverbial_pair_key);
                    if is_ident {
                        // Raku's `.raku` renders every value in the colon-pair
                        // form `:key(value.raku)` — including Bool, which shows
                        // as `:a(Bool::True)`, not the adverbial `:a` / `:!a`.
                        let k = key_str.as_deref().unwrap();
                        let repr = if v.is_nil() {
                            "Any".to_string()
                        } else {
                            raku_hash_value(v)
                        };
                        format!(":{}({})", k, repr)
                    } else {
                        // Non-identifier keys: use "key" => value format
                        let repr = if v.is_nil() {
                            "Any".to_string()
                        } else {
                            raku_hash_value(v)
                        };
                        format!("{} => {}", raku_value(&typed), repr)
                    }
                })
                .collect();
            let had_cycle = HASH_CYCLE_FOUND.with(|f| f.get());
            SEEN_HASH_PTRS.with(|seen| {
                let mut s = seen.borrow_mut();
                if let Some(pos) = s.iter().rposition(|(p, _)| *p == ptr) {
                    s.remove(pos);
                }
            });
            // A typed hash (`my Int %`, `my Int %{Str}`) renders in the
            // `(my ValueType %{KeyType} = ...)` form rather than a bare `{...}`,
            // so `.raku` round-trips its element/key type. Mirrors the slow-path
            // `dispatch_constrained_hash_raku`; kept here so the native fast path
            // and the itemized wrapper (`$(my Int %)`) stay type-aware without an
            // interpreter round-trip. (`Map` is handled above and returns early.)
            if map.value_type.is_some() || map.key_type.is_some() {
                let value_type = map.value_type.as_deref().unwrap_or("Any");
                let key_suffix = match map.key_type.as_deref() {
                    Some(kt) => format!("{{{}}}", kt),
                    None => String::new(),
                };
                let inner = parts.join(", ");
                return if inner.is_empty() {
                    format!("(my {} %{})", value_type, key_suffix)
                } else {
                    format!("(my {} %{} = {})", value_type, key_suffix, inner)
                };
            }
            let hash_repr = format!("{{{}}}", parts.join(", "));
            if is_top && had_cycle {
                format!("((my {}) = {})", var_name, hash_repr)
            } else {
                hash_repr
            }
        }
        ValueView::Nil => "Nil".to_string(),
        ValueView::Package(name) => name.resolve().to_string(),
        ValueView::Range(a, b) => {
            format!(
                "{}..{}",
                range_endpoint_display(a),
                range_endpoint_display(b)
            )
        }
        ValueView::RangeExcl(a, b) => {
            if a == 0 {
                format!("^{}", range_endpoint_display(b))
            } else {
                format!(
                    "{}..^{}",
                    range_endpoint_display(a),
                    range_endpoint_display(b)
                )
            }
        }
        ValueView::RangeExclStart(a, b) => {
            format!(
                "{}^..{}",
                range_endpoint_display(a),
                range_endpoint_display(b)
            )
        }
        ValueView::RangeExclBoth(a, b) => {
            format!(
                "{}^..^{}",
                range_endpoint_display(a),
                range_endpoint_display(b)
            )
        }
        ValueView::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => {
            let start_repr = match start.view() {
                ValueView::Whatever | ValueView::HyperWhatever => "-Inf".to_string(),
                _ => raku_value(start),
            };
            let end_repr = match end.view() {
                ValueView::Whatever | ValueView::HyperWhatever => "Inf".to_string(),
                _ => raku_value(end),
            };
            let start_sep = if excl_start { "^.." } else { ".." };
            let end_sep = if excl_end { "^" } else { "" };
            format!("{}{}{}{}", start_repr, start_sep, end_sep, end_repr)
        }
        ValueView::Scalar(inner) => {
            // $(expr) — itemized container. `.raku` shows the itemization sigil
            // shaped to the inner value: a Hash → `${...}`, an Array → `$[...]`,
            // a List/Seq → `$(...)`, and a plain scalar (Int/Str/Range/…) renders
            // unwrapped (itemizing an already-scalar value is a no-op:
            // `$(1).raku` → `1`). This is exactly the hash-value itemization rule.
            raku_hash_value(inner)
        }
        ValueView::Capture { positional, named } => {
            let mut parts = Vec::new();
            for v in positional.iter() {
                // Pairs appearing as positional items in a Capture are rendered
                // with quoted key syntax ("key" => value) to distinguish them
                // from named arguments which use colonpair syntax (:key(value)).
                match v.view() {
                    ValueView::Pair(k, val) => {
                        parts.push(format!(
                            "{} => {}",
                            raku_value(&Value::str(k.clone())),
                            raku_value(val)
                        ));
                    }
                    ValueView::ValuePair(k, val) => {
                        parts.push(format!("{} => {}", raku_value(k), raku_value(val)));
                    }
                    _ => parts.push(raku_value(v)),
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
                    parts.push(format!(":{}({})", k, raku_value(v)));
                }
            }
            format!("\\({})", parts.join(", "))
        }
        ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..) => {
            setbagmix_raku(v).unwrap_or_else(|| v.to_string_value())
        }
        // An ObjAt / ValueObjAt (from `.WHICH`) renders its `.raku` as the
        // constructor form `ValueObjAt.new("Int|42")`; only `.gist` / `.Str`
        // show the bare WHICH string (`Int|42`, via `to_string_value`).
        // A Match object embedded in a collection renders via its full
        // `Match.new(...)` form, the same as a direct `$/.raku`. Without this,
        // `$/.list.raku` / `$/.caps.raku` / an array holding a Match stringified
        // the Match to its matched text instead.
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Match" => super::match_helpers::match_raku_repr(&attributes.as_map()),
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "ObjAt" || class_name == "ValueObjAt" => {
            let which = attributes
                .as_map()
                .get("WHICH")
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            format!("{}.new(\"{}\")", class_name.resolve(), which)
        }
        ValueView::Mixin(inner, mixins) => {
            // An allomorphic value (IntStr/RatStr/NumStr/ComplexStr) renders as
            // `TypeStr.new(<numeric>, "<original string>")` — e.g. `<42>.raku`
            // → `IntStr.new(42, "42")`, `<1e3>.raku` → `NumStr.new(1000e0, "1e3")`
            // (the string half preserves the source literal). The numeric half is
            // the inner value's `.raku`. A non-allomorphic mixin falls back to its
            // inner value's `.raku`.
            if let Some(name) = crate::value::types::allomorph_type_name(inner, mixins) {
                let str_repr = mixins
                    .get("Str")
                    .map(raku_value)
                    .unwrap_or_else(|| "\"\"".to_string());
                format!("{}.new({}, {})", name, raku_value(inner), str_repr)
            } else {
                raku_value(inner)
            }
        }
        // A WhateverCode (`*+1`) renders as `WhateverCode.new`, including when it
        // appears as an element of an array/list being `.raku`-rendered.
        ValueView::Sub(data)
            if matches!(
                data.env.get("__mutsu_callable_type").map(Value::view),
                Some(ValueView::Str(kind)) if kind.as_str() == "WhateverCode"
            ) =>
        {
            "WhateverCode.new".to_string()
        }
        _ => v.to_string_value(),
    }
}

/// Render the `.raku`/`.perl` form of a Set/Bag/Mix (and their mutable `*Hash`
/// variants): `Set.new(1,2,3)`, `("a"=>2,"b"=>1).Bag`, `(:a(1.5)=>1).Mix`.
/// Each element is rendered with its original type (via `typed_key`), not the
/// internal string key. Returns `None` for any other value. Shared by
/// `raku_value` (recursive element rendering) and the `.raku` method dispatch
/// so both render identically.
pub(crate) fn setbagmix_raku(v: &Value) -> Option<String> {
    match v.view() {
        // An empty *immutable* Set/Bag/Mix renders via its lowercase coercer
        // (`set()`/`bag()`/`mix()`) in Raku, not the non-empty form. The empty
        // mutable variants keep their non-empty form (`SetHash.new()` /
        // `().BagHash` / `().MixHash`), so only special-case the immutable ones.
        ValueView::Set(s, false) if s.is_empty() => Some("set()".to_string()),
        ValueView::Bag(b, false) if b.is_empty() => Some("bag()".to_string()),
        ValueView::Mix(m, false) if m.is_empty() => Some("mix()".to_string()),
        ValueView::Set(s, mutable) => {
            let type_name = if mutable { "SetHash" } else { "Set" };
            let mut keys: Vec<&String> = s.iter().collect();
            keys.sort();
            let elems = keys
                .iter()
                .map(|k| raku_value(&s.typed_key(k)))
                .collect::<Vec<_>>()
                .join(",");
            Some(format!("{}.new({})", type_name, elems))
        }
        ValueView::Bag(b, mutable) => {
            let type_name = if mutable { "BagHash" } else { "Bag" };
            let mut keys: Vec<(&String, &num_bigint::BigInt)> = b.iter().collect();
            keys.sort_by_key(|(k, _)| (*k).clone());
            let pairs = keys
                .iter()
                .map(|(k, w)| format!("{}=>{}", raku_value(&b.typed_key(k)), w))
                .collect::<Vec<_>>()
                .join(",");
            Some(format!("({}).{}", pairs, type_name))
        }
        ValueView::Mix(m, mutable) => {
            let type_name = if mutable { "MixHash" } else { "Mix" };
            let mut keys: Vec<(&String, &f64)> = m.iter().collect();
            keys.sort_by_key(|(k, _)| (*k).clone());
            let pairs = keys
                .iter()
                .map(|(k, w)| {
                    let w_str = if w.fract() == 0.0 {
                        format!("{}", **w as i64)
                    } else {
                        format!("{}", w)
                    };
                    format!("{}=>{}", raku_value(&m.typed_key(k)), w_str)
                })
                .collect::<Vec<_>>()
                .join(",");
            Some(format!("({}).{}", pairs, type_name))
        }
        _ => None,
    }
}

pub(super) fn hash_pick_item(key: &str, value: &Value) -> Value {
    match key {
        "True" => Value::value_pair(Value::TRUE, value.clone()),
        "False" => Value::value_pair(Value::FALSE, value.clone()),
        _ => {
            if let Ok(n) = key.parse::<f64>() {
                return Value::value_pair(Value::num(n), value.clone());
            }
            Value::pair(key.to_string(), value.clone())
        }
    }
}

/// Coerce a value to a native integer type (e.g. `.byte()`, `.int8()`, `.uint32()`).
/// Wraps out-of-range values using modular arithmetic.
pub(super) fn native_int_coerce_method(
    target: &Value,
    type_name: &str,
) -> Result<Value, RuntimeError> {
    use num_bigint::BigInt as NumBigInt;
    use num_traits::ToPrimitive;

    let big_val: NumBigInt = match target.view() {
        ValueView::Int(i) => NumBigInt::from(i),
        ValueView::BigInt(n) => (**n).clone(),
        ValueView::Num(f) => {
            if f.is_nan() || f.is_infinite() {
                return Err(RuntimeError::new(format!(
                    "Cannot coerce {} to {}",
                    f, type_name
                )));
            }
            NumBigInt::from(f as i128)
        }
        ValueView::Str(s) => {
            if let Ok(i) = s.parse::<i128>() {
                NumBigInt::from(i)
            } else if let Ok(f) = s.parse::<f64>() {
                NumBigInt::from(f as i128)
            } else {
                return Err(RuntimeError::new(format!(
                    "Cannot coerce '{}' to {}",
                    *s, type_name
                )));
            }
        }
        ValueView::Bool(b) => NumBigInt::from(if b { 1 } else { 0 }),
        ValueView::Rat(n, d) => {
            if d == 0 {
                return Err(RuntimeError::new("Division by zero in Rat coercion"));
            }
            NumBigInt::from(n / d)
        }
        _ => {
            // Try to coerce through string -> parse
            let s = target.to_string_value();
            if let Ok(i) = s.parse::<i128>() {
                NumBigInt::from(i)
            } else {
                NumBigInt::from(0)
            }
        }
    };

    let wrapped = crate::runtime::native_types::wrap_native_int(type_name, &big_val);

    // Convert back to Value
    if let Some(i) = wrapped.to_i64() {
        Ok(Value::int(i))
    } else {
        Ok(Value::bigint(wrapped))
    }
}
