use super::*;
use num_bigint::BigInt as NumBigInt;
use num_traits::{Signed, Zero};

pub(crate) fn is_internal_anon_type_name(name: &str) -> bool {
    name.starts_with("__ANON_") && name.ends_with("__")
}

/// An anonymous `class`/`grammar`/`role` is registered under an internal
/// `__ANON_<KIND>_<N>__` name; Rakudo displays these as `<anon|N>` (the
/// number is arbitrary, only distinct within a run). Returns the display
/// form for those three kinds; anonymous enums (whose type displays as the
/// empty name) and internal non-type `__ANON_*` markers return None.
fn anon_type_display_name(name: &str) -> Option<String> {
    let inner = name.strip_suffix("__")?;
    let n = ["__ANON_CLASS_", "__ANON_GRAMMAR_", "__ANON_ROLE_"]
        .iter()
        .find_map(|prefix| inner.strip_prefix(prefix))?;
    let n: u64 = n.parse().ok()?;
    Some(format!("<anon|{}>", n + 1))
}

/// A lexically-scoped `my class`/`my role` whose bare name collides with an
/// earlier same-named lexical declaration is stored in the registry under a
/// mangled internal name `Foo\u{0}<site-id>` so its instances keep their own
/// identity (see `exec_register_class_op`). The `\u{0}` separator can never
/// appear in a source identifier, so the user-facing name is just everything
/// before it. Anonymous class/grammar/role internal names display as
/// Rakudo's `<anon|N>`. This is a pure function so `display.rs` (which has
/// no interpreter context) can map the name for `.gist`/`.raku`/`say`.
pub(crate) fn user_facing_type_name(name: &str) -> std::borrow::Cow<'_, str> {
    let base = match name.split_once('\u{0}') {
        Some((short, _)) => short,
        None => name,
    };
    match anon_type_display_name(base) {
        Some(display) => std::borrow::Cow::Owned(display),
        None => std::borrow::Cow::Borrowed(base),
    }
}

/// Format a value for display inside a Capture gist.
fn capture_value_gist(v: &Value) -> String {
    match v.view() {
        ValueView::Str(s) => format!("\"{}\"", *s),
        ValueView::Mixin(inner, mixins) => {
            if let Some(str_val) = mixins.get("Str") {
                let str_s = str_val.to_string_value();
                let type_name = match inner.view() {
                    ValueView::Int(_) | ValueView::BigInt(_) => "IntStr",
                    ValueView::Num(_) => "NumStr",
                    _ => "Allomorph",
                };
                format!(
                    "{}.new({}, \"{}\")",
                    type_name,
                    inner.to_string_value(),
                    str_s
                )
            } else {
                v.to_string_value()
            }
        }
        _ => v.to_string_value(),
    }
}

/// Format a Num in scientific notation matching Raku's output (e.g. `1e+40`, `-1e-05`).
fn format_num_scientific(f: f64) -> String {
    // Use Rust's {:e} format and ensure the exponent has an explicit sign
    let s = format!("{:e}", f);
    // Rust produces e.g. "1e40" or "1e-5"; Raku uses "1e+40" and "1e-05"
    if let Some(pos) = s.rfind('e') {
        let (mantissa, exp_part) = s.split_at(pos + 1); // exp_part is e.g. "40" or "-5"
        let exp_with_sign = if let Some(stripped) = exp_part.strip_prefix('-') {
            format!("-{:02}", stripped.parse::<i32>().unwrap_or(0).abs())
        } else {
            format!("+{}", exp_part)
        };
        format!("{}{}", mantissa, exp_with_sign)
    } else {
        s
    }
}

/// Apply tclc (titlecase first char, lowercase rest) to a string.
pub fn tclc_str(s: &str) -> String {
    let mut result = String::new();
    let mut first = true;
    for ch in s.chars() {
        if first {
            result.push_str(&crate::builtins::unicode_titlecase_first(ch));
            first = false;
        } else {
            for c in ch.to_lowercase() {
                result.push(c);
            }
        }
    }
    result
}

/// Apply wordcase to a string: find words matching <ident>+ % <[ - ' ]>
/// and apply tclc to each word. Non-word characters pass through unchanged.
pub fn wordcase_str(s: &str) -> String {
    wordcase_with(s, tclc_str)
}

/// Split `s` into (segment, is_word) runs the way Raku's `wordcase` does: a
/// "word" starts with an alphabetic/underscore character and may join further
/// identifiers via `-`/`'`. Non-word characters are returned as their own
/// (segment, false) runs so callers can rebuild the string verbatim.
pub fn wordcase_segments(s: &str) -> Vec<(String, bool)> {
    let chars: Vec<char> = s.chars().collect();
    let len = chars.len();
    let mut segments = Vec::new();
    let mut i = 0;
    let mut gap_start = 0;
    while i < len {
        if chars[i].is_alphabetic() || chars[i] == '_' {
            if gap_start < i {
                segments.push((chars[gap_start..i].iter().collect(), false));
            }
            let word_start = i;
            // Consume first ident: [alpha|_] \w*
            i += 1;
            while i < len && (chars[i].is_alphanumeric() || chars[i] == '_') {
                i += 1;
            }
            // Try to consume more idents separated by - or '
            loop {
                if i < len
                    && (chars[i] == '-' || chars[i] == '\'')
                    && i + 1 < len
                    && (chars[i + 1].is_alphabetic() || chars[i + 1] == '_')
                {
                    i += 1; // consume separator
                    i += 1; // consume first char of next ident
                    while i < len && (chars[i].is_alphanumeric() || chars[i] == '_') {
                        i += 1;
                    }
                } else {
                    break;
                }
            }
            segments.push((chars[word_start..i].iter().collect(), true));
            gap_start = i;
        } else {
            i += 1;
        }
    }
    if gap_start < len {
        segments.push((chars[gap_start..len].iter().collect(), false));
    }
    segments
}

/// `wordcase_str` with a custom per-word transform.
pub fn wordcase_with(s: &str, mut transform: impl FnMut(&str) -> String) -> String {
    let mut result = String::new();
    for (segment, is_word) in wordcase_segments(s) {
        if is_word {
            result.push_str(&transform(&segment));
        } else {
            result.push_str(&segment);
        }
    }
    result
}

pub fn format_complex(r: f64, i: f64) -> String {
    /// Format a float component of a Complex, preserving the sign of negative zero.
    fn fmt_num(v: f64) -> String {
        if v.is_nan() {
            "NaN".to_string()
        } else if v.is_infinite() {
            if v.is_sign_positive() {
                "Inf".to_string()
            } else {
                "-Inf".to_string()
            }
        } else if v.fract() == 0.0 {
            // Preserve sign of -0.0
            // Check if value fits in i64 range before casting (large floats saturate)
            let abs_v = v.abs();
            if abs_v <= i64::MAX as f64 {
                if v.is_sign_negative() {
                    format!("-{}", (-v) as i64)
                } else {
                    format!("{}", v as i64)
                }
            } else {
                // Large float that doesn't fit in i64: use scientific notation
                format!("{:e}", v)
            }
        } else {
            format!("{}", v)
        }
    }
    // Use \i notation when imaginary part is Inf, -Inf, or NaN
    let imag_special = i.is_infinite() || i.is_nan();
    let suffix = if imag_special { "\\i" } else { "i" };
    if i == 0.0 && !i.is_sign_negative() {
        format!("{}+0i", fmt_num(r))
    } else if i == 0.0 && i.is_sign_negative() {
        format!("{}-0i", fmt_num(r))
    } else if i.is_nan() {
        format!("{}+NaN\\i", fmt_num(r))
    } else if i < 0.0 || (i.is_infinite() && i.is_sign_negative()) {
        format!("{}-{}{}", fmt_num(r), fmt_num(i.abs()), suffix)
    } else {
        format!("{}+{}{}", fmt_num(r), fmt_num(i), suffix)
    }
}

/// Format a rational as its decimal string exactly as Rakudo's `Rational.Str`.
///
/// Rakudo does NOT print the full exact expansion of a terminating decimal:
/// it rounds *every* Rat to a fixed number of fractional digits and strips the
/// trailing zeros. There is no terminating/non-terminating split. The digit
/// count is (see rakudo `src/core.c/Rational.rakumod`):
///   Rat:    |denom| < 100_000 ? 6 : chars(|denom|) + 1
///   FatRat: |denom| < 100_000 ? 6 : chars(|denom|) + chars(whole) + 5
/// then `round(fract * 10^digits)`, left-zero-padded to `digits`, trailing
/// zeros stripped.
fn format_rat_str_bigint(numer: &NumBigInt, denom: &NumBigInt, is_fatrat: bool) -> String {
    // Callers guarantee denom != 0.
    let sign = numer.is_negative() ^ denom.is_negative();
    let n = numer.abs();
    let d = denom.abs();
    let whole = &n / &d;
    let rem = &n % &d; // fract = rem / d, with 0 <= rem < d
    let sign_str = if sign { "-" } else { "" };

    if rem.is_zero() {
        return format!("{}{}", sign_str, whole);
    }

    // Rakudo guards floating-point noise for Rats near an integer: when the
    // exact fractional part coerces to exactly 1.0 as a Num, emit the next Int.
    if !is_fatrat && crate::value::bigrat_to_f64(&rem, &d) == 1.0 {
        return format!("{}{}", sign_str, &whole + 1u8);
    }

    let hundred_k = NumBigInt::from(100_000u32);
    let digits: usize = if d < hundred_k {
        6
    } else if is_fatrat {
        d.to_string().len() + whole.to_string().len() + 5
    } else {
        d.to_string().len() + 1
    };

    // s = round(fract * 10^digits) = round(rem * 10^digits / d), rounding half
    // up (Rakudo `.round` = floor(x + 1/2); rem/d is non-negative).
    let scale = NumBigInt::from(10u8).pow(digits as u32);
    let scaled = &rem * &scale;
    let rounded = (&scaled * 2u8 + &d) / (&d * 2u8);

    let mut s = rounded.to_string();
    // Defensive carry guard: if rounding rolled fract over to 10^digits, the
    // value became the next integer. (Rakudo's digit formula makes this
    // unreachable, but keep the whole part correct if it ever happens.)
    if s.len() > digits {
        return format!("{}{}", sign_str, &whole + 1u8);
    }
    if s.len() < digits {
        s = format!("{}{}", "0".repeat(digits - s.len()), s);
    }
    let trimmed = s.trim_end_matches('0');
    if trimmed.is_empty() {
        format!("{}{}", sign_str, whole)
    } else {
        format!("{}{}.{}", sign_str, whole, trimmed)
    }
}

impl Value {
    pub(crate) fn to_string_value(&self) -> String {
        match self.view() {
            // A `VarRef` is a transient binder wrapper: it stringifies as the
            // variable's value.
            ValueView::VarRef { value, .. } => value.to_string_value(),
            ValueView::RakuAst(node) => crate::rakuast::node_gist(node),
            ValueView::Int(i) => i.to_string(),
            ValueView::BigInt(n) => n.to_string(),
            ValueView::Num(f) => {
                if f.is_nan() {
                    "NaN".to_string()
                } else if f.is_infinite() {
                    if f > 0.0 {
                        "Inf".to_string()
                    } else {
                        "-Inf".to_string()
                    }
                } else if f == 0.0 && f.is_sign_negative() {
                    "-0".to_string()
                } else if f.fract() == 0.0 && f.is_finite() {
                    let abs = f.abs();
                    if abs >= 1e15 || (abs != 0.0 && abs < 1e-4) {
                        // Scientific notation for very large/small integer-valued Nums
                        format_num_scientific(f)
                    } else {
                        format!("{}", f as i64)
                    }
                } else {
                    // Fractional Num: Raku renders in scientific notation once the
                    // magnitude leaves [1e-4, 1e15) — e.g. `1e-5` prints `1e-05`,
                    // not `0.00001`. Rust's `{}` never switches to scientific, so
                    // apply the same threshold as the integer-valued branch.
                    let abs = f.abs();
                    if !(1e-4..1e15).contains(&abs) {
                        format_num_scientific(f)
                    } else {
                        format!("{}", f)
                    }
                }
            }
            ValueView::Str(s) => (**s).clone(),
            ValueView::Bool(true) => "True".to_string(),
            ValueView::Bool(false) => "False".to_string(),
            ValueView::Range(a, b) => {
                // .Str on a finite range iterates elements and joins with spaces.
                // For infinite/very large ranges, fall back to range notation.
                if b.saturating_sub(a) > 1_000_000 || b == i64::MAX || a == i64::MIN {
                    format!("{}..{}", a, b)
                } else {
                    (a..=b).map(|i| i.to_string()).collect::<Vec<_>>().join(" ")
                }
            }
            ValueView::RangeExcl(a, b) => {
                if b.saturating_sub(a) > 1_000_000 || b == i64::MAX || a == i64::MIN {
                    format!("{}..^{}", a, b)
                } else {
                    (a..b).map(|i| i.to_string()).collect::<Vec<_>>().join(" ")
                }
            }
            ValueView::RangeExclStart(a, b) => {
                if b.saturating_sub(a) > 1_000_000 || b == i64::MAX || a == i64::MIN {
                    format!("{}^..{}", a, b)
                } else {
                    (a + 1..=b)
                        .map(|i| i.to_string())
                        .collect::<Vec<_>>()
                        .join(" ")
                }
            }
            ValueView::RangeExclBoth(a, b) => {
                if b.saturating_sub(a) > 1_000_000 || b == i64::MAX || a == i64::MIN {
                    format!("{}^..^{}", a, b)
                } else {
                    (a + 1..b)
                        .map(|i| i.to_string())
                        .collect::<Vec<_>>()
                        .join(" ")
                }
            }
            ValueView::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                let is_infinite = matches!(
                    end.view(),
                    ValueView::Whatever | ValueView::HyperWhatever | ValueView::Sub(_)
                ) || matches!(end.view(), ValueView::Num(n) if n.is_infinite() && n.is_sign_positive());
                if !is_infinite {
                    let items = crate::runtime::utils::value_to_list(self);
                    // `value_to_list` may return `[self.clone()]` for non-expandable ranges
                    // (e.g. NaN endpoints). Avoid recursive `.Str` by only expanding when
                    // the single returned item is not itself a GenericRange.
                    let single_generic_range = items.len() == 1
                        && matches!(
                            items.first().map(Value::view),
                            Some(ValueView::GenericRange { .. })
                        );
                    if !single_generic_range {
                        return items
                            .iter()
                            .map(|v| v.to_str_context())
                            .collect::<Vec<_>>()
                            .join(" ");
                    }
                }
                let start_sep = if excl_start { "^.." } else { ".." };
                let end_sep = if excl_end { "^" } else { "" };
                format!(
                    "{}{}{}{}",
                    start.to_string_value(),
                    start_sep,
                    end_sep,
                    end.to_string_value()
                )
            }
            ValueView::Array(_, crate::value::ArrayKind::Lazy) => "...".to_string(),
            ValueView::Array(items, ..) => {
                // Cycle detection for recursive array structures
                thread_local! {
                    static SEEN_ARR_PTRS: std::cell::RefCell<Vec<usize>> = const { std::cell::RefCell::new(Vec::new()) };
                }
                let ptr = crate::gc::Gc::as_ptr(&items) as usize;
                let is_cycle = SEEN_ARR_PTRS.with(|seen| {
                    let s = seen.borrow();
                    s.contains(&ptr)
                });
                if is_cycle {
                    return "[...]".to_string();
                }
                SEEN_ARR_PTRS.with(|seen| seen.borrow_mut().push(ptr));
                let result = items
                    .iter()
                    .map(|v| v.to_str_context())
                    .collect::<Vec<_>>()
                    .join(" ");
                SEEN_ARR_PTRS.with(|seen| {
                    let mut s = seen.borrow_mut();
                    if let Some(pos) = s.iter().rposition(|p| *p == ptr) {
                        s.remove(pos);
                    }
                });
                result
            }
            ValueView::LazyList(ll) => {
                if ll.is_genuinely_lazy() {
                    // Str/interpolation of a genuinely-lazy list renders `...`
                    // (Rakudo) rather than materializing the sequence.
                    "...".to_string()
                } else {
                    // A fully-realized cached lazy list stringifies like a list.
                    ll.cache
                        .lock()
                        .unwrap()
                        .as_ref()
                        .map_or_else(String::new, |items| {
                            items
                                .iter()
                                .map(|v| v.to_string_value())
                                .collect::<Vec<_>>()
                                .join(" ")
                        })
                }
            }
            ValueView::LazyIoLines { .. } => "(...)".to_string(),
            ValueView::Uni(u) => u.text.clone(),
            ValueView::Hash(items) => {
                // Cycle detection for recursive hash structures
                thread_local! {
                    static SEEN_HASH_PTRS: std::cell::RefCell<Vec<usize>> = const { std::cell::RefCell::new(Vec::new()) };
                }
                let ptr = crate::gc::Gc::as_ptr(&items) as usize;
                let is_cycle = SEEN_HASH_PTRS.with(|seen| {
                    let s = seen.borrow();
                    s.contains(&ptr)
                });
                if is_cycle {
                    return "{...}".to_string();
                }
                SEEN_HASH_PTRS.with(|seen| seen.borrow_mut().push(ptr));
                let mut pairs: Vec<_> = items
                    .iter()
                    .map(|(k, v)| {
                        // Object hashes store `.WHICH` string keys; show the
                        // original typed key (e.g. `1`, not `Int|1`).
                        format!(
                            "{}\t{}",
                            items.typed_key(k).to_string_value(),
                            v.to_string_value()
                        )
                    })
                    .collect();
                pairs.sort();
                SEEN_HASH_PTRS.with(|seen| {
                    let mut s = seen.borrow_mut();
                    if let Some(pos) = s.iter().rposition(|p| *p == ptr) {
                        s.remove(pos);
                    }
                });
                pairs.join("\n")
            }
            ValueView::Rat(n, d) => {
                if d == 0 {
                    if n == 0 {
                        "NaN".to_string()
                    } else if n > 0 {
                        "Inf".to_string()
                    } else {
                        "-Inf".to_string()
                    }
                } else {
                    format_rat_str_bigint(&NumBigInt::from(n), &NumBigInt::from(d), false)
                }
            }
            ValueView::FatRat(a, b) => {
                if b == 0 {
                    if a == 0 {
                        "NaN".to_string()
                    } else if a > 0 {
                        "Inf".to_string()
                    } else {
                        "-Inf".to_string()
                    }
                } else {
                    format_rat_str_bigint(&NumBigInt::from(a), &NumBigInt::from(b), true)
                }
            }
            ValueView::BigRat(n, d) => {
                if d.is_zero() {
                    if n.is_zero() {
                        "NaN".to_string()
                    } else if n.is_positive() {
                        "Inf".to_string()
                    } else {
                        "-Inf".to_string()
                    }
                } else if (n % d).is_zero() {
                    format!("{}", n / d)
                } else if self.is_bigfatrat() {
                    // A FatRat has unlimited precision: apply the FatRat digit
                    // budget over the full big-integer expansion (`format_rat_str_bigint`
                    // computes the decimal directly, so a huge denominator never
                    // produces `Inf`, unlike an f64 fallback).
                    format_rat_str_bigint(n, d, true)
                } else if d.abs().to_string().len() > 20 {
                    // A plain Rat with an astronomically large denominator (beyond
                    // ~u64 range) is a degenerate Rat: raku stringifies it via its
                    // Num value, so a vanishingly small ratio prints as `0` rather
                    // than a thousand-digit fraction. A denominator that merely
                    // overflows i64 but stays around u64 range still uses the exact
                    // budget below (e.g. `<1/99999999999999999999>`).
                    let val = crate::value::bigrat_to_f64(n, d);
                    Value::Num(val).to_string_value()
                } else {
                    // A big Rat with an in-range denominator follows raku's fixed
                    // digit-budget rounding (`Rational.Str`): `|denom| < 100_000 ?
                    // 6 : chars(|denom|) + 1`. Mirrors the i64 Rat arm above.
                    format_rat_str_bigint(n, d, false)
                }
            }
            ValueView::Complex(r, i) => format_complex(r, i),
            ValueView::Set(s, _) => {
                let mut keys: Vec<&String> = s.iter().collect();
                keys.sort();
                keys.iter()
                    .map(|k| k.as_str())
                    .collect::<Vec<_>>()
                    .join(" ")
            }
            ValueView::Bag(b, _) => {
                let mut keys: Vec<(&String, &num_bigint::BigInt)> = b.iter().collect();
                keys.sort_by_key(|(k, _)| (*k).clone());
                keys.iter()
                    .map(|(k, v)| {
                        if **v == num_bigint::BigInt::from(1) {
                            (*k).clone()
                        } else {
                            format!("{}({})", k, v)
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(" ")
            }
            ValueView::Mix(m, _) => {
                let mut keys: Vec<(&String, &f64)> = m.iter().collect();
                keys.sort_by_key(|(k, _)| (*k).clone());
                keys.iter()
                    .map(|(k, v)| {
                        if (**v - 1.0).abs() < f64::EPSILON {
                            (*k).clone()
                        } else if v.fract() == 0.0 {
                            format!("{}({})", k, **v as i64)
                        } else {
                            format!("{}({})", k, v)
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(" ")
            }
            // Pair `.Str` is `key.Str ~ "\t" ~ value.Str`; an undefined type-object
            // value stringifies to "" in Str context (raku warns + empty), NOT to
            // its `.gist` `(Any)`. Using `to_str_context` here is what makes
            // `(B => Any).Str eq (B => Mu).Str` (both "B\t"), so `is %h<k>:!p,
            // (k => SomeType)` compares equal for any type-object default
            // (S32-hash/adverbs object-hash missing-key defaults).
            ValueView::Pair(k, v) => format!("{}\t{}", k, v.to_str_context()),
            ValueView::ValuePair(k, v) => {
                format!("{}\t{}", k.to_str_context(), v.to_str_context())
            }
            ValueView::Enum { key, .. } => key.resolve(),
            ValueView::CompUnitDepSpec { short_name } => {
                format!("CompUnit::DependencySpecification({})", short_name)
            }
            ValueView::Package(s) => {
                let resolved = s.resolve();
                if is_internal_anon_type_name(&resolved) {
                    "()".to_string()
                } else {
                    format!("({})", user_facing_type_name(&resolved))
                }
            }
            ValueView::ParametricRole {
                base_name,
                type_args,
            } => {
                let args: Vec<String> = type_args.iter().map(|a| a.to_string_value()).collect();
                format!("({}[{}])", base_name, args.join(","))
            }
            // Rakudo: Code stringifies to its bare name (`~&say` is "say",
            // with a coercion warning mutsu does not emit).
            ValueView::Routine { name, .. } => name.resolve(),
            ValueView::Sub(data) => data.name.resolve(),
            ValueView::WeakSub(weak) => match weak.upgrade() {
                Some(data) => data.name.resolve(),
                None => String::new(),
            },
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Backtrace" => attributes
                .as_map()
                .get("text")
                .map(|v: &Value| v.to_string_value())
                .unwrap_or_default(),
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "IO::Path" || class_name.resolve().starts_with("IO::Path::") => {
                attributes
                    .as_map()
                    .get("path")
                    .map(|v: &Value| v.to_string_value())
                    .unwrap_or_else(|| format!("{}()", class_name))
            }
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Exception"
                || class_name.resolve().starts_with("X::")
                || class_name.resolve().starts_with("CX::") =>
            {
                attributes
                    .as_map()
                    .get("message")
                    .map(|v: &Value| v.to_string_value())
                    .unwrap_or_else(|| format!("{}()", class_name))
            }
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "ObjAt" || class_name == "ValueObjAt" => attributes
                .as_map()
                .get("WHICH")
                .map(|v: &Value| v.to_string_value())
                .unwrap_or_else(|| format!("{}()", class_name)),
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Match" => attributes
                .as_map()
                .get("str")
                .map(|v: &Value| v.to_string_value())
                .unwrap_or_default(),
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Proc" => attributes
                .as_map()
                .get("exitcode")
                .map(|v: &Value| v.to_string_value())
                .unwrap_or_else(|| format!("{}()", class_name)),
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) => {
                if let Some(ValueView::Array(bytes, ..)) =
                    attributes.as_map().get("bytes").map(Value::view)
                {
                    if bytes.is_empty() {
                        // An empty Blob/Buf *instance* gists as `Blob:0x<>` (the
                        // hex body is just empty), not `Blob()` — that spelling
                        // is reserved for the type object, which gists as
                        // `(Blob)` via a separate path.
                        format!("{}:0x<>", class_name)
                    } else {
                        // Determine hex width from element size in the class name
                        let cn = class_name.resolve();
                        let hex_width = if cn.contains("64") {
                            16 // 64-bit = 8 bytes = 16 hex chars
                        } else if cn.contains("32") {
                            8
                        } else if cn.contains("16") {
                            4
                        } else {
                            2 // 8-bit (default)
                        };
                        let hex: Vec<String> = bytes
                            .iter()
                            .map(|b| match b.view() {
                                ValueView::Int(i) => {
                                    if hex_width == 16 {
                                        format!("{:016X}", i as u64)
                                    } else if hex_width == 8 {
                                        format!("{:08X}", i as u32)
                                    } else if hex_width == 4 {
                                        format!("{:04X}", i as u16)
                                    } else {
                                        format!("{:02X}", i as u8)
                                    }
                                }
                                _ => "0".repeat(hex_width),
                            })
                            .collect();
                        format!("{}:0x<{}>", class_name, hex.join(" "))
                    }
                } else {
                    format!("{}()", class_name)
                }
            }
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Instant" => {
                let val = attributes
                    .as_map()
                    .get("value")
                    .map(|v| v.to_f64())
                    .unwrap_or(0.0);
                format!("Instant:{}", val)
            }
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Duration" => {
                let val = attributes
                    .as_map()
                    .get("value")
                    .map(|v| v.to_f64())
                    .unwrap_or(0.0);
                if val == val.floor() {
                    format!("{}", val as i64)
                } else {
                    format!("{}", val)
                }
            }
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Signature" => attributes
                .as_map()
                .get("gist")
                .map(|v: &Value| v.to_string_value())
                .unwrap_or_else(|| format!("{}()", class_name)),
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Stash" => attributes
                .as_map()
                .get("name")
                .map(|v: &Value| v.to_string_value())
                .unwrap_or_default(),
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Method"
                || class_name == "Sub"
                || class_name == "Routine"
                || class_name == "Attribute" =>
            {
                attributes
                    .as_map()
                    .get("name")
                    .map(|v: &Value| v.to_string_value())
                    .unwrap_or_else(|| format!("{}()", class_name))
            }
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Format" => attributes
                .as_map()
                .get("format")
                .map(|v: &Value| v.to_string_value())
                .unwrap_or_else(|| format!("{}()", class_name)),
            ValueView::Instance { attributes, .. }
                if attributes.contains_key("year")
                    && attributes.contains_key("month")
                    && attributes.contains_key("day")
                    && !attributes.contains_key("hour") =>
            {
                if let Some(ValueView::Str(s)) = attributes
                    .as_map()
                    .get("__formatter_rendered")
                    .map(Value::view)
                {
                    return s.to_string();
                }
                let (y, m, d) =
                    crate::builtins::methods_0arg::temporal::date_attrs(&(attributes).as_map());
                crate::builtins::methods_0arg::temporal::format_date(y, m, d)
            }
            ValueView::Instance { attributes, .. }
                if attributes.contains_key("year")
                    && attributes.contains_key("month")
                    && attributes.contains_key("day")
                    && attributes.contains_key("hour")
                    && attributes.contains_key("minute")
                    && attributes.contains_key("second")
                    && attributes.contains_key("timezone") =>
            {
                if let Some(ValueView::Str(s)) = attributes
                    .as_map()
                    .get("__formatter_rendered")
                    .map(Value::view)
                {
                    return s.to_string();
                }
                let (y, mo, d, h, mi, s, tz) =
                    crate::builtins::methods_0arg::temporal::datetime_attrs(&(attributes).as_map());
                crate::builtins::methods_0arg::temporal::format_datetime(y, mo, d, h, mi, s, tz)
            }
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Pod::Block::Declarator" => attributes
                .as_map()
                .get("contents")
                .map(|v: &Value| v.to_string_value())
                .unwrap_or_default(),
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "StrDistance" => attributes
                .as_map()
                .get("after")
                .map(|v: &Value| v.to_string_value())
                .unwrap_or_default(),
            ValueView::Instance { class_name, .. } => format!("{}()", class_name),
            ValueView::Junction { kind, values } => {
                let kind_str = match kind {
                    JunctionKind::Any => "any",
                    JunctionKind::All => "all",
                    JunctionKind::One => "one",
                    JunctionKind::None => "none",
                };
                let elems = values
                    .iter()
                    .map(|v| v.to_string_value())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", kind_str, elems)
            }
            ValueView::Regex(pattern) => format!("/{}/", *pattern),
            ValueView::RegexWithAdverbs(a) => {
                let pattern = &a.pattern;
                let global = &a.global;
                let exhaustive = &a.exhaustive;
                let overlap = &a.overlap;
                let repeat = &a.repeat;
                let nth = &a.nth;
                let perl5 = &a.perl5;
                let ignore_case = &a.ignore_case;
                let sigspace = &a.sigspace;
                let samecase = &a.samecase;
                let samespace = &a.samespace;
                let mut prefix = String::new();
                if *ignore_case {
                    prefix.push_str(":i");
                }
                if *sigspace {
                    prefix.push_str(":s");
                }
                if *global {
                    prefix.push_str(":g");
                }
                if *exhaustive {
                    prefix.push_str(":ex");
                }
                if *overlap {
                    prefix.push_str(":ov");
                }
                if let Some(count) = repeat {
                    prefix.push_str(&format!(":x({count})"));
                }
                if let Some(raw) = nth {
                    prefix.push_str(&format!(":nth({raw})"));
                }
                if *perl5 {
                    prefix.push_str(":P5");
                }
                if *samecase {
                    prefix.push_str(":ii");
                }
                if *samespace {
                    prefix.push_str(":ss");
                }
                format!("m{prefix}/{pattern}/")
            }
            ValueView::Version { parts, plus, minus } => {
                let s = Self::version_parts_to_string(parts);
                if plus {
                    format!("{}+", s)
                } else if minus {
                    format!("{}-", s)
                } else {
                    s
                }
            }
            ValueView::Seq(items)
            | ValueView::HyperSeq(items)
            | ValueView::RaceSeq(items)
            | ValueView::Slip(items) => items
                .iter()
                .map(|v| v.to_str_context())
                .collect::<Vec<_>>()
                .join(" "),
            ValueView::Promise(p) => format!("Promise({})", p.status()),
            ValueView::Channel(_) => "Channel".to_string(),
            ValueView::Nil => String::new(),
            ValueView::Whatever => "*".to_string(),
            ValueView::HyperWhatever => "**".to_string(),
            ValueView::Capture { positional, named } => {
                let mut parts = Vec::new();
                for v in positional.iter() {
                    match v.view() {
                        ValueView::Str(s) => parts.push(format!("\"{}\"", *s)),
                        _ => parts.push(v.to_string_value()),
                    }
                }
                let mut named_entries: Vec<_> = named.iter().collect();
                named_entries.sort_by_key(|(k, _)| (*k).clone());
                for (k, v) in named_entries {
                    if let ValueView::Bool(true) = v.view() {
                        parts.push(format!(":{}(Bool::True)", k));
                    } else if let ValueView::Bool(false) = v.view() {
                        parts.push(format!(":{}(Bool::False)", k));
                    } else {
                        parts.push(format!(":{}({})", k, capture_value_gist(v)));
                    }
                }
                format!("\\({})", parts.join(", "))
            }
            ValueView::Mixin(inner, mixins) => {
                if let Some(str_val) = mixins.get("Str") {
                    str_val.to_string_value()
                } else if let (ValueView::Bool(_), Some(ValueView::Bool(b))) =
                    (inner.view(), mixins.get("Bool").map(Value::view))
                {
                    // `True but False`: Bool stringifies from the effective
                    // boolean (`self ?? 'True' !! 'False'`), so follow `.Bool`.
                    if b { "True" } else { "False" }.to_string()
                } else {
                    inner.to_string_value()
                }
            }
            ValueView::Proxy { .. } => "Proxy".to_string(),
            ValueView::CustomType(c) => {
                if c.name.is_empty() {
                    "(CustomType)".to_string()
                } else {
                    format!("({})", c.name)
                }
            }
            ValueView::CustomTypeInstance(d) => format!("{}()", d.type_name),
            ValueView::Scalar(inner) => inner.to_string_value(),
            ValueView::ContainerRef(_) => self.with_deref(Value::to_string_value),
            ValueView::LazyThunk(thunk_data) => {
                // If already forced, display the cached value
                let cache = thunk_data.cache.lock().unwrap();
                if let Some(ref cached) = *cache {
                    cached.to_string_value()
                } else {
                    // Not yet forced — display as a thunk placeholder
                    "lazy(...)".to_string()
                }
            }
            ValueView::HashEntryRef { .. } => self.hash_entry_read().to_string_value(),
        }
    }

    /// Stringify a value in Raku's Str context.
    /// Type objects (Package) become empty string; everything else uses to_string_value.
    pub(crate) fn to_str_context(&self) -> String {
        match self.view() {
            ValueView::Package(_) => String::new(),
            _ => self.to_string_value(),
        }
    }

    pub(crate) fn version_parts_to_string(parts: &[VersionPart]) -> String {
        parts
            .iter()
            .map(|p| match p {
                VersionPart::Num(n) => n.to_string(),
                VersionPart::Str(s) => s.clone(),
                VersionPart::Whatever => "*".to_string(),
            })
            .collect::<Vec<_>>()
            .join(".")
    }

    /// Parse a version string into parts, handling:
    /// - `.`, `-`, `+`, `/` as separators
    /// - `_` as a separator (becomes a separate part if alone)
    /// - Transitions between digits and non-digit/non-separator chars create implicit splits
    /// - Leading zeros stripped from numeric parts
    pub(crate) fn parse_version_string(s: &str) -> (Vec<VersionPart>, bool, bool) {
        let mut plus = false;
        let mut minus = false;
        let mut raw = s;
        if let Some(stripped) = raw.strip_suffix('+') {
            plus = true;
            raw = stripped;
        } else if let Some(stripped) = raw.strip_suffix('-') {
            minus = true;
            raw = stripped;
        }

        let mut parts = Vec::new();
        let mut current = String::new();
        let mut is_digit_run = false;

        for ch in raw.chars() {
            match ch {
                '.' | '-' | '+' | '/' => {
                    if !current.is_empty() {
                        parts.push(Self::make_version_part(&current, is_digit_run));
                        current.clear();
                    }
                    is_digit_run = false;
                }
                '_' => {
                    if !current.is_empty() {
                        parts.push(Self::make_version_part(&current, is_digit_run));
                        current.clear();
                    }
                    // _ by itself is a "whatever" separator part
                    parts.push(VersionPart::Str("_".to_string()));
                    is_digit_run = false;
                }
                c if c.is_ascii_digit() => {
                    if !current.is_empty() && !is_digit_run {
                        // Transition from alpha to digit
                        parts.push(Self::make_version_part(&current, false));
                        current.clear();
                    }
                    current.push(c);
                    is_digit_run = true;
                }
                c => {
                    if !current.is_empty() && is_digit_run {
                        // Transition from digit to alpha
                        parts.push(Self::make_version_part(&current, true));
                        current.clear();
                    }
                    current.push(c);
                    is_digit_run = false;
                }
            }
        }
        if !current.is_empty() {
            parts.push(Self::make_version_part(&current, is_digit_run));
        }

        if parts.is_empty() {
            parts.push(VersionPart::Num(0));
        }

        (parts, plus, minus)
    }

    fn make_version_part(s: &str, is_digit: bool) -> VersionPart {
        if s == "*" {
            VersionPart::Whatever
        } else if is_digit {
            VersionPart::Num(s.parse::<i64>().unwrap_or(0))
        } else {
            VersionPart::Str(s.to_string())
        }
    }
}
