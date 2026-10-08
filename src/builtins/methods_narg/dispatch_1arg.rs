use super::allomorph::{allomorph_accepts, out_of_range_failure};
use super::base::{BaseDigits, f64_to_rat, parse_radix_checked, rat_base_repeating, rat_to_base};

use super::flatten::{flatten_target, is_hammer_pair, parse_flat_depth};
use super::fmt_contains::fmt_value_needs_coercion;
use super::indent::str_indent;
use super::numeric::{int_to_subscript, int_to_superscript};
use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{ArrayKind, RuntimeError, Value, ValueView};
use num_traits::ToPrimitive;

/// Whether `arg` is a `Real`, the type of the `Real $epsilon` parameter of
/// `.Rat(eps)` / `.FatRat(eps)`: a number, a `Bool`, an enum value, an allomorph
/// (`<0.01>`), or an object that carries a numeric payload (`Duration`,
/// `Instant`). A `Str`, `Nil`, a type object, a `Complex` and every container
/// are not.
// Cost: O(1).
fn is_real_epsilon(arg: &Value) -> bool {
    match arg.view() {
        ValueView::Num(_)
        | ValueView::Int(_)
        | ValueView::BigInt(_)
        | ValueView::Rat(..)
        | ValueView::BigRat(..)
        | ValueView::FatRat(..)
        | ValueView::Bool(_)
        | ValueView::Enum { .. } => true,
        ValueView::Mixin(inner, _) => is_real_epsilon(inner),
        ValueView::Scalar(inner) => is_real_epsilon(inner),
        // `Duration` and `Instant` keep their number in `value`; an instance of
        // a user subclass of `Int` / `Num` / `Rat` keeps it in a payload.
        ValueView::Instance { class_name, .. } => {
            class_name == "Duration"
                || class_name == "Instant"
                || crate::value::numeric_payload::numeric_subclass_payload(arg).is_some()
        }
        _ => false,
    }
}

/// The epsilon `.Rat(eps)` / `.FatRat(eps)` was bound to. Rakudo's parameter is
/// `Real $epsilon`, so anything else (a `Str`, `Nil`, a type object, a
/// `Complex`) fails the bind with `X::TypeCheck::Binding::Parameter`, as
/// `Num.Rat('0.01')` does there (#12043). `param` is the parameter's name as
/// Rakudo reports it: the `Num` candidates name theirs `epsilon` (`Rat`) and
/// `$epsilon` (`FatRat`), the `Rat` candidates leave it anonymous (`<anon>`).
/// A `Real` numifies; a non-finite one has no usable value and keeps the
/// default `1e-6`.
// Cost: O(1).
fn rat_epsilon_arg(arg: &Value, param: &str) -> Result<f64, RuntimeError> {
    if !is_real_epsilon(arg) {
        return Err(runtime::utils::typecheck_binding_parameter_with_hint(
            param,
            "Real",
            arg,
            &runtime::utils::value_short_repr(arg),
            None,
        ));
    }
    let value = match arg.view() {
        // `Value::to_f64` reads the `Duration` / `Instant` / payload instances.
        ValueView::Instance { .. } => Some(arg.to_f64()),
        _ => crate::runtime::to_float_value(arg),
    };
    Ok(value.filter(|f| f.is_finite()).unwrap_or(1e-6))
}

/// `f.Rat(epsilon)`: the simplest rational within `epsilon` of `f` (a continued
/// fraction), with `NaN` as `0/0` and the infinities as `±1/0`.
// Cost: O(log(1/epsilon)) continued-fraction steps.
fn num_rat_with_epsilon(f: f64, epsilon: f64) -> Value {
    if f.is_nan() {
        Value::rat_raw(0, 0)
    } else if f.is_infinite() {
        Value::rat_raw(if f.is_sign_positive() { 1 } else { -1 }, 0)
    } else {
        crate::builtins::num_to_rat_with_epsilon(f, epsilon)
    }
}

pub(crate) fn native_method_1arg(
    target: &Value,
    method_sym: Symbol,
    arg: &Value,
) -> Option<Result<Value, RuntimeError>> {
    let method = method_sym.resolve();
    let method = method.as_str();

    // Scalar containers are transparent for method dispatch (no .VAR at this arity).
    let target = target.descalarize();
    // An instance of a user subclass of `Int` answers `Int`'s methods on its
    // payload (`builtins::numeric_subclass`).
    if let Some(result) =
        crate::builtins::numeric_subclass::dispatch(target, method_sym, std::slice::from_ref(arg))
    {
        return Some(result);
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
    // Cost: O(n + r), n = subject chars copied and r = replacement chars.
    if method == "replace-with" && target.is_match_instance() {
        if target.match_is_failed() {
            return Some(Ok(Value::NIL));
        }
        let before = target.match_side_text(true).or_else(|| {
            let orig = target.match_orig()?.to_string_value();
            let from = target.match_from()?.max(0) as usize;
            Some(orig.chars().take(from).collect())
        })?;
        let after = target.match_side_text(false).or_else(|| {
            let orig = target.match_orig()?.to_string_value();
            let to = target.match_to()?.max(0) as usize;
            Some(orig.chars().skip(to).collect())
        })?;
        return Some(Ok(Value::str(format!(
            "{}{}{}",
            before,
            arg.to_string_value(),
            after
        ))));
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
        // Cost: O(n), n = chars of the invocant.
        "chop" => {
            // Type objects (Package) should throw
            if let ValueView::Package(type_name) = target.view() {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller chop({}:U)",
                    type_name,
                ))));
            }
            let s = target.to_string_value();
            let n = match arg.view() {
                ValueView::Int(i) => i.max(0) as usize,
                ValueView::BigInt(bi) => {
                    use num_traits::ToPrimitive;
                    bi.to_usize().unwrap_or(usize::MAX)
                }
                // A plain string coerces like `.Int` (parse an integer literal).
                ValueView::Str(_) => arg.to_string_value().parse::<usize>().unwrap_or(1),
                // Any other numeric (Num/Rat/FatRat/allomorph) coerces like `.Int`,
                // truncating toward zero: `.chop(3.6)` chops 3.
                _ => {
                    let f = arg.to_f64();
                    if f.is_finite() && f > 0.0 {
                        f.trunc() as usize
                    } else {
                        0
                    }
                }
            };
            let char_count = s.chars().count();
            let keep = char_count.saturating_sub(n);
            let result: String = s.chars().take(keep).collect();
            Some(Ok(Value::str(result)))
        }
        // The argument forms of the Unicode methods are rows
        // (`method_table::unicode`); these arms keep the `Cool` receivers with no
        // table shape (a `Match`, a `Range`, ...).
        // Cost: O(1), a table lookup.
        "uniprop" => {
            crate::builtins::method_table::unicode::uniprop_of(target, std::slice::from_ref(arg))
        }
        // Cost: O(1), a table lookup.
        "unimatch" => {
            crate::builtins::method_table::unicode::unimatch(target, std::slice::from_ref(arg))
        }
        // Cost: O(n), n = chars of the invocant.
        "uniprops" => {
            crate::builtins::method_table::unicode::uniprops_of(target, std::slice::from_ref(arg))
        }
        // Cost: O(p + m), p = match position, m = chars of the needle (the
        // invocant is borrowed).
        "contains" => {
            if let ValueView::Package(type_name) = arg.view() {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller contains({}:U)",
                    type_name,
                ))));
            }
            // A Regex needle needs the regex engine (&mut self); fall through to
            // the slow path (dispatch_contains) instead of matching its gist.
            if let ValueView::Regex(..) = arg.view() {
                return None;
            }
            // The shared `Str`/`Cool`/`Map` row handler (ADR-11276); it also
            // covers a Junction needle, which the table itself never hands it.
            Some(crate::builtins::method_table::str_search::contains(
                target,
                std::slice::from_ref(arg),
            ))
        }
        // starts-with / ends-with: the plain `.starts-with($needle)` form (a
        // single positional argument) is a pure prefix/suffix check on a Str
        // receiver, so handle it natively here. The case-/mark-insensitive forms
        // (`:i`/`:ignorecase`/`:m`/`:ignoremark`) carry a second (Pair) argument
        // and so never reach this 1-arg path — they keep falling through to the
        // interpreter's `dispatch_prefix_suffix_check` (runtime/methods_string.rs).
        // Cost: O(m), m = chars of the needle (both are borrowed).
        "starts-with" | "ends-with" if matches!(target.view(), ValueView::Str(_)) => {
            if let ValueView::Package(type_name) = arg.view() {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller {}({}:U)",
                    method, type_name
                ))));
            }
            // The `Str` row's handlers (ADR-11276).
            let args = std::slice::from_ref(arg);
            Some(if method == "starts-with" {
                crate::builtins::method_table::str_search::starts_with(target, args)
            } else {
                crate::builtins::method_table::str_search::ends_with(target, args)
            })
        }
        // Cost: O(n + m), n = chars of the invocant, m = chars of the mark source.
        "samemark" => {
            let target_str = target.to_string_value();
            let source_str = arg.to_string_value();
            Some(Ok(Value::str(crate::builtins::samemark_string(
                &target_str,
                &source_str,
            ))))
        }
        // Cost: O(n + m), n = chars of the invocant, m = chars of the case pattern.
        "samecase" => {
            let source_str = target.to_string_value();
            let pattern_str = arg.to_string_value();
            Some(Ok(Value::str(crate::builtins::samecase_string(
                &source_str,
                &pattern_str,
            ))))
        }
        // An allomorph (`IntStr`/`RatStr`/`NumStr`) answers with its numeric
        // inner value, so the epsilon binds exactly where it does for that type.
        // Cost: O(1) plus the inner value's own `Rat`/`FatRat` cost.
        "Rat" | "FatRat"
            if matches!(target.view(), ValueView::Mixin(inner, _)
            if matches!(inner.view(), ValueView::Int(_) | ValueView::Rat(..) | ValueView::Num(_))) =>
        {
            let ValueView::Mixin(inner, _) = target.view() else {
                return None;
            };
            native_method_1arg(inner, method_sym, arg)
        }
        "Rat" => {
            // .Rat(epsilon) — use continued fraction algorithm with given epsilon.
            // Only an invocant that binds the epsilon type-checks it: an `Int`
            // (already rational) never does, so `7.Rat('0.01')` is `7.0`.
            let result = match target.view() {
                ValueView::Rat(_, _) => {
                    if let Err(e) = rat_epsilon_arg(arg, "<anon>") {
                        return Some(Err(e));
                    }
                    target.clone()
                }
                ValueView::Int(i) => Value::rat_raw(i, 1),
                ValueView::Num(f) => match rat_epsilon_arg(arg, "epsilon") {
                    Ok(epsilon) => num_rat_with_epsilon(f, epsilon),
                    Err(e) => return Some(Err(e)),
                },
                ValueView::FatRat(n, d) => Value::rat_raw(n, d),
                // Whether the imaginary part is negligible is judged against
                // `$*TOLERANCE`, which only the interpreter can read
                // (`Interpreter::dispatch_complex_to_real`).
                ValueView::Complex(..) => return None,
                // `Instant` and `Duration` are the rows' (`method_table::instances::instant`).
                ValueView::Instance { class_name, .. }
                    if class_name == "Instant" || class_name == "Duration" =>
                {
                    return None;
                }
                ValueView::Str(s) => {
                    // `Str.Rat` takes no `Real` epsilon in Rakudo; the lenient
                    // reading of whatever was passed is kept.
                    if let Ok(f) = s.parse::<f64>() {
                        let epsilon = rat_epsilon_arg(arg, "epsilon").unwrap_or(1e-6);
                        crate::builtins::num_to_rat_with_epsilon(f, epsilon)
                    } else {
                        Value::rat_raw(0, 1)
                    }
                }
                _ => Value::rat_raw(0, 1),
            };
            Some(Ok(result))
        }
        "FatRat" => {
            // .FatRat(epsilon) — a `Num` is rationalized with the epsilon as
            // `.Rat(epsilon)` does (`3.14159e0.FatRat(0.01)` is `22/7`).
            let result = match target.view() {
                ValueView::FatRat(_, _) => target.clone(),
                ValueView::Int(i) => Value::fat_rat_raw(i, 1),
                ValueView::Rat(n, d) => {
                    if let Err(e) = rat_epsilon_arg(arg, "<anon>") {
                        return Some(Err(e));
                    }
                    Value::fat_rat_raw(n, d)
                }
                ValueView::Num(f) => match rat_epsilon_arg(arg, "$epsilon") {
                    Ok(epsilon) => match num_rat_with_epsilon(f, epsilon).view() {
                        ValueView::Rat(n, d) => Value::fat_rat_raw(n, d),
                        _ => Value::fat_rat_raw(0, 1),
                    },
                    Err(e) => return Some(Err(e)),
                },
                // As for `Rat` above: the interpreter judges the imaginary part.
                ValueView::Complex(..) => return None,
                // `Instant` and `Duration` are the rows' (`method_table::instances::instant`).
                ValueView::Instance { class_name, .. }
                    if class_name == "Instant" || class_name == "Duration" =>
                {
                    return None;
                }
                _ => Value::fat_rat_raw(0, 1),
            };
            Some(Ok(result))
        }
        // Cost: O(p + m) amortized, p = match position, m = chars of the needle;
        // the byte offset is converted through the cached grapheme index.
        "index" => {
            // Fall through to runtime dispatch for type objects, named args (Pairs),
            // array of needles, and multi-arg calls handled by dispatch_index
            if matches!(
                arg.view(),
                ValueView::Package(_) | ValueView::Pair(..) | ValueView::Array(..)
            ) {
                return None;
            }
            // The shared `Str`/`Cool`/`Map` row handler (ADR-11276).
            Some(crate::builtins::method_table::str_search::index(
                target,
                std::slice::from_ref(arg),
            ))
        }
        // Cost: O(k), k = chars returned, once the invocant's grapheme index is
        // cached (see `native_substr_slice`).
        "substr" => crate::builtins::substr::native_substr_slice(target, arg, None),
        // Cost: O(n + L * s), n = chars of the invocant, L = lines, s = |steps|.
        "indent" => {
            let s = target.to_string_value();
            let (result, warning) = match str_indent(&s, arg) {
                Ok(r) => r,
                Err(e) => return Some(Err(e)),
            };
            if let Some(warn_msg) = warning {
                return Some(Err(crate::value::RuntimeError::warn_signal_with_resume(
                    warn_msg,
                    Value::str(result),
                )));
            }
            Some(Ok(Value::str(result)))
        }
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
        // Cost: O(n), n = chars in a string bound/value for comparison and error rendering.
        // The `Range.in-range` row's implementation (`method_table::range`).
        "in-range" => {
            crate::builtins::method_table::range::in_range_value(target, std::slice::from_ref(arg))
        }
        // Cost: see `native_split_method`.
        "split" => {
            if let ValueView::Instance { class_name, .. } = target.view()
                && (class_name == "Supply"
                    || class_name == "IO::Handle"
                    || class_name == "IO::Pipe"
                    || class_name == "IO::CatHandle")
            {
                return None;
            }
            // A `%?RESOURCES` entry splits its content (`resource_split_target`).
            if let ValueView::Instance {
                class_name,
                attributes,
                ..
            } = target.view()
                && class_name == "IO::Path"
                && attributes.contains_key("resource")
            {
                return None;
            }
            // IO::Spec::* has its own split method
            if let ValueView::Package(name) = target.view()
                && name.as_str().starts_with("IO::Spec")
            {
                return None;
            }
            crate::builtins::split::native_split_method(target, std::slice::from_ref(arg))
        }
        "comb" => {
            // Supply/IO targets have their own comb semantics in the interpreter.
            if let ValueView::Instance { class_name, .. } = target.view()
                && (class_name == "Supply"
                    || class_name == "IO::Handle"
                    || class_name == "IO::Path"
                    || class_name == "IO::Pipe"
                    || class_name == "IO::CatHandle")
            {
                return None;
            }
            // Pure Int-chunk / Str-fixed split (shared with the interpreter via
            // builtins::comb); Regex/Sub/bare matchers return None -> interpreter.
            crate::builtins::comb::native_comb_method(target, std::slice::from_ref(arg))
        }
        // Cost: O(1) for the Seq forms, a lazy Seq over the invocant
        // (`crate::value::StrIterSpec`) that `$limit` caps; `:count` is O(n),
        // n = bytes of the invocant, and builds no line.
        "lines" => {
            if let ValueView::Instance { class_name, .. } = target.view()
                && class_name == "Supply"
            {
                return None;
            }
            use crate::value::{StrIterMode, str_iter_seq};
            if let ValueView::Pair(key, value) = arg.view() {
                if key == "chomp" {
                    let mode = StrIterMode::Lines {
                        chomp: value.truthy(),
                    };
                    return Some(Ok(str_iter_seq(target, mode, None)));
                }
                // `.lines(:count)` returns the number of lines instead of the list.
                if key == "count" {
                    let mode = StrIterMode::Lines { chomp: true };
                    if value.truthy() {
                        let n = crate::value::str_iter_count(target, mode, None);
                        return Some(Ok(Value::int(n as i64)));
                    }
                    return Some(Ok(str_iter_seq(target, mode, None)));
                }
                return None;
            }
            let limit = crate::value::str_iter_limit(arg)?;
            Some(Ok(str_iter_seq(
                target,
                StrIterMode::Lines { chomp: true },
                limit,
            )))
        }
        // Cost: O(1), a lazy Seq over the invocant (`crate::value::StrIterSpec`)
        // that `$limit` caps.
        "words" => {
            let limit = crate::value::str_iter_limit(arg)?;
            Some(Ok(crate::value::str_iter_seq(
                target,
                crate::value::StrIterMode::Words,
                limit,
            )))
        }
        // Cost: O(e + t), e = elements of the invocant, t = total chars of the result
        // (each element stringified once, one `join` into a single buffer).
        "join" => {
            if crate::builtins::is_join_lazy(target) {
                return Some(Ok(Value::str("...".to_string())));
            }
            // `.join` stringifies every element, so a zero-denominator Rational
            // among them dies like its own `.Str` (GH #9621).
            if let Err(err) = crate::runtime::utils::check_str_coercion_zero_denominator(target) {
                return Some(Err(err));
            }
            // A Uni/NFC/NFD/NFKC/NFKD value has no itemization wrapper of its
            // own and decomposes into its codepoints in their original
            // (unsorted) order -- matching Rakudo (`'ba'.NFC.join(',')` is
            // `"98,97"`). Same idiom as `.map`/`.grep`/`.sort` (issue #8532).
            if let ValueView::Uni(u) = target.view() {
                let sep = arg.to_string_value();
                let joined = u
                    .codepoints()
                    .iter()
                    .map(|cp| cp.to_string())
                    .collect::<Vec<_>>()
                    .join(&sep);
                return Some(Ok(Value::str(joined)));
            }
            // Shaped arrays: join over leaves
            if crate::runtime::utils::is_shaped_array(target) {
                let leaves = crate::runtime::utils::shaped_array_leaves(target);
                let sep = arg.to_string_value();
                return crate::builtins::method_table::list::join_items(&leaves, &sep).map(Ok);
            }
            // A list's elements (a hole reads as the array's `is default`
            // value): the `List.join` row's implementation (ADR-11276,
            // `method_table::list`).
            if let Some(items) = crate::builtins::method_table::list::join_source_items(target) {
                return crate::builtins::method_table::list::join_items(
                    &items,
                    &arg.to_string_value(),
                )
                .map(Ok);
            }
            match target.view() {
                ValueView::Capture { positional, .. } => {
                    let sep = arg.to_string_value();
                    let joined = positional
                        .iter()
                        .map(Value::to_string_value)
                        .collect::<Vec<_>>()
                        .join(&sep);
                    Some(Ok(Value::str(joined)))
                }
                // Match.join joins the POSITIONAL CAPTURES (`.list`), not the
                // matched string: `($s ~~ /(..)(..)/).join("-")` is "ab-cd",
                // and a captureless match joins to "" (its .list is empty).
                ValueView::Instance { .. } if target.is_match_instance() => {
                    let sep = arg.to_string_value();
                    let items: Vec<Value> = target
                        .match_list()
                        .and_then(|v| v.as_list_items().map(|i| i.to_vec()))
                        .unwrap_or_default();
                    let joined = items
                        .iter()
                        .map(Value::to_str_context)
                        .collect::<Vec<_>>()
                        .join(&sep);
                    Some(Ok(Value::str(joined)))
                }
                // A Pair is a one-element list (`Any.join` is `self.list.join`),
                // so the separator never appears: the result is the Pair's
                // own `.Str`, `key\tvalue`.
                ValueView::Pair(..) | ValueView::ValuePair(..) => {
                    Some(Ok(Value::str(target.to_string_value())))
                }
                ValueView::Hash(map) => {
                    let sep = arg.to_string_value();
                    let joined = map
                        .iter()
                        .map(|(k, v)| format!("{}\t{}", k, v.to_string_value()))
                        .collect::<Vec<_>>()
                        .join(&sep);
                    Some(Ok(Value::str(joined)))
                }
                // Scalar values: .join returns the value as a string
                ValueView::Str(_)
                | ValueView::Int(_)
                | ValueView::Num(_)
                | ValueView::Rat(..)
                | ValueView::Bool(_)
                | ValueView::Instance { .. }
                | ValueView::Nil => Some(Ok(Value::str(target.to_string_value()))),
                // Other types (LazyList, etc.) fall through to the runtime handler
                _ => None,
            }
        }
        "flat" => {
            if is_hammer_pair(arg) {
                return Some(Ok(
                    crate::builtins::method_table::list_transform::flat_hammer(target),
                ));
            }
            if let Some(depth) = parse_flat_depth(arg) {
                return Some(Ok(flatten_target(target, Some(depth), false)));
            }
            None
        }
        // Cost: O(k) on Array/List, k = selected elements; O(e) otherwise,
        // e = receiver elements materialized before selecting a window.
        "head" | "tail" => super::head_tail::dispatch(target, method, arg),
        // Cost: O(e) per call, e = elements of the invocant (snapshotted), then O(k)
        // per combination of size k pulled (`ListGen::Combinations`).
        // The `List.combinations` row's handler (ADR-11276): the cascade
        // answers the receivers the table has no shape for.
        "combinations" => crate::builtins::method_table::list_aggregate::combinations_of(
            target,
            std::slice::from_ref(arg),
        ),
        // Cost: O(1) per call on an Array (lazy, `ListGen::Batch`), O(n) per batch
        // pulled; O(e) on any other invocant, e = elements (decomposed, then chunked
        // eagerly; a lazy invocant throws X::Cannot::Lazy before reaching here).
        "batch" => {
            // `.batch(N)` and the named `.batch(:elems(N))` are equivalent.
            let n = match arg.view() {
                ValueView::Int(i) => i,
                ValueView::Pair(key, val) if key == "elems" || key == "batch" => {
                    val.to_f64() as i64
                }
                _ => return None,
            };
            if n < 1 {
                let message = format!(
                    "Batching sublist length is out of range. Is: {n}, should be in 1..^Inf"
                );
                let mut attrs = std::collections::HashMap::new();
                attrs.insert(
                    "what".to_string(),
                    Value::str_from("Batching sublist length"),
                );
                attrs.insert("got".to_string(), Value::int(n));
                attrs.insert("range".to_string(), Value::str_from("1..^Inf"));
                attrs.insert("message".to_string(), Value::str(message.clone()));
                let ex = Value::make_instance(Symbol::intern("X::OutOfRange"), attrs);
                let mut err = RuntimeError::new(message);
                err.exception = Some(Box::new(ex));
                return Some(Err(err));
            }
            let n = n as usize;
            // An Array batches lazily through a live cursor, as Rakudo's
            // `Rakudo::Iterator.Batch` over the Array's iterator does.
            if let ValueView::Array(_, kind) = target.view()
                && kind != crate::value::ArrayKind::Shaped
            {
                return Some(Ok(Value::seq_list_gen(
                    crate::value::ListGen::batch(target.clone(), n),
                    false,
                )));
            }
            // A Blob/Buf batches its byte *values* (it is iterated as a list of
            // its bytes), not as a single opaque element.
            let items = match crate::builtins::methods_narg::buf::buf_get_bytes(target) {
                Some(bytes) => bytes.into_iter().map(|b| Value::int(b as i64)).collect(),
                None => runtime::value_to_list_for_receiver(target),
            };
            let batches: Vec<Value> = items
                .chunks(n)
                .map(|chunk| Value::array(chunk.to_vec()))
                .collect();
            Some(Ok(Value::seq(batches)))
        }
        // Cost: O(n - p + m) amortized, p = match position, m = chars of the
        // needle; the byte offset is converted through the cached grapheme index.
        "rindex" => {
            // Fall through to runtime dispatch for arrays (list of needles)
            // and type objects
            if matches!(
                arg.view(),
                ValueView::Array(..) | ValueView::Package(_) | ValueView::Pair(..)
            ) {
                return None;
            }
            // The `Str`/`Cool` row's handler (ADR-11276).
            Some(crate::builtins::method_table::str_search::rindex(
                target,
                std::slice::from_ref(arg),
            ))
        }
        // The `fmt` rows' implementation (`method_table::collections::fmt`).
        "fmt" => super::fmt_contains::fmt_native(target, std::slice::from_ref(arg)),
        // Cost: O(f + n), f = chars of the format (the invocant), n = rendered length.
        "sprintf" => {
            // Method form: '%f'.sprintf(value) — target is the format string.
            // The `*@args` slurpy spreads a single positional container across the
            // directives (`"%s-%s".sprintf([1,2])` is `1-2`, not `1 2-`; likewise a
            // Range: `"%d %d".sprintf(1..2)` is `1 2`), so any list-like arg must
            // defer to the slow-path `sprintf` arm, which routes through
            // `builtin_sprintf` (the same slurpy flattening the sub form gets). A
            // bare type object likewise needs the interpreter-aware warning path
            // (`%s` warns and stringifies to "").
            // An Instance/Package/mixin arg needs `.Str`/`.Int`/`.Numeric`
            // coercion the pure formatter can't dispatch (`fmt_value_needs_coercion`);
            // defer to the slow-path `sprintf` arm below.
            if matches!(arg.view(), ValueView::Package(_))
                || arg.as_list_items().is_some()
                || fmt_value_needs_coercion(arg)
                || matches!(
                    arg.view(),
                    ValueView::Range(..)
                        | ValueView::RangeExcl(..)
                        | ValueView::RangeExclStart(..)
                        | ValueView::RangeExclBoth(..)
                        | ValueView::GenericRange { .. }
                        | ValueView::Seq(..)
                        | ValueView::Slip(..)
                )
            {
                return None;
            }
            let fmt = target.to_string_value();
            // The single scalar arg feeds exactly one directive. Any other
            // directive count is an arg-count mismatch (`"%s and %s".sprintf("a")`
            // wants two args), which `format_sprintf` would silently paper over by
            // rendering the missing directives as empty/0. Defer to the slow-path
            // arm so `builtin_sprintf` raises the same `X::Str::Sprintf::Directives::Count`
            // the sub form does.
            if runtime::sprintf_directive_count(&fmt) != 1 {
                return None;
            }
            let rendered = runtime::format_sprintf(&fmt, Some(arg));
            Some(Ok(Value::str(rendered)))
        }
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
        // Cost: see `parse_base` (src/builtins/parse_base.rs).
        "parse-base" => {
            let radix = match arg.view() {
                ValueView::Int(n) => n,
                _ => return None,
            };
            let s = target.to_string_value();
            Some(crate::builtins::parse_base::parse_base(&s, radix))
        }
        "base" => match target.view() {
            ValueView::Int(_) | ValueView::BigInt(_) => {
                let radix = match arg.view() {
                    ValueView::Int(r) if (2..=36).contains(&r) => r as u32,
                    ValueView::Str(s) => match s.parse::<u32>() {
                        Ok(r) if (2..=36).contains(&r) => r,
                        _ => {
                            return Some(Ok(out_of_range_failure("base requires radix 2..36")));
                        }
                    },
                    _ => {
                        return Some(Ok(out_of_range_failure("base requires radix 2..36")));
                    }
                };
                Some(Ok(Value::str(crate::builtins::int_to_base(target, radix))))
            }
            ValueView::Num(f) => {
                if f.is_infinite() || f.is_nan() {
                    return Some(Err(RuntimeError::new(format!(
                        "X::Numeric::CannotConvert: Cannot convert {} to base",
                        if f.is_nan() {
                            "NaN"
                        } else if f > 0.0 {
                            "Inf"
                        } else {
                            "-Inf"
                        },
                    ))));
                }
                let radix = match arg.view() {
                    ValueView::Int(r) if (2..=36).contains(&r) => r as u32,
                    ValueView::Int(_) => {
                        return Some(Ok(out_of_range_failure("base requires radix 2..36")));
                    }
                    ValueView::Str(s) => match s.parse::<u32>() {
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
                let (n, d) = f64_to_rat(f);
                Some(Ok(Value::str(rat_to_base(n, d, radix, BaseDigits::Auto))))
            }
            ValueView::Rat(n, d) | ValueView::FatRat(n, d) => {
                let radix = match parse_radix_checked(arg)? {
                    Ok(r) => r,
                    Err(_) => return Some(Ok(out_of_range_failure("base requires radix 2..36"))),
                };
                Some(Ok(Value::str(rat_to_base(n, d, radix, BaseDigits::Auto))))
            }
            // Handle Instance types (Duration, Instant, etc.) by
            // extracting their numeric value
            ValueView::Instance { attributes, .. } => {
                let radix = match parse_radix_checked(arg)? {
                    Ok(r) => r,
                    Err(_) => return Some(Ok(out_of_range_failure("base requires radix 2..36"))),
                };
                if let Some(val) = attributes.as_map().get("value") {
                    match val.view() {
                        ValueView::Int(i) => {
                            Some(Ok(Value::str(rat_to_base(i, 1, radix, BaseDigits::Auto))))
                        }
                        ValueView::Rat(n, d) | ValueView::FatRat(n, d) => {
                            Some(Ok(Value::str(rat_to_base(n, d, radix, BaseDigits::Auto))))
                        }
                        ValueView::Num(f) => {
                            let (n, d) = f64_to_rat(f);
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
            let (n, d) = match target.view() {
                ValueView::Int(i) => (i, 1i64),
                ValueView::Rat(n, d) => (n, d),
                ValueView::Num(f) => f64_to_rat(f),
                _ => return None,
            };
            let (non_repeating, repeating) = rat_base_repeating(n, d, radix);
            Some(Ok(Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(vec![
                    Value::str(non_repeating),
                    Value::str(repeating),
                ])),
                ArrayKind::List,
            )))
        }
        // `round($scale)` is the `Cool.round` row's handler; this arm keeps the
        // receivers the table has no shape for (an allomorph: `<1.5>.round(0.5)`).
        // Cost: see `cool_real::round_to`.
        "round" => {
            crate::builtins::method_table::cool_real::round_to(target, std::slice::from_ref(arg))
        }
        // Cost: see `sampling::pick`.
        "pick" => crate::builtins::sampling::pick(target, std::slice::from_ref(arg)),
        // Cost: see `sampling::pickpairs`.
        "pickpairs" => crate::builtins::sampling::pickpairs(target, std::slice::from_ref(arg)),
        // Cost: see `sampling::roll`.
        "roll" => crate::builtins::sampling::roll(target, std::slice::from_ref(arg)),
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
        // The `Blob`/`Buf` rows' implementation (`method_table::blob_read`).
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
