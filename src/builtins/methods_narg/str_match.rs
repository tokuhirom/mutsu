use crate::value::{RuntimeError, Value, ValueView};

pub(crate) fn is_str_or_match_receiver(target: &Value) -> bool {
    matches!(target.view(), ValueView::Str(_)) || target.is_match_instance()
}

/// Separate `(needle, …)`-style positional args from the `:i`/`:ignorecase`/
/// `:m`/`:ignoremark` named markings shared by `contains` / `starts-with` /
/// `ends-with` / `substr-eq`. Returns `None` if an *unexpected* named arg is
/// present (the interpreter owns those semantics). `ignore_mark` being requested
/// is folded by the shared `str_prim::Fold`, like `:i`.
fn split_string_match_args(args: &[Value]) -> Option<(Vec<&Value>, bool, bool)> {
    let mut positional: Vec<&Value> = Vec::new();
    let mut ignore_case = false;
    let mut ignore_mark = false;
    for arg in args {
        if let ValueView::Pair(key, value) = arg.view() {
            match key.as_str() {
                "i" | "ignorecase" => ignore_case = value.truthy(),
                "m" | "ignoremark" => ignore_mark = value.truthy(),
                _ => return None,
            }
        } else {
            positional.push(arg);
        }
    }
    Some((positional, ignore_case, ignore_mark))
}

/// `.starts-with($needle, :i?)` / `.ends-with($needle, :i?)` on a `Str` receiver —
/// the case-insensitive named forms. The plain `starts-with($needle)` form keeps its
/// `native_method_1arg` arm; this handles only the forms that carry a marking Pair
/// (which pushes them past the arity-keyed native dispatch). Mirrors
/// `Interpreter::dispatch_prefix_suffix_check`.
///
/// Returns `None` (fall through) for: non-Str receivers, a Package needle,
/// unknown named args, and the bare
/// single-needle form.
// Cost: O(m) amortized, m = chars of the needle (`str_prim::affix_matches`
// folds only the graphemes the needle covers).
pub(crate) fn native_prefix_suffix_with_options(
    target: &Value,
    args: &[Value],
    is_prefix: bool,
) -> Option<Result<Value, RuntimeError>> {
    if !is_str_or_match_receiver(target) {
        return None;
    }
    let (positional, ignore_case, ignore_mark) = split_string_match_args(args)?;
    let needle_val: &Value = positional.first().copied()?;
    // Bare single needle (no markings) stays on the 1-arg native arm.
    if positional.len() == args.len() {
        return None;
    }
    if let ValueView::Package(_) = needle_val.view() {
        return None;
    }
    let needle = needle_val.to_string_value();
    let fold = crate::builtins::str_prim::Fold::new(ignore_case, ignore_mark);
    let ok = crate::builtins::str_prim::affix_matches(target, &needle, is_prefix, fold);
    Some(Ok(Value::truth(ok)))
}

/// `.substr-eq($needle, $pos?, :i?)` on a `Str` receiver — the case-insensitive and/or
/// named forms. The plain `substr-eq($needle, Int $pos)` form keeps its
/// `native_method_2arg` arm; this handles the forms carrying a marking Pair (which push
/// them past the arity-keyed dispatch). Mirrors `Interpreter::dispatch_substr_eq`.
///
/// Returns `None` (fall through) for: non-Str receivers, a Package needle,
/// unknown named args, non-Int/Str positions (Whatever resolution),
/// out-of-range / negative positions (X::OutOfRange Failure), and the bare forms
/// already handled by the 1-/2-arg arms.
// Cost: O(m) amortized, m = chars of the needle, once the invocant's grapheme
// index is cached (`$pos` is resolved through it).
pub(crate) fn native_substr_eq_with_options(
    target: &Value,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    if !is_str_or_match_receiver(target) {
        return None;
    }
    let (positional, ignore_case, ignore_mark) = split_string_match_args(args)?;
    // Only the named forms reach here; the bare positional forms keep their
    // existing 1-/2-arg native arms.
    if positional.len() == args.len() {
        return None;
    }
    let needle_val: &Value = positional.first().copied()?;
    if let ValueView::Package(_) = needle_val.view() {
        return None;
    }
    let start = match positional.get(1).copied().map(Value::view) {
        Some(ValueView::Int(i)) => i,
        Some(ValueView::Str(s)) => s.parse::<i64>().ok()?,
        Some(_) => return None,
        None => 0,
    };
    if start < 0 {
        return None;
    }
    let needle = needle_val.to_string_value();
    crate::builtins::grapheme_index::with_str_index(target, |text, idx| {
        if start as usize > idx.len() {
            return None;
        }
        let fold = crate::builtins::str_prim::Fold::new(ignore_case, ignore_mark);
        let eq = crate::builtins::str_prim::eq_at(text, idx, start as usize, &needle, fold);
        Some(Ok(Value::truth(eq)))
    })
}
