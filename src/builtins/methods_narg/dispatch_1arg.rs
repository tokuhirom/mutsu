use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

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
    // A positional index of a receiver whose row binds it (`Int:D`, and every
    // other index as Rakudo's candidates do) is the row's alone, through
    // `method_table::index_arg` (ADR-11276 §9.105).
    // Cost: O(1), a bit test, a tag probe and one hash lookup.
    if matches!(method, "AT-POS" | "EXISTS-POS")
        && crate::builtins::method_table::index_arg::binds(target, method_sym)
    {
        return None;
    }
    match method {
        // A list with no settled shape (a lazy or unreified `Seq`, a
        // `LazyList`) reads its element through the `AT-POS`/`EXISTS-POS`
        // rows' implementation (`method_table::positional`); any other
        // receiver with no shape is `answer_shapeless`'s (§9.154).
        "AT-POS" => {
            // Any other index is bound through its `.Int` (`index_arg`).
            if !matches!(
                arg.view(),
                ValueView::Int(_) | ValueView::Num(_) | ValueView::Rat(..) | ValueView::Str(_)
            ) {
                return None;
            }
            let idx = match crate::builtins::method_table::positional::at_pos_index(arg) {
                Ok(idx) => idx,
                Err(answer) => return answer,
            };
            crate::builtins::method_table::positional::at_pos_of(target, idx)
        }
        "EXISTS-POS" => {
            crate::builtins::method_table::positional::exists_pos(target, std::slice::from_ref(arg))
        }
        _ => None,
    }
}
