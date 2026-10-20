use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};

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

    // The receiver step every entry runs (ADR-11276 §2.4,
    // `method_table::receiver_step`): the held value of a container, a
    // hash-like receiver's pairs, ... is a call of its own, which asks the
    // table first, so the decline step below sees the receiver the table was
    // asked about.
    // Cost: O(1) tag probes for a receiver in no such state.
    match crate::builtins::method_table::receiver_step::step(target, method, 1) {
        Some(crate::builtins::method_table::receiver_step::Step::Redirect(receiver, _)) => {
            return native_method_1arg(&receiver, method_sym, arg);
        }
        Some(crate::builtins::method_table::receiver_step::Step::Answer(answer)) => {
            return Some(answer);
        }
        Some(crate::builtins::method_table::receiver_step::Step::Itself) | None => {}
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
    // A list with no settled shape (a lazy or unreified `Seq`, a `LazyList`)
    // reads its element through `Seq`'s `AT-POS`/`EXISTS-POS` rows; any other
    // receiver with no shape is `answer_shapeless`'s (§9.154).
    if matches!(method, "AT-POS" | "EXISTS-POS") {
        return crate::builtins::method_table::answer_as(
            crate::value::DispatchShape::Seq,
            target,
            method_sym,
            std::slice::from_ref(arg),
        );
    }
    None
}
