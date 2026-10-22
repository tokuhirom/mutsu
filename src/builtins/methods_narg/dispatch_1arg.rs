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
