//! The `Routine` and `Code` rows a regex answers (ADR-14477).
//!
//! `Regex is Method`, so its owner chain reaches `Routine`'s and `Code`'s rows
//! (`code_class::owners_of`). A regex value (`ValueView::Regex`) is not a
//! routine the registry holds nor a `Sub` carrying a body, so it has none of
//! the state those rows read: each answers what a routine with no
//! declaration, no multi candidates and no traits answers.

use crate::value::{RuntimeError, Value, ValueMap, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

/// Whether `target` is a regex value.
// Cost: O(1).
pub(super) fn is_regex(target: &Value) -> bool {
    matches!(
        target.view(),
        ValueView::Regex(_) | ValueView::RegexWithAdverbs(..)
    )
}

/// The answer of the `Routine`/`Code` row `method` for the regex `target`, or
/// `None` for a method no regex answer is modelled for.
// Cost: O(1).
pub(super) fn answer(target: &Value, method: &str) -> Answer {
    let value = match method {
        "multi" | "is_dispatcher" | "yada" | "rw" | "is-implementation-detail" => Value::FALSE,
        "dispatcher" | "dispatchees" => Value::NIL,
        "package" => Value::package(crate::symbol::Symbol::intern("Mu")),
        "candidates" => Value::array(vec![target.clone()]),
        "prec" => Value::hash(ValueMap::default()),
        "precedence" | "associative" => Value::str(String::new()),
        _ => return None,
    };
    Some(Ok(value))
}
