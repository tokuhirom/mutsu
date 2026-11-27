//! `Str.match(Regex, :as(Str))`: the matches as their matched text.
//!
//! rakudo's `:$as!` candidate answers with the `Str` of each match instead of
//! the `Match` (a `Nil` miss stays `Nil`), and the `:g`/`:ov`/`:ex`/`:x`/`:nth`
//! forms honour it too. The `:c` and `:p` candidates come first in rakudo's
//! dispatch and ignore it, as does any other type (`:as(Int)`).

use super::*;
use crate::value::ValueView;

impl Interpreter {
    /// Whether `args` of a `.match` ask for `:as(Str)` in a form that honours
    /// it (no `:c`/`:continue`/`:p`/`:pos`).
    // Cost: O(a), a = number of arguments.
    pub(super) fn match_wants_str(args: &[Value]) -> bool {
        let mut as_str = false;
        for arg in args {
            if let ValueView::Pair(key, value) = arg.view() {
                match key.as_str() {
                    "as" => {
                        as_str = matches!(value.view(), ValueView::Package(name) if name == crate::sym!("Str"))
                    }
                    "c" | "continue" | "p" | "pos" => return false,
                    _ => {}
                }
            }
        }
        as_str
    }

    /// The result of a `.match` made into what `:as(Str)` returns: a `Match`
    /// becomes its text and a list of matches a list of texts.
    // Cost: O(r), r = number of matches.
    pub(super) fn match_result_as_str(result: Value) -> Value {
        if result.is_match_instance() {
            return Value::str(result.to_string_value());
        }
        match result.view() {
            ValueView::Array(..) => Value::array(
                crate::value::to_list::value_to_list(&result)
                    .into_iter()
                    .map(Self::match_result_as_str)
                    .collect(),
            ),
            _ => result,
        }
    }
}
