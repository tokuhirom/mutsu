//! `Match.make`, the body of its row (ADR-11276 §9.164).

use super::*;

impl Interpreter {
    /// `$match.make($value)`: writes the match's `.made` in place (#12954) and
    /// what `$/.made` and the grammar's action walk read next. `None` when
    /// `target` is not a match.
    // Cost: O(1).
    pub(crate) fn match_make(&mut self, target: &Value, value: &Value) -> Option<Value> {
        if !target.set_match_ast_in_place(value.clone()) {
            return None;
        }
        self.env.insert("made".to_string(), value.clone());
        self.regex_state.action_made = Some(value.clone());
        Some(value.clone())
    }
}
