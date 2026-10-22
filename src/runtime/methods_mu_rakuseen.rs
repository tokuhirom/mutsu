//! `Mu.rakuseen($id, &code)`, the body of the `Mu.rakuseen` row (ADR-11276
//! §9.164): Rakudo's cyclic-structure guard for `.raku`/`.gist`.

use super::*;

impl Interpreter {
    /// A user `.raku` wraps its body in `self.rakuseen(self.^name, { ... })`;
    /// on the first sight of an id the code runs and its rendering is the
    /// answer, but a REPEAT of the same id means a reference cycle: the answer
    /// is a backreference name instead of re-running the code (which would
    /// recurse forever), and the outer occurrence binds it as
    /// `(my \NAME = ...)`.
    // Cost: O(1) to test the id, plus the code's own call.
    pub(crate) fn mu_rakuseen(&mut self, id: &Value, code: &Value) -> Result<Value, RuntimeError> {
        let id = id.to_string_value();
        if self.raku_cycle_guards.rakuseen.revisit(&id) {
            return Ok(Value::str(Self::rakuseen_backref_name(&id)));
        }
        self.raku_cycle_guards.rakuseen.enter(id.clone());
        let result = self.call_sub_value(code.clone(), vec![]);
        let cycle_hit = self.raku_cycle_guards.rakuseen.leave(&id);
        let rendered = result?;
        if cycle_hit {
            return Ok(Value::str(format!(
                "(my \\{} = {})",
                Self::rakuseen_backref_name(&id),
                rendered.to_string_value()
            )));
        }
        Ok(rendered)
    }

    /// A `.raku`-legal identifier derived from a `rakuseen` id, used for the
    /// `(my \NAME = ...)` cycle backreference. Non-identifier characters (`::`,
    /// `|`, spaces, …) are folded to `_` so the emitted binding parses.
    // Cost: O(n), n = chars of the id.
    fn rakuseen_backref_name(id: &str) -> String {
        let sanitized: String = id
            .chars()
            .map(|c| {
                if c.is_alphanumeric() || c == '_' {
                    c
                } else {
                    '_'
                }
            })
            .collect();
        format!("{}_backref", sanitized)
    }
}
