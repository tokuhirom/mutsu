//! `CALLER::OUTER::x` and `CALLER::UNIT::x`: a name in the lexical scopes
//! around a calling frame (#18787).
//!
//! `CALLER::` names the frame that called the running one, and the pseudo
//! package after it walks that frame's *lexical* chain, not its callers:
//! `f1({ $CALLER::OUTER::x })` reads the `$x` of the block that declared `f1`,
//! even when `f1` was called from a nested block with its own `$x`. The
//! caller's env carries that chain as its frame root's static link (ADR-12529
//! phase 3), which `Env::enclosing_scopes` reads: the frame's capture (the
//! scope that created it) when the link puts it first, then the program
//! scope.

use super::*;

impl Interpreter {
    /// The value of `name` -- an `OUTER::`/`UNIT::` chain after `CALLER::`,
    /// such as `OUTER::x` or `@UNIT::a` -- in the lexical scopes around the
    /// frame `depth` calls up. `None` when `name` is not such a chain; `Nil`
    /// when the scopes do not hold it.
    // Cost: O(d + s), d = tiers of the caller's chain up to its frame root,
    // s = enclosing scopes searched (each a hashed lookup).
    pub(crate) fn caller_lexical_chain_var(&self, name: &str, depth: usize) -> Option<Value> {
        let (sigil, rest) = match name.as_bytes().first() {
            Some(b'@' | b'%' | b'&') => name.split_at(1),
            _ => ("", name),
        };
        let mut outer_depth = 0usize;
        let mut rest = rest;
        while let Some(after) = rest.strip_prefix("OUTER::") {
            outer_depth += 1;
            rest = after;
        }
        let unit = outer_depth == 0 && rest.starts_with("UNIT::");
        if unit {
            rest = &rest["UNIT::".len()..];
        }
        if (outer_depth == 0 && !unit) || rest.is_empty() || crate::qualified::is_qualified_str(rest) {
            return None;
        }
        let index = self.caller_env_stack.len().checked_sub(depth)?;
        let env = self.caller_env_stack.get(index)?;
        let key = format!("{sigil}{rest}");
        let found = if unit {
            env.unit_scope().and_then(|scope| scope.get(&key))
        } else {
            env.enclosing_scopes()
                .iter()
                .find_map(|scope| scope.get(&key))
        };
        Some(found.unwrap_or(Value::NIL))
    }
}
