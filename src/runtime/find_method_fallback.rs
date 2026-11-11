//! `.^find_method($name)` for a name a class does not have, when the class
//! (or a parent) declares `FALLBACK` (#16888, EEL 0.3's `Event`).
//!
//! Rakudo's `ClassHOW.find_method` asks the class's fallbacks before giving up:
//! a class with a `FALLBACK` method answers every missing name with a dispatcher
//! (a `ForeignCode`) that calls `FALLBACK` with the name in front of the
//! arguments. Code that probes for a method therefore sees a defined callable,
//! not `Mu`; EEL's runner relies on it to report an unknown subpattern rather
//! than die binding `Mu`. `:no_fallback` skips it, as in rakudo.
//!
//! The dispatcher is a small Raku closure over the name, built from a snippet
//! parsed once per process.

use super::Interpreter;
use crate::ast::Stmt;
use crate::value::{RuntimeError, Value, ValueView};

const FALLBACK_DISPATCHER_SRC: &str = r#"sub ($name) {
    -> Mu \obj, |args { obj.FALLBACK($name, |args) }
}"#;

impl Interpreter {
    /// The `FALLBACK` dispatcher `.^find_method` answers for `method_name` on
    /// `invocant`; `None` when `adverbs` holds a true `:no_fallback` or the
    /// type has no `FALLBACK`.
    // Cost: O(m) for the `FALLBACK` lookup, m = the MRO; plus the closure's
    // one-time parse.
    pub(super) fn find_method_fallback(
        &mut self,
        invocant: &Value,
        method_name: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        let no_fallback = args.iter().any(|a| {
            matches!(a.view(), ValueView::Pair(key, value) if key == "no_fallback" && value.truthy())
        });
        if no_fallback || method_name == "FALLBACK" {
            return Ok(None);
        }
        if self.classhow_find_method(invocant, "FALLBACK").is_none() {
            return Ok(None);
        }
        use std::sync::OnceLock;
        static PARSED: OnceLock<Vec<Stmt>> = OnceLock::new();
        let stmts = PARSED.get_or_init(|| {
            crate::runtime::prelude_source::parse_prelude_source(FALLBACK_DISPATCHER_SRC)
                .map(|(s, _)| s)
                .unwrap_or_default()
        });
        if stmts.is_empty() {
            return Ok(None);
        }
        let maker = self.eval_block_value(stmts)?;
        let dispatcher = self.eval_call_on_value(maker, vec![Value::str_from(method_name)])?;
        Ok(Some(dispatcher))
    }
}
