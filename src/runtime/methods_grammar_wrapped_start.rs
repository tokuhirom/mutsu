//! `.parse`/`.subparse` entering a start rule that carries a `.wrap` chain
//! (`G.^find_method('TOP').wrap(...)`, issue #9190).
//!
//! Rakudo's `.parse` calls the start rule as a method on a fresh cursor, so a
//! wrapper installed on it runs like any other method wrapper. mutsu's
//! `dispatch_package_parse` instead matches the start rule's pattern
//! directly, which never looked at the wrap table. Here the wrapper is called
//! with a cursor at the start position; the terminal its `callsame` reaches
//! re-enters `dispatch_package_parse` with the wrap check switched off for
//! exactly that one call, so the whole regular parse (full-match anchoring,
//! `:actions`, proto candidates, failure reporting) runs unchanged inside the
//! wrapper. The wrapper's return value is the parse result.

use std::cell::Cell;

use super::*;

thread_local! {
    /// Set by the wrap-chain terminal just before it re-enters
    /// `dispatch_package_parse`, and consumed by the first wrap check that
    /// call makes -- so a nested `.parse` inside the rule still sees its own
    /// start rule's wrappers.
    static START_RULE_WRAP_BYPASS: Cell<bool> = const { Cell::new(false) };
}

/// Consume the one-shot bypass flag the terminal set.
pub(super) fn take_start_rule_wrap_bypass() -> bool {
    START_RULE_WRAP_BYPASS.with(|flag| flag.replace(false))
}

impl Interpreter {
    /// Run the `.wrap` chain of `package_name`'s start rule for the
    /// `.parse`/`.subparse` call `method(args)`.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn call_wrapped_start_rule(
        &mut self,
        package_name: &str,
        start_rule: &str,
        method: &str,
        args: &[Value],
        text: &str,
        pos: usize,
        chain: &[(u64, Value)],
    ) -> Result<Value, RuntimeError> {
        self.call_wrapped_token_method_with_terminal(
            Symbol::intern(package_name),
            start_rule,
            &[],
            text,
            pos,
            chain,
            Some((method, args)),
        )
    }

    /// The terminal of a wrapped start rule: the regular parse, with the wrap
    /// check bypassed so it does not run the wrapper a second time.
    pub(super) fn run_wrapped_start_rule_terminal(
        &mut self,
        package_name: &str,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        START_RULE_WRAP_BYPASS.with(|flag| flag.set(true));
        let result = self.dispatch_package_parse(package_name, method, args);
        // A parse that returned before reaching its wrap check (an error)
        // must not leave the flag for an unrelated later parse.
        START_RULE_WRAP_BYPASS.with(|flag| flag.set(false));
        result
    }
}
