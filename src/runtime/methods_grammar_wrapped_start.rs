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

/// The `.parse` call a custom HOW's start-rule wrapper stands for: the grammar,
/// the parse method and its arguments, armed while that wrapper runs.
pub(super) struct CustomHowStart {
    pub(super) package: String,
    pub(super) method: String,
    pub(super) args: Vec<Value>,
}

thread_local! {
    /// Armed by [`Interpreter::call_custom_how_start_rule`] around the wrapper
    /// a custom `GrammarHOW.find_method` returned for the start rule. The
    /// wrapper's call of the original rule (`$meth($cursor)`) takes it and runs
    /// the real parse there (see `dispatch_package_parse_impl`), so the wrapper
    /// brackets the whole parse as it brackets any other rule call.
    static CUSTOM_HOW_START: std::cell::RefCell<Option<CustomHowStart>> =
        const { std::cell::RefCell::new(None) };
}

/// Take the armed custom-HOW start call, if `package` is its grammar.
// Cost: O(1).
pub(super) fn take_custom_how_start(package: &str) -> Option<CustomHowStart> {
    CUSTOM_HOW_START.with(|slot| {
        let mut slot = slot.borrow_mut();
        if slot.as_ref().is_some_and(|start| start.package == package) {
            slot.take()
        } else {
            None
        }
    })
}

/// Consume the one-shot bypass flag the terminal set.
pub(super) fn take_start_rule_wrap_bypass() -> bool {
    START_RULE_WRAP_BYPASS.with(|flag| flag.replace(false))
}

impl Interpreter {
    /// `.parse`/`.subparse` entering a start rule of a grammar whose custom
    /// HOW (`EXPORTHOW`) has a `find_method`: ask it for the rule as it does
    /// for a subrule and, when it answers a wrapper, run the wrapper with a
    /// cursor at the start position. `None` when there is nothing to wrap (no
    /// custom HOW, the plain method came back, or `find_method` failed in a
    /// way the ordinary parse reports itself).
    // Cost: O(1) plus the user `find_method` and wrapper.
    pub(super) fn call_custom_how_start_rule(
        &mut self,
        package_name: &str,
        start_rule: &str,
        parse_call: (&str, &[Value]),
        text: &str,
        pos: usize,
    ) -> Option<Result<Value, RuntimeError>> {
        let how = self
            .registry()
            .grammar_custom_how
            .get(package_name)
            .cloned()?;
        let pkg = Symbol::intern(package_name);
        let found = self.run_regex_sub_call_here(Some(pkg), |interp| {
            interp.call_method_with_values(
                how,
                "find_method",
                vec![Value::package(pkg), Value::str(start_rule.to_string())],
            )
        });
        let meth = match found {
            Ok(v) => v,
            Err(e) => return Some(Err(e)),
        };
        if !matches!(meth.view(), ValueView::Sub(_)) {
            return None;
        }
        let chars: Vec<char> = text.chars().collect();
        let cursor = self.new_grammar_cursor(&chars, pos, pkg);
        CUSTOM_HOW_START.with(|slot| {
            *slot.borrow_mut() = Some(CustomHowStart {
                package: package_name.to_string(),
                method: parse_call.0.to_string(),
                args: parse_call.1.to_vec(),
            });
        });
        let called = self.run_regex_sub_call_here(Some(pkg), |interp| {
            interp.call_sub_value(meth, vec![cursor])
        });
        CUSTOM_HOW_START.with(|slot| *slot.borrow_mut() = None);
        Some(called.map(Self::failed_cursor_as_nil))
    }

    /// Run the `.wrap` chain of `package_name`'s start rule for the
    /// `.parse`/`.subparse` call `parse_call` (its method name and arguments),
    /// handing the wrapper `cursor`.
    pub(super) fn call_wrapped_start_rule(
        &mut self,
        package_name: &str,
        start_rule: &str,
        parse_call: (&str, &[Value]),
        cursor: Value,
        chain: &[(u64, Value)],
    ) -> Result<Value, RuntimeError> {
        let result = self.call_wrapped_token_method_with_terminal(
            Symbol::intern(package_name),
            start_rule,
            vec![cursor],
            chain,
            Some(parse_call),
        )?;
        Ok(Self::failed_cursor_as_nil(result))
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
