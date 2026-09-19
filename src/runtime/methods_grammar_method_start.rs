//! `.parse`/`.subparse` when the grammar's start rule is a plain `method`
//! rather than a `rule`/`token`/`regex` (issue #8752).
//!
//! A well-established Raku idiom overrides `TOP` as an ordinary method to run
//! setup code (bind a dynamic variable from named args, say) before
//! delegating to the real entry rule:
//!
//! ```raku
//! grammar G {
//!     method TOP (Bool :$validate = False) {
//!         my $*VALIDATE = $validate;
//!         self.ip-variants
//!     }
//!     rule ip-variants { ... }
//! }
//! ```
//!
//! `dispatch_package_parse`'s candidate/reduce-time machinery is built around
//! an actual regex pattern for the start rule, which a plain method does not
//! have. This module instead calls the method as an ordinary method call,
//! with `self` bound to a cursor over the whole input anchored at the
//! requested start position -- the same cursor shape the NQP cursor protocol
//! builds (`regex_cursor::make_cursor_value`). A call the method makes back
//! into the grammar (`self.ip-variants`) is then an ordinary method call that
//! lands on the "grammar token called as an instance method" fallback in
//! `methods_instance_ops.rs`, which continues from the cursor's own position
//! instead of restarting on `""` once the receiver is a live cursor.
//!
//! `:actions` is honoured the same way the regular start-rule path honours
//! it once the method returns -- one dispatch of `actions.<start_rule>($match)`
//! on the final result -- but the reduce-time dynvar overlay and per-subrule
//! action dispatch the regex engine drives while matching are NOT hooked up
//! here: a plain method has no subrule reduces of its own for this function
//! to intercept. That is a narrower surface than the full regex-driven path;
//! see issue #8752 for the design note this deliberately leaves open.

use super::*;

/// The pieces of `dispatch_package_parse`'s own arguments that
/// `dispatch_package_parse_via_method` needs, bundled to keep the function
/// under clippy's argument-count lint rather than tacking on a ninth
/// positional parameter.
pub(super) struct MethodStartRuleCall<'a> {
    pub(super) package_name: &'a str,
    pub(super) start_rule: &'a str,
    pub(super) text: &'a str,
    pub(super) is_full_parse: bool,
    pub(super) start_pos: Option<usize>,
    pub(super) continue_pos: Option<usize>,
    pub(super) rule_args: &'a [Value],
}

impl Interpreter {
    /// Run a method-shaped start rule and produce the same kind of result
    /// `dispatch_package_parse`'s regular path would: a `Match`/failed-`Match`
    /// for `.subparse`, or a `Match`/`Failure` for `.parse`/`.parsefile`.
    pub(super) fn dispatch_package_parse_via_method(
        &mut self,
        call: MethodStartRuleCall<'_>,
        actions_obj: &mut Option<Value>,
    ) -> Result<Value, RuntimeError> {
        let MethodStartRuleCall {
            package_name,
            start_rule,
            text,
            is_full_parse,
            start_pos,
            continue_pos,
            rule_args,
        } = call;
        let from = start_pos.or(continue_pos).unwrap_or(0) as i64;
        let cursor = Interpreter::make_cursor_value(package_name, text, from, from, false);
        let value = self.call_method_with_values(cursor, start_rule, rule_args.to_vec())?;
        // rakudo requires the start rule -- regex-shaped or not -- to hand
        // back a Match/Cursor; a method that returns anything else (commonly
        // `Nil`, from a body with no explicit delegation) is a hard error,
        // not a failed parse: `Method 'TOP' returned a Nil object (Nil)
        // rather than a Match object` (measured against rakudo 2026.07).
        if !value.is_match_instance() {
            return Err(RuntimeError::new(format!(
                "Method '{}' returned a {} object ({}) rather than a Match object",
                start_rule,
                crate::runtime::value_type_name(&value),
                crate::runtime::gist_value(&value),
            )));
        }
        if !value.truthy() {
            // A genuine failed Match (e.g. the rule TOP delegated to did not
            // match) -- same outcome as a regex-shaped start rule failing.
            self.env.insert("/".to_string(), Value::NIL);
            return Ok(if is_full_parse {
                self.parse_failure_for_pattern(text, None)
            } else {
                value
            });
        }
        // `.parse`/`.parsefile` require the start rule to consume the WHOLE
        // input, same as when the start rule is a regex.
        if is_full_parse && value.match_to().map(|to| to as usize) != Some(text.chars().count()) {
            self.env.insert("/".to_string(), Value::NIL);
            let best_end = value.match_to().unwrap_or(0).max(0) as usize;
            return Ok(self.make_parse_failure_value(text, best_end));
        }
        let value = if let Some(actions) = actions_obj.as_mut() {
            self.invoke_grammar_actions(value, actions, start_rule)?
        } else {
            value
        };
        self.reset_capture_env_vars();
        self.env.insert("/".to_string(), value.clone());
        Ok(value)
    }
}
