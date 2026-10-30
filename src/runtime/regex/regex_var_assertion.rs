//! `<$name>` on a lexical the pattern itself declared (#13029).
//!
//! ```raku
//! "x a" ~~ / :my $n; { $n = "x" } <$n> /;
//! ```
//!
//! The parse-time `<$var>` arm (`regex_parse_core`) reads the variable when the
//! pattern is parsed and compiles its value into the tree. An in-regex `:my`
//! lexical, a hoisted leading `:my` and a rule's runtime parameter have no
//! value then — or only their initializer — so for those names the parser
//! emits [`RegexAtom::VarAssertion`] instead, and the value is read here, when
//! the atom is matched, the way rakudo's `INTERPOLATE_ASSERTION` reads it.
//!
//! [`RegexAtom::VarAssertion`]: crate::runtime::regex_types::RegexAtom::VarAssertion

use super::super::*;
use super::regex_atom_leaf::VarInterpTarget;

impl Interpreter {
    /// What `<$name>` matches when `$name` holds `val` (`None`: unbound): its
    /// value as a pattern entered as one capture-isolated group, so the
    /// callee's captures stay its own while a backreference inside it still
    /// sees them. A `Regex` value runs its own source (in its defining scope
    /// when it closed over one); a `Str` is regex source, refused like the
    /// parse-time `<$var>` arm refuses it (a code block without `use
    /// MONKEY-SEE-NO-EVAL`, a longname alias, an undeclared variable), with
    /// the error raised through `PENDING_REGEX_ERROR`. An undefined value
    /// dies as rakudo's dispatch to `INTERPOLATE_ASSERTION` does
    /// ([`Self::interpolate_assertion_undefined_error`]).
    // Cost: O(r) to render the value, r = its length; the parse is cached by
    // its source, and the isolated wrapper is built once per parse.
    pub(super) fn var_assertion_target(
        &self,
        val: Option<Value>,
        ignore_case: bool,
    ) -> VarInterpTarget {
        let val = val.unwrap_or(Value::NIL);
        let is_regex = matches!(
            val.view(),
            ValueView::Regex(_) | ValueView::RegexWithAdverbs(_)
        );
        if !is_regex && let Some(err) = Self::interpolate_assertion_undefined_error(&val) {
            crate::runtime::regex_parse::PENDING_REGEX_ERROR.with(|e| {
                *e.borrow_mut() = Some(err);
            });
            return VarInterpTarget::End(None);
        }
        let text = if is_regex {
            Self::regex_alternation_sources(&val, false).swap_remove(0)
        } else {
            val.to_string_value()
        };
        let refusal = if Self::contains_longname_alias(&text) {
            Some(Self::make_longname_alias_error())
        } else if !is_regex
            && !self.monkey_see_no_eval()
            && Self::contains_dangerous_regex_code(&text)
        {
            Some(Self::make_security_policy_error())
        } else {
            None
        };
        let closure_scope = val.regex_closure_scope();
        let _closure_scope_guard = closure_scope
            .clone()
            .map(super::regex_helpers::RegexInterpClosureScopeGuard::activate);
        let refusal = refusal.or_else(|| self.check_undeclared_vars_in_pattern(&text));
        if let Some(err) = refusal {
            crate::runtime::regex_parse::PENDING_REGEX_ERROR.with(|e| {
                *e.borrow_mut() = Some(err);
            });
            return VarInterpTarget::End(None);
        }
        let source = if ignore_case { format!(":i {text}") } else { text };
        let Some(parsed) = self.parse_regex(&source) else {
            return VarInterpTarget::End(None);
        };
        VarInterpTarget::Pattern(match closure_scope {
            // The scope is the value's own, so this wrapper is not shared.
            Some(scope) => std::sync::Arc::new(RegexPattern::single_token(
                RegexAtom::CaptureIsolatedGroupScoped((*parsed).clone(), scope),
            )),
            None => RegexPattern::capture_isolated(&parsed),
        })
    }

    /// The error `<$var>` raises when `$var` holds an undefined value (Nil
    /// or a type object): rakudo has no `INTERPOLATE_ASSERTION` candidate for
    /// a `:U` argument, so the call dies with `X::Multi::NoMatch` (#13042).
    /// `None` for a defined value.
    // Cost: O(1).
    pub(in crate::runtime) fn interpolate_assertion_undefined_error(
        val: &Value,
    ) -> Option<RuntimeError> {
        let type_name = match val.view() {
            ValueView::Nil => "Any".to_string(),
            ValueView::Package(name) => name.resolve().to_string(),
            _ => return None,
        };
        Some(crate::runtime::methods_signature_errors::make_no_match_error_with_message(
            format!(
                "Cannot resolve caller INTERPOLATE_ASSERTION(Match:D: {type_name}:U, BOOTInt, \
                 BOOTInt, BOOTInt, BOOTInt, PseudoStash:D); none of these signatures matches:\n    \
                 (Match $:: Associative:D, $, $, $, $, $, *%_)\n    \
                 (Match $:: Iterable:D \\var, int \\im, int \\monkey, int \\s, $, \\context, *%_)\n    \
                 (Match $:: Mu:D \\var, int \\im, int \\monkey, $, $, \\context, *%_)"
            ),
        ))
    }
}
