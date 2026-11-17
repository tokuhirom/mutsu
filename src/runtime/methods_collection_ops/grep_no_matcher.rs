use super::*;

impl Interpreter {
    /// The `X::Multi::NoMatch` a `.grep` call without a matcher raises, worded
    /// as rakudo's dispatcher does (#11630):
    ///
    /// ```text
    /// Cannot resolve caller grep(List:D: :k); none of these signatures matches:
    ///     ($:: Bool:D $t, *%_)
    ///     ($:: Mu $t, *%_)
    /// ```
    ///
    /// `args` are the call's own arguments; only named ones can be left when
    /// there is no matcher, and they are listed after the invocant.
    // Cost: O(a), a = number of arguments.
    pub(super) fn grep_no_matcher_error(target: &Value, args: &[Value]) -> RuntimeError {
        let smiley = if crate::runtime::types::value_is_defined(target) {
            ":D"
        } else {
            ":U"
        };
        let named: Vec<String> = args
            .iter()
            .filter_map(|arg| match arg.view() {
                ValueView::Pair(key, value) => Some(match value.view() {
                    ValueView::Bool(true) => format!(":{key}"),
                    _ => format!(":{key}({})", value.to_string_value()),
                }),
                _ => None,
            })
            .collect();
        let type_name = crate::runtime::utils::value_type_name(target);
        let message = format!(
            "Cannot resolve caller grep({type_name}{smiley}: {}); none of these signatures matches:{}",
            named.join(", "),
            crate::runtime::builtin_method_sigs::candidate_lines(type_name, "grep"),
        );
        Self::multi_no_match_exception("grep", message)
    }
}
