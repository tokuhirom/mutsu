use super::*;

impl Interpreter {
    /// The `$/` visible to the executing frame: its local slot when one exists
    /// (a `method foo($/)` parameter, a `my $/`), else the env entry. This is
    /// the `$/` the `$/` variable itself reads, so every construct defined in
    /// terms of `$/` (`$<name>`, `make`) must go through here rather than read
    /// env `/` directly -- a nested regex op (`.subst`, `~~`) inside an action
    /// method rewrites env `/` to its own (possibly failed) match while the
    /// method's `$/` parameter stays intact.
    ///
    /// Cost: O(1), one hashed `find_local_slot` probe plus one env probe.
    pub(super) fn frame_match_topic(&self, code: &CompiledCode) -> Option<Value> {
        self.locals_get_by_name(code, "/")
            .or_else(|| self.env().get("/").cloned())
            // A `$/` a block shares with its scope is a cell
            // (`env::routine_owned`); `make` works on what it holds.
            .map(|v| v.deref_container())
    }

    /// Rakudo's precondition of the `make` routine: `$/` must hold a Match,
    /// otherwise it throws `X::Make::MatchRequired` (`make "x"` outside any
    /// match, or after a failed one, leaves `$/` Nil).
    ///
    /// Cost: O(1), one [`Self::frame_match_topic`] read.
    pub(super) fn check_make_match_topic(&self, code: &CompiledCode) -> Result<(), RuntimeError> {
        let topic = self.frame_match_topic(code).unwrap_or(Value::NIL);
        if topic.is_match_instance() {
            return Ok(());
        }
        let got = crate::value::types::what_type_name(&topic);
        let msg = format!("The make function expects $/ to contain a Match, but it contains {got}");
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        attrs.insert("got".to_string(), topic);
        let mut err = RuntimeError::new(msg);
        err.exception = Some(Box::new(Value::make_instance(
            crate::symbol::Symbol::intern("X::Make::MatchRequired"),
            attrs,
        )));
        Err(err)
    }

    /// Rakudo's `make` sets `$/.made`: store `value` as the `.made` of the
    /// frame's `$/` Match (found as [`Self::frame_match_topic`] finds it).
    /// The write is in place, so every holder of that Match -- the parse
    /// tree, a code block or wrapper that kept it -- sees it (#12954).
    ///
    /// Cost: O(1), one [`Self::frame_match_topic`] read and one cell write.
    pub(super) fn attach_made_to_match_topic(&mut self, code: &CompiledCode, value: Value) {
        if let Some(topic) = self.frame_match_topic(code) {
            topic.set_match_ast_in_place(value);
        }
    }
}
