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

    /// Rakudo's `make` sets `$/.made`: store `value` as the `.ast` of the
    /// frame's `$/` Match, writing it back where
    /// [`Self::frame_match_topic`] found it (the local slot first, else env).
    /// A Match is a value here, so the updated copy (same identity) replaces
    /// the old one.
    ///
    /// Cost: O(a), a = the Match's attribute count (the copy).
    pub(super) fn attach_made_to_match_topic(&mut self, code: &CompiledCode, value: Value) {
        if let Some(slot) = self.find_local_slot(code, "/") {
            if let Some(updated) = self.locals[slot].match_with_ast_keeping_id(value) {
                self.locals[slot] = updated;
            }
        } else if let Some(updated) = self
            .env()
            .get("/")
            .and_then(|m| m.match_with_ast_keeping_id(value))
        {
            self.env_mut().insert("/".to_string(), updated);
        }
    }
}
