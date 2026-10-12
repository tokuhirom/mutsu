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

    /// Run a one-argument `make EXPR` straight on the builtin when nothing
    /// shadows the routine, instead of through the generic call funnel.
    ///
    /// `make` is called once per action method of a grammar parse, and the
    /// funnel spent ~15,000 instructions per call proving it is not a user
    /// routine (the compiled-function and multi lookups, the native-function
    /// tables, the lexical `&make` probes) before its interpreter terminal
    /// reached [`Self::builtin_make`]. This lane checks the same shadowing
    /// sources once, through the generation-cached probes the dispatcher
    /// builtins use ([`Self::dispatcher_builtin_shadowed`]), then does what
    /// the terminal does: the `$/` precondition, the builtin, and the `.made`
    /// write into the frame's `$/`. `args` are already normalized (`VarRef`s
    /// unwrapped, `Proxy` arguments fetched). Any other call shape — no or
    /// several positionals, a named argument — is left to the funnel, which
    /// raises the binding error.
    ///
    /// Cost: O(l + d), l = locals declared under `&make` in `code` (normally
    /// 0), d = the env scope-chain depth of the `&make` lookup; the registry
    /// probes are generation-cached.
    pub(super) fn try_make_builtin_call(
        &mut self,
        code: &CompiledCode,
        name_sym: crate::symbol::Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let [arg] = args else {
            return None;
        };
        if arg.is_string_pair_value()
            || self.dispatcher_builtin_shadowed(
                code,
                name_sym.as_str(),
                name_sym,
                crate::symbol::wk::amp_make(),
            )
        {
            return None;
        }
        if let Err(err) = self.check_make_match_topic(code) {
            return Some(Err(err));
        }
        let result = match self.builtin_make(args) {
            Ok(value) => value,
            Err(err) => return Some(Err(err)),
        };
        self.attach_made_to_match_topic(code, result.clone());
        Some(Ok(result))
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
