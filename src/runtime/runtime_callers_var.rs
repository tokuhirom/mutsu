//! `CALLERS::<name>`: a variable of any caller scope.

use super::*;

impl Interpreter {
    /// Look up a variable through the `$CALLERS::` chain — "any caller scope".
    ///
    /// `cascade` (a `$*`-twigil dynamic name or a routine) walks the caller
    /// frames from the one at `depth` outward to the oldest, returning the
    /// first that holds the dynamic var. A plain name (`cascade == false`)
    /// cascades too under 6.e, as rakudo's does (#18836); under 6.d it is
    /// [`Self::get_caller_var`]'s, since rakudo 6.d does not cascade a
    /// non-twigil `is dynamic` lexical.
    // Cost: O(c * t), c = caller frames walked, t = each frame's tiers.
    pub(crate) fn get_callers_var(
        &self,
        name: &str,
        depth: usize,
        cascade: bool,
    ) -> Result<Value, RuntimeError> {
        if !cascade {
            if !crate::parser::current_language_version_starts_with("6.e") {
                return self.caller_var_lookup(name, depth, false);
            }
            // 6.e: `CALLERS::` searches every caller from `depth` outward
            // (#18836), so a block or thunk the routine runs between it and
            // its caller does not hide the caller's symbol. The first frame
            // that has the name answers; one that is not dynamic is a Failure
            // of `X::Symbol::NotDynamic` (`CALLER::` still throws
            // `X::Caller::NotDynamic`).
            for depth in depth..=self.caller_env_stack.len() {
                match self.caller_frame_var(name, depth, false) {
                    Ok(Some(found)) => return Ok(found),
                    Ok(None) => {}
                    Err(err)
                        if err.exception.as_deref().is_some_and(|ex| {
                            matches!(ex.view(), ValueView::Instance { class_name, .. }
                                if class_name.as_str() == "X::Caller::NotDynamic")
                        }) =>
                    {
                        return Ok(Self::symbol_not_dynamic_failure(&format!("${name}")));
                    }
                    Err(err) => return Err(err),
                }
            }
            return Ok(Value::NIL);
        }
        let stack_len = self.caller_env_stack.len();
        if depth == 0 || depth > stack_len {
            // The program's mainline has no caller pad to walk, but its scope
            // still holds the routines it declares and imports.
            if depth > 0 && name.starts_with('&') {
                return Ok(self
                    .caller_scope_routine(&name[1..], false)
                    .unwrap_or(Value::NIL));
            }
            return Ok(Value::NIL);
        }
        // caller_env_stack[stack_len - depth] is the depth-th caller (depth 1 =
        // the immediate caller at the top); older callers are at lower indices.
        // Cascade from there toward index 0.
        // A lexical routine (`&name`) needs no `is dynamic`: the caller pads
        // are searched for the routine itself, and a routine declared in a
        // caller's compilation unit (not bound in its env) resolves by name.
        let is_routine = name.starts_with('&');
        for env in self.caller_env_stack[..=stack_len - depth].iter().rev() {
            // A routine is a lexical: it is found where the caller's own scope
            // binds it, following that frame's static link, not through the
            // frames that called it (an import bound in the importer's env is
            // not in the scope of a module routine that imported nothing).
            let found = if is_routine {
                env.get(name)
            } else {
                env.get_through_callers(name)
            };
            if let Some(val) = found
                && (is_routine || self.is_var_dynamic(name))
            {
                return Ok(val.clone());
            }
        }
        if is_routine {
            // A routine in a caller's lexical scope: declared by its file or
            // package, or bound in its unit's import sets rather than its pad.
            if let Some(found) = self.caller_scope_routine(&name[1..], false) {
                return Ok(found);
            }
            return Ok(self.resolve_code_var(&name[1..]));
        }
        Ok(Value::NIL)
    }
}
