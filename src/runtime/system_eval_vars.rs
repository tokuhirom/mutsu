//! The compile-time variable checks of an `EVAL`'d snippet: placeholders in
//! the mainline, and variables used without a declaration in scope.
//!
//! The undeclared-variable scan itself lives in `eval_var_scan.rs`.

use super::*;

impl Interpreter {
    /// Placeholder variables ($^x, @_, ...) used directly in the mainline are
    /// X::Placeholder::Mainline. This must take precedence over the undeclared
    /// check (otherwise `@_` would be reported as X::Undeclared).
    pub(crate) fn check_eval_mainline_placeholders(
        &self,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        // A placeholder inside a regex code block takes no signature at all
        // (mutsu#10542).
        if let Some(ph) = crate::ast::regex_placeholders::find_regex_code_block_placeholder(stmts) {
            let mut attrs = ValueMap::default();
            attrs.insert("placeholder".to_string(), Value::str(ph.clone()));
            attrs.insert(
                "message".to_string(),
                Value::str(format!(
                    "Placeholder variable '{ph}' may not be used here because the \
                     surrounding block does not take a signature."
                )),
            );
            return Err(RuntimeError::typed("X::Placeholder::Block", attrs));
        }
        if let Some(ph) = crate::ast::collect_unattached_placeholders(stmts)
            .into_iter()
            .next()
        {
            let mut attrs = ValueMap::default();
            attrs.insert("placeholder".to_string(), Value::str(ph.clone()));
            attrs.insert(
                "message".to_string(),
                Value::str(format!(
                    "Cannot use placeholder parameter {} outside of a sub or block",
                    ph
                )),
            );
            return Err(RuntimeError::typed("X::Placeholder::Mainline", attrs));
        }
        Ok(())
    }

    /// Reject a variable used where no declaration of it is in scope — in
    /// the snippet itself or in the caller's environment — as rakudo does at
    /// compile time (X::Undeclared, with "Did you mean" suggestions).
    // Cost: see `eval_var_scan::first_undeclared_var`.
    pub(crate) fn check_eval_undeclared_vars(&self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        match super::eval_var_scan::first_undeclared_var(self, stmts) {
            Some(found) => Err(Self::undeclared_var_error(found)),
            None => Ok(()),
        }
    }

    /// Reject a `:=` whose target is a lexical of the scope around the
    /// `EVAL` (`my $x; EVAL q[$x := 5]`), as rakudo does while compiling the
    /// `EVAL`: `X::Bind`, "Cannot use bind operator with this left-hand side".
    /// A bind to a variable the snippet declares itself is untouched.
    // Cost: see `eval_var_scan::first_outer_bind`.
    pub(crate) fn check_eval_outer_binds(&self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        if super::eval_var_scan::first_outer_bind(self, stmts).is_none() {
            return Ok(());
        }
        let message = "Cannot use bind operator with this left-hand side";
        let mut attrs = ValueMap::default();
        attrs.insert("message".to_string(), Value::str(message.to_string()));
        Err(RuntimeError::typed("X::Bind", attrs))
    }

    /// Reject, at CHECK time, a variable the unit uses before its top level
    /// declares it (`sub f { $zz = 42 }; my $zz = 1`), as rakudo does
    /// (#12800). Without it a routine resolved the name at run time and
    /// wrote its caller's same-named lexical.
    // Cost: see `eval_var_scan::first_var_used_before_unit_declares`.
    pub(crate) fn check_routine_forward_vars(&self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        match super::eval_var_scan::first_var_used_before_unit_declares(self, stmts) {
            Some(found) => {
                let mut err = Self::undeclared_var_error(found);
                err.set_code(Some(crate::value::RuntimeErrorCode::ParseGeneric));
                Err(err)
            }
            None => Ok(()),
        }
    }

    /// The `X::Undeclared` for the variable `(sigil, name, suggestions)`.
    fn undeclared_var_error(
        (sigil, var_name, suggestions): super::eval_var_scan::Undeclared,
    ) -> RuntimeError {
        let symbol = format!("{}{}", sigil, var_name);
        let mut attrs = ValueMap::default();
        attrs.insert("name".to_string(), Value::str(symbol.clone()));
        attrs.insert("symbol".to_string(), Value::str(symbol.clone()));
        // `post` is the source text following the eject point. For a bare
        // undeclared variable reference, that is the symbol itself.
        attrs.insert("post".to_string(), Value::str(symbol.clone()));
        // `highexpect` is the list of additional things the parser was
        // still expecting at the eject point. For an undeclared variable
        // there is nothing else expected, so it is an empty list.
        attrs.insert("highexpect".to_string(), Value::array(vec![]));
        let mut message = format!("Variable '{}' is not declared.", symbol);
        if suggestions.len() == 1 {
            message.push_str(&format!(" Did you mean '{}'?", suggestions[0]));
        } else if suggestions.len() > 1 {
            let quoted: Vec<String> = suggestions.iter().map(|s| format!("'{}'", s)).collect();
            message.push_str(&format!(
                " Did you mean any of these: {}?",
                quoted.join(", ")
            ));
        }
        attrs.insert(
            "suggestions".to_string(),
            Value::array(suggestions.into_iter().map(Value::str).collect()),
        );
        attrs.insert("message".to_string(), Value::str(message));
        RuntimeError::typed("X::Undeclared", attrs)
    }
}
