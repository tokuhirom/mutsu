//! Source EVAL entry points and parser retries. Unit isolation lives in
//! `system_eval_string` and is shared with AST EVAL.

use super::*;

#[path = "system_eval_helpers.rs"]
mod helpers;
use helpers::{
    looks_like_bracketed_statement_list, rewrite_prefixed_angle_list, unwrap_bracketed_statements,
    unwrap_parenthesized_statements,
};

impl Interpreter {
    pub(super) fn eval_eval_string(&mut self, code: &str) -> Result<Value, RuntimeError> {
        // An EVAL is a compunit of its own: a top-level `use` in it attaches
        // its LEAVE phasers to the EVAL, not to the enclosing program
        // (`runtime::attach_target`).
        //
        // What its `use` statements import is lexical to the EVAL too (#11069,
        // `runtime::eval_import_scope`). The scope is opened outside the
        // compunit, so a top-level `use` still sees no enclosing 'block'.
        self.with_eval_import_scope(|interp| {
            interp.run_compunit(|interp| interp.eval_eval_string_unit(code))
        })
    }

    fn eval_eval_string_unit(&mut self, code: &str) -> Result<Value, RuntimeError> {
        let trimmed = code.trim();
        if trimmed == "<>" || trimmed == "<STDIN>" {
            return Err(RuntimeError::obsolete(
                trimmed,
                "lines() to read input, ('') to represent a null string or () to \
                 represent an empty list",
            ));
        }
        // The EVAL is the unit `$*CU` names while its BEGIN prologue and CHECK
        // phase run.
        // Leading blank lines stay: a diagnosis's `.line` counts them, as in
        // rakudo (#18870).
        let source = code.trim_end();
        let _unit = crate::unit_source_text::UnitSourceTextGuard::enter(source);
        self.isolated_eval_unit(|interp| interp.eval_eval_source(source))
    }

    /// Record the classes a dying declaration added, if an `EVAL` unit is
    /// compiling it (see `ModuleState::eval_failed_classes`).
    // Cost: O(c), c = classes registered.
    pub(crate) fn note_failed_eval_class_decl(
        &mut self,
        classes_before: &crate::runtime::registry::ClassTable,
    ) {
        if self.env.get("__mutsu_in_eval").is_none_or(|v| !v.truthy()) {
            return;
        }
        let added: Vec<String> = self
            .registry()
            .classes
            .keys()
            .filter(|name| !classes_before.contains_key(name))
            .cloned()
            .collect();
        self.module.eval_failed_classes.extend(added);
    }

    /// Parse and run the source of a string `EVAL`, retrying the snippet
    /// shapes the parser rejects at unit scope.
    fn eval_eval_source(&mut self, trimmed: &str) -> Result<Value, RuntimeError> {
        // A `:key<>` colonpair (empty angle brackets) in the EVAL'd source's Pod
        // is a fatal compile error in Raku; short-circuit before evaluating.
        let pod_err = self.collect_pod_blocks(trimmed).err();
        let pod_failed = pod_err.is_some();
        // Collect operator sub names so the parser recognizes them in EVAL context
        let op_names = self.collect_operator_sub_names();
        let op_assoc = self.collect_operator_assoc_map();
        let bracketed_stmt_inner = unwrap_bracketed_statements(trimmed)
            .filter(|inner| looks_like_bracketed_statement_list(inner));
        // General case: parse and evaluate as Raku code
        let mut result = if let Some(err) = pod_err {
            Err(err)
        } else if let Some(inner) = bracketed_stmt_inner {
            // EVAL q[[ ... ]] can yield one wrapper [] around statement lists.
            self.parse_and_eval_with_operators(inner, &op_names, &op_assoc)
                .or_else(|_| self.parse_and_eval_with_operators(trimmed, &op_names, &op_assoc))
        } else {
            self.parse_and_eval_with_operators(trimmed, &op_names, &op_assoc)
        };
        self.emit_parse_warnings(crate::parser::take_parse_warnings());
        // Fallback: parser still rejects forms like `~< foo bar >`.
        // Rewrite to an equivalent parenthesized form and try again.
        if !pod_failed
            && result.is_err()
            && let Some(rewritten) = rewrite_prefixed_angle_list(trimmed)
        {
            result = self.parse_and_eval_with_operators(&rewritten, &op_names, &op_assoc);
        }
        // Accept parenthesized statement lists like `(6;)` in EVAL.
        if !pod_failed
            && result.is_err()
            && let Some(inner) = unwrap_parenthesized_statements(trimmed)
        {
            result = self.parse_and_eval_with_operators(inner, &op_names, &op_assoc);
        }
        // EVAL q[[ ... ]] sometimes carries one outer statement-list bracket pair.
        if !pod_failed
            && result.is_err()
            && bracketed_stmt_inner.is_none()
            && let Some(inner) = unwrap_bracketed_statements(trimmed)
        {
            result = self.parse_and_eval_with_operators(inner, &op_names, &op_assoc);
        }
        // EVAL should accept routine declarations in snippet context.
        // If unit-scope parsing rejects a declaration, retry inside an implicit block.
        if !pod_failed
            && result.is_err()
            && trimmed.contains("sub ")
            && let Some(err) = result.as_ref().err()
            && err.message.contains("X::UnitScope::Invalid")
        {
            let wrapped = format!("{{ {}; }}", trimmed);
            let saved_wrapped_eval = self.env.get("__mutsu_eval_wrapped_decls").cloned();
            self.env
                .insert("__mutsu_eval_wrapped_decls".to_string(), Value::TRUE);
            result = self.parse_and_eval_with_operators(&wrapped, &op_names, &op_assoc);
            if let Some(saved) = saved_wrapped_eval {
                self.env
                    .insert("__mutsu_eval_wrapped_decls".to_string(), saved);
            } else {
                self.env.remove("__mutsu_eval_wrapped_decls");
            }
        }
        result
    }
}
