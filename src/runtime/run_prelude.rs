use super::run::{NATIVECALL_POINTER_PRELUDE, RATIONAL_ROLE_PRELUDE};
use super::*;

impl Interpreter {
    /// Prepend builtin prelude role definitions to `stmts` when the source
    /// references them. Currently this provides the parametric `Rational` role
    /// for user classes written as `does Rational[...]`. The prelude is parsed
    /// once and cached.
    pub(super) fn inject_prelude_roles(source: &str, stmts: &mut Vec<Stmt>) {
        // Only inject when the program mentions `Rational` and does not declare
        // its own role of that name (which would conflict).
        if !source.contains("Rational") || source.contains("role Rational") {
            return;
        }
        use std::sync::OnceLock;
        static RATIONAL_STMTS: OnceLock<Vec<Stmt>> = OnceLock::new();
        let prelude = RATIONAL_STMTS.get_or_init(|| {
            crate::parse_dispatch::parse_source(RATIONAL_ROLE_PRELUDE)
                .map(|(s, _)| s)
                .unwrap_or_default()
        });
        if prelude.is_empty() {
            return;
        }
        let mut combined = prelude.clone();
        combined.append(stmts);
        *stmts = combined;
    }

    /// Prepend the builtin NativeCall `Pointer` class when a program that uses
    /// NativeCall references `Pointer` without declaring its own. Parsed once
    /// and cached, like [`inject_prelude_roles`].
    pub(super) fn inject_nativecall_prelude(source: &str, stmts: &mut Vec<Stmt>) {
        if !source.contains("NativeCall")
            || !source.contains("Pointer")
            || source.contains("class Pointer")
        {
            return;
        }
        use std::sync::OnceLock;
        static POINTER_STMTS: OnceLock<Vec<Stmt>> = OnceLock::new();
        let prelude = POINTER_STMTS.get_or_init(|| {
            crate::parse_dispatch::parse_source(NATIVECALL_POINTER_PRELUDE)
                .map(|(s, _)| s)
                .unwrap_or_default()
        });
        if prelude.is_empty() {
            return;
        }
        let mut combined = prelude.clone();
        combined.append(stmts);
        *stmts = combined;
    }

    pub(super) fn source_has_no_precompilation(code: &str) -> bool {
        code.lines().any(|line| {
            let trimmed = line.trim();
            trimmed == "no precompilation;"
                || trimmed == "no precompilation"
                || trimmed.starts_with("no precompilation;")
                || trimmed.starts_with("no precompilation ")
        })
    }

    fn direct_need_dependencies(source: &str) -> Vec<String> {
        let mut out = Vec::new();
        for line in source.lines() {
            let trimmed = line.trim_start();
            let Some(rest) = trimmed.strip_prefix("need ") else {
                continue;
            };
            let dep = rest
                .trim()
                .trim_end_matches(';')
                .trim()
                .trim_matches('"')
                .trim_matches('\'');
            if dep.is_empty() || dep.contains(char::is_whitespace) {
                continue;
            }
            out.push(dep.to_string());
        }
        out
    }

    pub(super) fn dependency_disables_precomp(&self, source: &str) -> bool {
        for dep in Self::direct_need_dependencies(source) {
            let Some((dep_path, _)) = self.resolve_module_path(&dep) else {
                continue;
            };
            let Ok(dep_code) = std::fs::read_to_string(dep_path) else {
                continue;
            };
            if Self::source_has_no_precompilation(&dep_code) {
                return true;
            }
        }
        false
    }

    /// Validate `package EXPORTHOW { ... }` directive members. An EXPORTHOW
    /// member named `<directive>::<declarator>` must use a recognized directive
    /// (DECLARE, SUPERSEDE, or COMPOSE); anything else is X::EXPORTHOW::InvalidDirective.
    /// A bare member name (no `::`) is shorthand for installing a package type and
    /// is always allowed.
    pub(super) fn validate_exporthow_directives(
        stmts: &[crate::ast::Stmt],
    ) -> Result<(), RuntimeError> {
        use crate::ast::Stmt;
        const VALID: [&str; 3] = ["DECLARE", "SUPERSEDE", "COMPOSE"];
        for stmt in stmts {
            let Stmt::Package { name, body, .. } = stmt else {
                continue;
            };
            if name.resolve() != "EXPORTHOW" {
                continue;
            }
            for member in body {
                let member_name = match member {
                    Stmt::ClassDecl { name, .. } | Stmt::RoleDecl { name, .. } => name.resolve(),
                    _ => continue,
                };
                if let Some((directive, _)) = member_name.split_once("::")
                    && !VALID.contains(&directive)
                {
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("directive".to_string(), Value::str(directive.to_string()));
                    attrs.insert(
                        "message".to_string(),
                        Value::str(format!("EXPORTHOW directive '{}' is unknown", directive)),
                    );
                    return Err(RuntimeError::typed("X::EXPORTHOW::InvalidDirective", attrs));
                }
            }
        }
        Ok(())
    }

    pub(super) fn should_skip_runtime_for_use_only_module(stmts: &[crate::ast::Stmt]) -> bool {
        if stmts.is_empty()
            || !stmts
                .iter()
                .all(|stmt| matches!(stmt, crate::ast::Stmt::Use { .. }))
        {
            return false;
        }
        let non_version_use_count = stmts
            .iter()
            .filter_map(|stmt| match stmt {
                crate::ast::Stmt::Use { module, .. } => Some(module.as_str()),
                _ => None,
            })
            .filter(|module| *module != "v6")
            .count();
        non_version_use_count > 1
    }

    pub(super) fn raku_single_quoted_literal(value: &str) -> String {
        let mut escaped = String::with_capacity(value.len() + 2);
        escaped.push('\'');
        for ch in value.chars() {
            match ch {
                '\'' => escaped.push_str("\\'"),
                '\\' => escaped.push_str("\\\\"),
                '\n' => escaped.push_str("\\n"),
                '\r' => escaped.push_str("\\r"),
                '\t' => escaped.push_str("\\t"),
                _ => escaped.push(ch),
            }
        }
        escaped.push('\'');
        escaped
    }

    /// Register top-level, non-empty sub bodies before execution so calls that appear
    /// earlier in source can resolve to later definitions.
    pub(crate) fn preregister_top_level_subs(
        &mut self,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let mut forward_sigs = std::collections::HashSet::new();
        for stmt in stmts {
            if let Stmt::SubDecl {
                name,
                params,
                param_defs,
                body,
                multi,
                ..
            } = stmt
            {
                if *multi || !body.is_empty() {
                    continue;
                }
                forward_sigs.insert(format!("{}|{:?}|{:?}", name, params, param_defs));
            }
        }

        for stmt in stmts {
            if let Stmt::SubDecl {
                name,
                params,
                param_defs,
                return_type,
                associativity,
                body,
                multi,
                is_rw,
                is_raw,
                is_export,
                is_test_assertion,
                supersede,
                ..
            } = stmt
            {
                if *multi || body.is_empty() {
                    continue;
                }
                let sig_key = format!("{}|{:?}|{:?}", name, params, param_defs);
                if !forward_sigs.contains(&sig_key) {
                    continue;
                }
                let name_str = name.resolve();
                self.register_sub_decl(
                    &name_str,
                    params,
                    param_defs,
                    return_type.as_ref(),
                    associativity.as_ref(),
                    body,
                    *multi,
                    *is_rw,
                    *is_raw,
                    *is_test_assertion,
                    *supersede,
                    &[],
                )?;
                if *is_export {
                    self.register_sub_decl_as_global(
                        &name_str,
                        params,
                        param_defs,
                        return_type.as_ref(),
                        associativity.as_ref(),
                        body,
                        *multi,
                        *is_rw,
                        *is_raw,
                        *is_test_assertion,
                        *supersede,
                    )?;
                }
            }
        }
        Ok(())
    }

    /// If the first non-Use/non-Package statement is a `unit class Foo;` (ClassDecl with empty
    /// body), merge all subsequent method/sub declarations into the class body.
    pub(super) fn merge_unit_class(stmts: Vec<Stmt>) -> Vec<Stmt> {
        // Find the index of a unit ClassDecl with empty body (from `unit class Foo;`)
        let class_idx = stmts.iter().position(
            |s| matches!(s, Stmt::ClassDecl { body, is_unit: true, .. } if body.is_empty()),
        );
        if let Some(idx) = class_idx {
            let mut result: Vec<Stmt> = stmts[..idx].to_vec();
            if let Stmt::ClassDecl {
                name,
                name_expr,
                parents,
                class_is_rw,
                is_hidden,
                is_lexical,
                hidden_parents,
                does_parents,
                repr,
                body: _,
                language_version,
                custom_traits,
                decl_id,
                ..
            } = &stmts[idx]
            {
                let body: Vec<Stmt> = stmts[idx + 1..].to_vec();
                result.push(Stmt::ClassDecl {
                    name: *name,
                    name_expr: name_expr.clone(),
                    parents: parents.clone(),
                    class_is_rw: *class_is_rw,
                    is_hidden: *is_hidden,
                    is_lexical: *is_lexical,
                    hidden_parents: hidden_parents.clone(),
                    does_parents: does_parents.clone(),
                    repr: repr.clone(),
                    body,
                    language_version: language_version.clone(),
                    custom_traits: custom_traits.clone(),
                    is_unit: true,
                    decl_id: *decl_id,
                });
            }
            result
        } else {
            stmts
        }
    }
}
