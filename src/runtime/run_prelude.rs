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

    /// Execute top-level `BEGIN { ... }` phaser blocks at compile time (before
    /// the mainline runs), so that a read appearing textually *before* the
    /// BEGIN in source order observes its side effects. In Raku, BEGIN runs at
    /// compile time; mutsu otherwise compiles it inline and runs it in source
    /// position, so e.g. `my @a; my $c = @a.elems; BEGIN { @a = 1,2,3 }` would
    /// see `$c == 0` instead of `3`.
    ///
    /// Only "hoistable" BEGIN blocks are pre-run: their body must declare no
    /// outer-visible symbols (`sub`/`class`/`role`/...). A hoisted BEGIN runs
    /// via the sub-interpreter (`eval_block_value`), whose plain lexical writes
    /// persist into the shared `env` (only `&`-callable keys are isolated), and
    /// it is then removed from `body_main` so it does not run a second time.
    ///
    /// If pre-running a BEGIN throws — e.g. it references a `class` declared
    /// later in source that is not yet registered — the `env` is rolled back
    /// and the BEGIN is left in place to run at its original position, so
    /// dependency cases keep working exactly as before.
    pub(crate) fn run_toplevel_begin_phasers(&mut self, body_main: &mut Vec<Stmt>) {
        let has_candidate = body_main.iter().any(|s| {
            matches!(s, Stmt::Phaser { kind: crate::ast::PhaserKind::Begin, body }
                if Self::begin_body_is_hoistable(body))
        });
        if !has_candidate {
            return;
        }
        let mut new_body: Vec<Stmt> = Vec::with_capacity(body_main.len());
        let mut hoisted_any = false;
        for stmt in std::mem::take(body_main) {
            let hoistable_body = match &stmt {
                Stmt::Phaser {
                    kind: crate::ast::PhaserKind::Begin,
                    body,
                } if Self::begin_body_is_hoistable(body) => Some(body),
                _ => None,
            };
            if let Some(body) = hoistable_body {
                let snapshot = self.env.clone();
                match self.eval_block_value(body) {
                    Ok(_) => {
                        hoisted_any = true;
                        continue; // consumed: dropped from the mainline
                    }
                    Err(_) => {
                        // Roll back partial writes and keep the BEGIN in place
                        // so it runs (successfully) at its original source
                        // position during the mainline.
                        self.env = snapshot;
                    }
                }
            }
            new_body.push(stmt);
        }
        if hoisted_any {
            // A hoisted BEGIN wrote its lexicals into `env` at compile time. The
            // mainline still contains the bare `my @a;` declarations for those
            // vars, which would reset them to empty. Drop the no-init reset for
            // any variable the BEGIN already populated so the seeded value
            // survives into the mainline (Raku creates the container once, at
            // compile time; the runtime declaration does not re-initialize it).
            Self::drop_seeded_noinit_decls(&mut new_body, &self.env);
        }
        *body_main = new_body;
    }

    /// A top-level BEGIN is safe to pre-execute at compile time only when its
    /// body is a self-contained sequence of value assignments — no
    /// declarations, no barewords, and no calls. The narrowness is deliberate:
    /// the pre-run goes through the `eval_block_value` sub-interpreter, whose
    /// semantics diverge from the mainline VM for name resolution and for
    /// sink-context error forcing, so anything that could resolve a symbol or
    /// raise an exception must stay on the mainline (in-place) path instead.
    ///
    /// - Declarations (`sub`/`class`/`role`/...) inside a BEGIN are not visible
    ///   outside it in Raku anyway, and pre-running then discarding the
    ///   sub-interpreter's registries must not silently drop one.
    /// - A bareword may name a `class`/`role`/`enum`/type object not registered
    ///   until the mainline runs, so pre-executing would capture a wrong value.
    /// - A call may target a routine not yet registered at compile time, or may
    ///   throw — both of which are handled correctly only by the mainline path
    ///   (which wraps BEGIN-time errors in `X::Comp::BeginTime`).
    ///
    /// The `BareWord(`/`Call` markers are matched in the AST debug form, which
    /// catches them at any nesting depth without a bespoke full-AST walker. All
    /// call-shaped `Expr` variants (`Call`, `MethodCall`, `HyperMethodCall`,
    /// `CallOn`, `DynamicMethodCall`, ...) contain `Call` in their name.
    fn begin_body_is_hoistable(body: &[Stmt]) -> bool {
        let has_decl = body.iter().any(|s| {
            matches!(
                s,
                Stmt::SubDecl { .. }
                    | Stmt::ClassDecl { .. }
                    | Stmt::RoleDecl { .. }
                    | Stmt::EnumDecl { .. }
                    | Stmt::Package { .. }
                    | Stmt::ProtoDecl { .. }
                    | Stmt::MethodDecl { .. }
                    | Stmt::SubsetDecl { .. }
                    | Stmt::TokenDecl { .. }
                    | Stmt::RuleDecl { .. }
                    | Stmt::Use { .. }
            )
        });
        if has_decl {
            return false;
        }
        let debug = format!("{body:?}");
        !debug.contains("BareWord(") && !debug.contains("Call")
    }

    /// Remove the no-init reset of plain `my`/`our`-free declarations whose
    /// variable a hoisted top-level BEGIN already populated in `env`. Descends
    /// one level into `SyntheticBlock` (which carries `my (@a, @b)` group
    /// declarations). Only plain, unadorned, empty-default declarations are
    /// touched — anything with a type constraint, trait, `state`/`our`/`is
    /// dynamic`/`is export`, or a real initializer is left intact.
    fn drop_seeded_noinit_decls(body: &mut Vec<Stmt>, env: &crate::env::Env) {
        body.retain_mut(|stmt| match stmt {
            Stmt::SyntheticBlock(inner) => {
                Self::drop_seeded_noinit_decls(inner, env);
                !inner.is_empty()
            }
            _ => match Self::plain_noinit_vardecl_name(stmt) {
                Some(name) => env.get(name).is_none(),
                None => true,
            },
        });
    }

    /// If `stmt` is a plain `my $x;` / `my @a;` / `my %h;` with the synthesized
    /// empty-container default and no adornments, return the variable name (in
    /// env-key form). Otherwise `None`.
    fn plain_noinit_vardecl_name(stmt: &Stmt) -> Option<&str> {
        if let Stmt::VarDecl {
            name,
            expr,
            type_constraint,
            is_state,
            is_our,
            is_dynamic,
            is_export,
            custom_traits,
            where_constraint,
            ..
        } = stmt
        {
            if *is_state
                || *is_our
                || *is_dynamic
                || *is_export
                || type_constraint.is_some()
                || !custom_traits.is_empty()
                || where_constraint.is_some()
            {
                return None;
            }
            let is_default = match expr {
                Expr::Literal(v) => match v.view() {
                    ValueView::Nil => true,
                    ValueView::Array(d, _) => d.items.is_empty(),
                    _ => false,
                },
                Expr::Hash(items) => items.is_empty(),
                _ => false,
            };
            if is_default {
                return Some(name.as_str());
            }
        }
        None
    }
}
