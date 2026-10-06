use super::run::{
    ENUMERATION_ROLE_PRELUDE, IO_SOCKET_ROLE_PRELUDE, METAMODEL_ROLE_PRELUDE,
    RATIONAL_ROLE_PRELUDE, TRAIT_MOD_DOES_PRELUDE, TRAIT_MOD_IS_DEFAULT_PRELUDE,
    X_WRAPPER_ROLE_PRELUDE,
};
use super::source_code_text::CodeText;
use super::*;

impl Interpreter {
    /// Prepend builtin prelude role definitions to `stmts` when the source
    /// references them. Currently this provides the parametric `Rational` role
    /// for user classes written as `does Rational[...]`. The prelude is parsed
    /// once and cached.
    ///
    /// Like every gate in this file it is asked of a [`CodeText`], not of the
    /// raw source: a name that appears only in a comment or a Pod block is
    /// prose, and prose must not switch a prelude on (GH #7611).
    pub(super) fn inject_prelude_roles(source: &CodeText<'_>, stmts: &mut Vec<Stmt>) {
        // Only inject when the program mentions `Rational` and does not declare
        // its own role of that name (which would conflict).
        if !source.contains("Rational") || source.contains("role Rational") {
            return;
        }
        use std::sync::OnceLock;
        static RATIONAL_STMTS: OnceLock<Vec<Stmt>> = OnceLock::new();
        let prelude = RATIONAL_STMTS.get_or_init(|| {
            crate::runtime::prelude_source::parse_prelude_source(RATIONAL_ROLE_PRELUDE)
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

    /// Stamp every routine declaration in a parsed prelude with the internal
    /// `__mutsu_prelude` trait. Marker traits are `__`-prefixed by convention
    /// (`__our_scoped`, `__lexical_hoist`) so registration can tell them from a
    /// user trait and never tries to apply one as a role.
    pub(super) fn mark_prelude_subs(stmts: &mut [Stmt]) {
        for stmt in crate::ast::scope_members_mut(stmts) {
            if let Stmt::SubDecl { custom_traits, .. } = stmt {
                custom_traits.push((crate::runtime::PRELUDE_SUB_TRAIT.to_string(), None));
            }
        }
    }

    /// Prepend the builtin `IO::Socket` role when a module/program composes it
    /// (`does IO::Socket`) without declaring its own. Enables the community
    /// `IO::Socket::SSL` binding, whose class header is
    /// `class IO::Socket::SSL does IO::Socket`. Parsed once and cached.
    pub(super) fn inject_iosocket_prelude(source: &CodeText<'_>, stmts: &mut Vec<Stmt>) {
        if !source.contains("does IO::Socket") || source.contains("role IO::Socket") {
            return;
        }
        use std::sync::OnceLock;
        static IO_SOCKET_STMTS: OnceLock<Vec<Stmt>> = OnceLock::new();
        let prelude = IO_SOCKET_STMTS.get_or_init(|| {
            crate::runtime::prelude_source::parse_prelude_source(IO_SOCKET_ROLE_PRELUDE)
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

    /// Prepend CORE's `trait_mod:<is>(Attribute:D $attr, :$default!)` candidate
    /// (see [`TRAIT_MOD_IS_DEFAULT_PRELUDE`]) to a program that names
    /// `trait_mod:<is>` at all. This
    /// candidate's `:default!` shape is CORE.setting's own and is vanishingly
    /// unlikely to collide with a distribution's own candidate for some other
    /// named trait (`ASN::Types` declares four `trait_mod:<is>` candidates of
    /// its own — `:$UTF8String`, `:$OctetString`, `:$optional`,
    /// `:$default-value` — none of which is `:default`, and all four keep
    /// dispatching to their own bodies exactly as before).
    pub(super) fn inject_trait_mod_is_default_prelude(
        source: &CodeText<'_>,
        stmts: &mut Vec<Stmt>,
    ) {
        // Gated on the `:default(` call-site spelling, not merely on the
        // routine's name: adding this candidate at all — even one nothing
        // ever dispatches to — changes `trait_mod:<is>` from a single- to a
        // multi-candidate routine, and a file that reflects on its OWN
        // `&trait_mod:<is>` (`OUR::{'&trait_mod:<is>'} := &trait_mod:<is>;`,
        // re-exporting a re-imported trait handler under `OUR::`) resolves
        // that reference through the by-name multi-dispatch path only once
        // 2+ candidates exist, and recurses infinitely doing so (a
        // stack-overflow crash, not a wrong answer — see
        // `t/modules/import-export/our-trait-mod-reexport.t`). Requiring the
        // actual `:default(...)` argument shape ASN::BER's own re-dispatch
        // uses keeps the gate closed for every file that merely declares or
        // re-exports a trait_mod:<is> candidate of its own, at the cost of
        // also gating on files that spell it `:default` without parens
        // (a bare `:$default` boolean shorthand) — accepted, since a `has
        // default(...)` cross-check has no such shorthand form in practice.
        if !source.contains("trait_mod:<is>") || !source.contains(":default(") {
            return;
        }
        use std::sync::OnceLock;
        static TRAIT_MOD_IS_DEFAULT_STMTS: OnceLock<Vec<Stmt>> = OnceLock::new();
        let prelude = TRAIT_MOD_IS_DEFAULT_STMTS.get_or_init(|| {
            let mut stmts =
                crate::runtime::prelude_source::parse_prelude_source(TRAIT_MOD_IS_DEFAULT_PRELUDE)
                    .map(|(s, _)| s)
                    .unwrap_or_default();
            Self::mark_prelude_subs(&mut stmts);
            stmts
        });
        if prelude.is_empty() {
            return;
        }
        let mut combined = prelude.clone();
        combined.append(stmts);
        *stmts = combined;
    }

    pub(super) fn inject_trait_mod_does_prelude(source: &CodeText<'_>, stmts: &mut Vec<Stmt>) {
        if !source.contains("trait_mod:<does>") {
            return;
        }
        use std::sync::OnceLock;
        static TRAIT_MOD_DOES_STMTS: OnceLock<Vec<Stmt>> = OnceLock::new();
        let prelude = TRAIT_MOD_DOES_STMTS.get_or_init(|| {
            crate::runtime::prelude_source::parse_prelude_source(TRAIT_MOD_DOES_PRELUDE)
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

    /// Prepend the `Metamodel::Naming` / `Metamodel::Stashing` metaroles
    /// ([`METAMODEL_ROLE_PRELUDE`]) to a program that composes either of them.
    ///
    /// Gated on the role names themselves rather than on a bare `Metamodel`:
    /// `Metamodel::Primitives` is dispatched natively and needs nothing
    /// injected, and `.^name`/`.^compose` mention no `Metamodel::` name at all,
    /// so keying on the prefix would prepend two role declarations to most
    /// programs that merely introspect. A program declaring its own role of
    /// either name keeps it (the prelude would collide with it), exactly as
    /// [`Self::inject_prelude_roles`] treats `role Rational`.
    pub(super) fn inject_metamodel_role_prelude(source: &CodeText<'_>, stmts: &mut Vec<Stmt>) {
        let wants_naming =
            source.contains("Metamodel::Naming") && !source.contains("role Metamodel::Naming");
        let wants_stashing =
            source.contains("Metamodel::Stashing") && !source.contains("role Metamodel::Stashing");
        if !wants_naming && !wants_stashing {
            return;
        }
        use std::sync::OnceLock;
        static METAMODEL_ROLE_STMTS: OnceLock<Vec<Stmt>> = OnceLock::new();
        let prelude = METAMODEL_ROLE_STMTS.get_or_init(|| {
            crate::runtime::prelude_source::parse_prelude_source(METAMODEL_ROLE_PRELUDE)
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

    /// Prepend the builtin `Enumeration` role ([`ENUMERATION_ROLE_PRELUDE`]) to
    /// a program (or module) that mentions it.
    ///
    /// Gated on the name like the other role preludes, and skipped when the
    /// compunit declares its own `role Enumeration` (which the prelude would
    /// collide with). Injected for MODULES too, not only the main program: the
    /// distribution that motivated this is `Logic::Ternary`, whose
    /// `class Logic::Ternary does Enumeration` lives in the module, while the
    /// program that loads it (`use Logic::Ternary;`) never names `Enumeration`
    /// at all — so gating on the main program's source alone would never fire.
    pub(super) fn inject_enumeration_prelude(source: &CodeText<'_>, stmts: &mut Vec<Stmt>) {
        if !source.contains("Enumeration") || source.contains("role Enumeration") {
            return;
        }
        use std::sync::OnceLock;
        static ENUMERATION_STMTS: OnceLock<Vec<Stmt>> = OnceLock::new();
        let prelude = ENUMERATION_STMTS.get_or_init(|| {
            crate::runtime::prelude_source::parse_prelude_source(ENUMERATION_ROLE_PRELUDE)
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

    /// Prepend the builtin `X::Wrapper` role ([`X_WRAPPER_ROLE_PRELUDE`]) to a
    /// program (or module) that mentions it.
    ///
    /// Gated on the name like the other role preludes, and skipped when the
    /// compunit declares its own `role X::Wrapper` (which the prelude would
    /// collide with). Injected for MODULES too, not only the main program:
    /// `AttrX::Mooish`'s `AttrX::Mooish::X.rakumod` -- the distribution that
    /// motivated this -- references `X::Wrapper` from inside a `BEGIN {
    /// ::?CLASS.^add_role(::('X::Wrapper')) }`, while the program that loads
    /// it (`use AttrX::Mooish;`) never names `X::Wrapper` at all.
    pub(super) fn inject_x_wrapper_prelude(source: &CodeText<'_>, stmts: &mut Vec<Stmt>) {
        if !source.contains("X::Wrapper") || source.contains("role X::Wrapper") {
            return;
        }
        use std::sync::OnceLock;
        static X_WRAPPER_STMTS: OnceLock<Vec<Stmt>> = OnceLock::new();
        let prelude = X_WRAPPER_STMTS.get_or_init(|| {
            crate::runtime::prelude_source::parse_prelude_source(X_WRAPPER_ROLE_PRELUDE)
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

    // Cost: O(n), n = bytes of `code`; one substring search when the
    // directive is absent (the common case), a line scan only when present.
    pub(crate) fn source_has_no_precompilation(code: &str) -> bool {
        if !code.contains("no precompilation") {
            return false;
        }
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
        if !source.contains("need ") {
            return out;
        }
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
                if let Some((directive, _)) =
                    crate::qualified::split_first(crate::qualified::known_symbol(&member_name))
                    && !VALID.contains(&directive)
                {
                    let mut attrs = ValueMap::default();
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

    /// Eagerly compile a forward-declared top-level sub's full body so
    /// `preregister_top_level_subs` can install it with bytecode already
    /// attached, instead of leaving `compiled` unset and letting the first
    /// call between the forward stub and the real declaration compile it on
    /// demand (ADR-0019 C7 — the routine registry must not compile a
    /// migrated declaration lazily). This runs before the mainline is
    /// compiled, so it reuses the same on-the-fly compiler `otf_compile_function_def`
    /// already falls back to for a body-less def, just eagerly rather than
    /// at first call; the throwaway `FunctionDef` below only carries the
    /// fields that compile step reads.
    pub(super) fn compile_forward_declared_sub(
        &mut self,
        name: Symbol,
        params: &[String],
        param_defs: &[ParamDef],
        body: &[Stmt],
        is_rw: bool,
        is_raw: bool,
    ) -> std::sync::Arc<crate::opcode::CompiledFunction> {
        let tmp_def = crate::ast::FunctionDef {
            is_cached: false,
            package: Symbol::intern(&self.current_package()),
            name,
            params: params.to_vec(),
            param_defs: param_defs.to_vec(),
            body: body.to_vec(),
            is_test_assertion: false,
            is_implementation_detail: false,
            is_rw,
            is_raw,
            declarator: crate::ast::RoutineDeclarator::Sub,
            empty_sig: params.is_empty() && param_defs.is_empty(),
            is_stub: false,
            return_type: None,
            is_default: false,
            deprecated_message: None,
            op_prec: None,
            source_file: self.current_source_file(),
            source_line: None,
            decl_order: 0,
            compiled: None,
            dispatchee: None,
            body_fp_cache: std::sync::OnceLock::new(),
            captured_readonly: None,
            body_facts_cache: std::sync::OnceLock::new(),
            routine_cell: Default::default(),
        };
        self.otf_compile_function_def(&tmp_def)
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
                let metadata = crate::opcode::compiled_routine_metadata(
                    params,
                    param_defs,
                    body,
                    return_type.as_ref(),
                    *is_rw,
                    *is_raw,
                );
                let compiled = self
                    .compile_forward_declared_sub(*name, params, param_defs, body, *is_rw, *is_raw);
                self.register_compiled_sub_decl(
                    &name_str,
                    params,
                    param_defs,
                    return_type.as_ref(),
                    associativity.as_ref(),
                    &[],
                    *multi,
                    *is_rw,
                    *is_raw,
                    *is_test_assertion,
                    *supersede,
                    &[],
                    None,
                    &metadata,
                    Some(&compiled),
                )?;
                if *is_export {
                    let saved_pkg = self.current_package();
                    self.set_current_package("GLOBAL".to_string());
                    let global_result = self.register_compiled_sub_decl(
                        &name_str,
                        params,
                        param_defs,
                        return_type.as_ref(),
                        associativity.as_ref(),
                        &[],
                        *multi,
                        *is_rw,
                        *is_raw,
                        *is_test_assertion,
                        *supersede,
                        &[],
                        None,
                        &metadata,
                        Some(&compiled),
                    );
                    self.set_current_package(saved_pkg);
                    global_result?;
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// ADR-0019 C7: `preregister_top_level_subs` must install the early
    /// full-body candidate with bytecode already attached, not leave
    /// `compiled` unset for the first call between the forward stub and the
    /// real declaration to compile on demand.
    #[test]
    fn forward_declared_sub_installs_with_compiled_bytecode() {
        let mut interp = Interpreter::new();
        let (stmts, _) =
            crate::parse_dispatch::parse_source("sub add($a, $b); sub add($a, $b) { $a + $b }")
                .expect("parse");
        interp
            .preregister_top_level_subs(&stmts)
            .expect("preregister");
        let key = Symbol::intern(&format!("{}::add", interp.current_package()));
        let registry = interp.registry();
        let def = registry
            .functions
            .get(&key)
            .expect("add should be registered");
        assert!(
            def.compiled.is_some(),
            "preregistration must attach compiled bytecode instead of leaving \
             the first call to compile the body on demand"
        );
    }

    /// The prelude clash check reads the unit's own scope: through a
    /// `unit module` wrapper and a `SyntheticBlock` group, never into a block
    /// (where a routine is a lexical of its own and cannot clash), and a
    /// `proto sub` declares the name too.
    #[test]
    fn declares_toplevel_sub_reads_the_unit_scope() {
        let declares = |src: &str| {
            let (stmts, _) = crate::parse_dispatch::parse_source(src).expect("parse");
            Interpreter::declares_toplevel_sub(&stmts, "refresh")
        };
        assert!(declares("sub refresh($x) { $x }"));
        assert!(declares("unit module Foo; sub refresh($x) { $x }"));
        assert!(declares("proto sub refresh(|) {*}"));
        assert!(declares("sub refresh($x) { $x }(1);"));
        assert!(!declares("{ sub refresh($x) { $x } }"));
        assert!(!declares("module Foo { sub refresh($x) { $x } }"));
    }
}
