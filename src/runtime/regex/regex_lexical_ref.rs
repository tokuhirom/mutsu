//! `<&$re>` / `<&re>` — a subrule reference that names a *lexical* holding a
//! Regex value, rather than a rule in the grammar's registry.
//!
//! An anonymous declarator term (`token ($x) { … }`) produces a Regex value
//! with nowhere to register itself, so the only way to call it is through the
//! variable it was stored in:
//!
//! ```raku
//! my &tokenized = $hg.tokenize();      # returns `token ( Str:D $text ) { … }`
//! say so 'bαг' ~~ / <&tokenized: 'bar'> /;
//! ```
//!
//! The registry-backed path cannot serve that: there is no `token_defs` entry
//! to resolve, and the parameters live on the *value*
//! ([`Value::regex_signature`]). This module is the fallback the subrule
//! dispatcher consults when the registry lookup would come up empty — it finds
//! the lexical, binds the call's arguments to the value's signature, and hands
//! back an instantiated pattern in exactly the shape
//! `resolve_named_regex_candidates_in_pkg` produces for a named rule.
//!
//! The resolution reads the caller's lexical scope, so it is deliberately
//! *not* memoized: two calls of the same spelling in two scopes are two
//! different regexes.

use super::super::*;
use super::regex_helpers::NamedRegexLookupSpec;
use crate::symbol::Symbol;

impl Interpreter {
    /// Could this subrule reference name a lexical Regex? `<&x…>` sets
    /// `token_lookup`; `<&$x…>` additionally keeps the sigil on the name. A
    /// pure string test, so the overwhelmingly common `<rule>` reference pays
    /// nothing.
    pub(super) fn may_name_lexical_regex(spec: &NamedRegexLookupSpec) -> bool {
        spec.token_lookup || spec.lookup_name.starts_with('$')
    }

    /// The Regex value this reference resolves to, or `None` when it does not
    /// name a lexical Regex — or when a registry rule of the same name exists,
    /// which wins and keeps every existing `<&rule>` reference on its current
    /// path. The single decision point: both the candidate resolution and the
    /// closure-scope install must agree on whether the lexical is in play.
    fn resolve_lexical_regex(&mut self, spec: &NamedRegexLookupSpec, pkg: Symbol) -> Option<Value> {
        if !Self::may_name_lexical_regex(spec) {
            return None;
        }
        let value = self.lookup_lexical_regex(&spec.lookup_name)?;
        self.resolve_token_patterns_static_in_pkg(spec.lookup_name.trim_start_matches('$'), pkg)
            .is_empty()
            .then_some(value)
    }

    /// The lexical this reference names, when it holds a Regex. Reads exactly
    /// the env key the reference's sigil implies (see
    /// [`crate::value::RegexClosure`] for the keying convention).
    fn lookup_lexical_regex(&self, lookup_name: &str) -> Option<Value> {
        let bare = lookup_name.trim_start_matches(['&', '$']);
        if bare.is_empty() {
            return None;
        }
        // Exactly the binding the reference spells, as raku resolves it:
        // `<&x>` is the `&x` routine lexical and `<&$x>` the `$x` scalar (a
        // scalar is keyed sigil-less in `env`). Accepting either for either
        // would make `<&$x>` find a `my &x`, which raku rejects outright.
        let key = if lookup_name.starts_with('$') {
            bare.to_string()
        } else {
            format!("&{bare}")
        };
        let value = self.env.get(&key)?.clone().into_deref();
        matches!(
            value.view(),
            ValueView::Regex(_) | ValueView::RegexWithAdverbs(_)
        )
        .then_some(value)
    }

    /// Whether this reference actually resolves to a caller-scope Regex.
    /// Unqualified names are eligible for the lexical fallback because
    /// `my regex name` uses that spelling, but a normal package token/rule
    /// must still remain eligible for regex prefilter analysis.
    pub(super) fn lexical_regex_is_in_scope(&self, spec: &NamedRegexLookupSpec) -> bool {
        Self::may_name_lexical_regex(spec) && self.lookup_lexical_regex(&spec.lookup_name).is_some()
    }

    /// Candidates for a `<&lexical(args)>` reference, or `None` when it does
    /// not resolve to one (see [`Interpreter::resolve_lexical_regex`]).
    pub(super) fn lexical_regex_subrule_candidates(
        &mut self,
        spec: &NamedRegexLookupSpec,
        pkg: Symbol,
        arg_values: &[Value],
    ) -> Option<std::sync::Arc<Vec<super::regex_token_resolve::ParsedTokenCandidate>>> {
        let value = self.resolve_lexical_regex(spec, pkg)?;
        let pattern = self.instantiate_regex_value_with_args(&value, arg_values)?;
        let parsed = self.parse_candidate_in_pkg(&pattern, pkg)?;
        Some(std::sync::Arc::new(vec![(parsed, pkg, None)]))
    }

    /// Put the defining scope of a `<&lexical>` reference's Regex value into
    /// the env for the duration of that subrule's resolve-and-match, returning
    /// the shadowed bindings for the caller to restore.
    ///
    /// A regex is a closure over the scope its literal was written in, and an
    /// anonymous declarator returned from a sub (HomoGlypher's `tokenize`)
    /// reads that scope from its `<?{ … }>` code blocks — which run inline, in
    /// this env, where the cursor reaches them. The value carries the snapshot
    /// ([`Value::regex_closure_scope`]); only splicing the pattern text in, as
    /// the resolution does, would leave those names unbound at match time.
    pub(in crate::runtime) fn install_lexical_regex_closure_scope(
        &mut self,
        spec: &NamedRegexLookupSpec,
        pkg: Symbol,
    ) -> Option<super::regex_dynparams::SavedDynParams> {
        let scope = self
            .resolve_lexical_regex(spec, pkg)?
            .regex_closure_scope()?;
        let mut saved: super::regex_dynparams::SavedDynParams = Vec::with_capacity(scope.len());
        for (key, value) in scope.iter() {
            saved.push((key.clone(), self.env.get(key).cloned()));
            self.env.insert(key.clone(), value.clone());
        }
        (!saved.is_empty()).then_some(saved)
    }

    /// Bind `arg_values` to the signature a Regex value carries and render the
    /// resulting pattern. Mirrors the named-rule path
    /// (`resolve_one_token_pattern_with_args`): the parameters are bound in a
    /// scratch interpreter, then baked into the pattern's code blocks and
    /// interpolated into its text, because both run against the *caller's* env
    /// at match time and would otherwise never see them.
    fn instantiate_regex_value_with_args(
        &mut self,
        value: &Value,
        arg_values: &[Value],
    ) -> Option<String> {
        let pattern = match value.view() {
            ValueView::Regex(pat) => pat.to_string(),
            ValueView::RegexWithAdverbs(a) => a.pattern.to_string(),
            _ => return None,
        };
        let signature = value.regex_signature();
        let param_defs: &[crate::ast::ParamDef] =
            signature.as_deref().map(Vec::as_slice).unwrap_or(&[]);
        if param_defs.is_empty() && arg_values.is_empty() {
            return Some(pattern);
        }
        // The instantiation depends on the caller's lexicals and on the
        // arguments, neither of which the subrule memo key carries.
        super::regex_arg_purity::note_opaque_read();
        let mut interp = Interpreter {
            env: self.env.clone(),
            ..self.new_regex_scratch_sharing_io()
        };
        self.copy_decl_registry_into(&mut interp);
        // A regex is a closure over the scope its literal was written in; the
        // body of an anonymous declarator returned from a sub (HomoGlypher's
        // `tokenize`) routinely reads such a lexical.
        if let Some(scope) = value.regex_closure_scope() {
            for (key, captured) in scope.iter() {
                interp.env.insert(key.clone(), captured.clone());
            }
        }
        let names: Vec<String> = param_defs.iter().map(|pd| pd.name.clone()).collect();
        interp
            .bind_function_args_values(param_defs, &names, arg_values)
            .ok()?;
        let bare_names: Vec<String> = param_defs
            .iter()
            .filter(|pd| !pd.name.is_empty() && !pd.slurpy)
            .map(|pd| {
                pd.name
                    .trim_start_matches([':', '@', '%', '&', '!', '.'])
                    .to_string()
            })
            .collect();
        let pattern = interp.bake_bound_params_into_regex_code_blocks(&pattern, &bare_names);
        let pattern = interp.interpolate_bound_regex_scalars(&pattern);
        interp.instantiate_named_regex_arg_calls(&pattern).ok()
    }
}
