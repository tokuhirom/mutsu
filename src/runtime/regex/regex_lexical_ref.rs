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
        spec.token_lookup || spec.lookup_name.starts_with(['$', '{'])
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
        if spec.lookup_name.starts_with('{') {
            return self.eval_alias_code_regex(&spec.lookup_name);
        }
        let value = self.lookup_lexical_regex(&spec.lookup_name, !spec.token_lookup)?;
        self.resolve_token_patterns_static_in_pkg(spec.lookup_name.trim_start_matches('$'), pkg)
            .is_empty()
            .then_some(value)
    }

    /// The lexical this reference names, when it holds a Regex. Reads exactly
    /// the env key the reference's sigil implies (see
    /// [`crate::value::RegexClosure`] for the keying convention).
    ///
    /// With `str_is_pattern`, a Str value is a pattern, as for `<{ code }>`:
    /// an alias call `<a=$s>` interpolates it (#10673). A `<&$s>` call does
    /// not -- rakudo refuses to call a Str.
    fn lookup_lexical_regex(&self, lookup_name: &str, str_is_pattern: bool) -> Option<Value> {
        let bare = lookup_name.trim_start_matches(['&', '$']);
        // `$*dyn` is filed under its twigil-ful name (`*dyn`), which the
        // sigil-less scalar key below already spells.
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
        match value.view() {
            ValueView::Regex(_) | ValueView::RegexWithAdverbs(_) => Some(value),
            // `my &c = &re` / a `&class` parameter bound to a `my regex`:
            // the `&re` reference captured that declaration's Regex value
            // (`Value::routine_token_capture`), which is what `<&c>` calls.
            ValueView::Routine {
                is_regex: true,
                captured_regex: Some(regex),
                ..
            } => Some((**regex).clone()),
            ValueView::Str(s) if str_is_pattern => Some(Value::regex(s.to_string())),
            _ => None,
        }
    }

    /// The Regex a `<name={ code }>` alias matches: `code` run at match time,
    /// in the caller's env (where a rule's `$*` parameters are bound for its
    /// match window). A Str result is a pattern, as for `<{ code }>`.
    // Cost: one run of `code` (its parse is cached by text).
    fn eval_alias_code_regex(&mut self, block: &str) -> Option<Value> {
        // The code's result depends on the live env, which no memo key carries.
        super::regex_arg_purity::note_opaque_read();
        let code = block.strip_prefix('{')?.strip_suffix('}')?;
        let (stmts, id) = self.parse_regex_code_cached_with_id(code)?;
        let env = self.env.clone();
        let value = match self.run_regex_sub_eval(env, None, |interp| {
            interp.eval_block_value_cached(&stmts, id)
        }) {
            Ok(v) => v,
            Err(e) => e.return_value?,
        }
        .into_deref();
        match value.view() {
            ValueView::Regex(_) | ValueView::RegexWithAdverbs(_) => Some(value),
            ValueView::Routine {
                is_regex: true,
                captured_regex: Some(regex),
                ..
            } => Some((**regex).clone()),
            ValueView::Str(s) => Some(Value::regex(s.to_string())),
            _ => None,
        }
    }

    /// Whether this reference actually resolves to a caller-scope Regex.
    /// Unqualified names are eligible for the lexical fallback because
    /// `my regex name` uses that spelling, but a normal package token/rule
    /// must still remain eligible for regex prefilter analysis.
    pub(super) fn lexical_regex_is_in_scope(&self, spec: &NamedRegexLookupSpec) -> bool {
        Self::may_name_lexical_regex(spec)
            && self
                .lookup_lexical_regex(&spec.lookup_name, !spec.token_lookup)
                .is_some()
    }

    /// Candidates for a `<&lexical(args)>` reference, or `None` when it does
    /// not resolve to one (see [`Interpreter::resolve_lexical_regex`]).
    pub(super) fn lexical_regex_subrule_candidates(
        &mut self,
        spec: &NamedRegexLookupSpec,
        pkg: Symbol,
        arg_values: &[Value],
    ) -> Option<std::sync::Arc<super::regex_token_candidates::TokenCandidates>> {
        let value = self.resolve_lexical_regex(spec, pkg)?;
        let pattern = self.instantiate_regex_value_with_args(&value, arg_values);
        if pattern.is_none()
            && super::super::regex_parse::PENDING_REGEX_ERROR.with(|error| error.borrow().is_some())
        {
            return Some(std::sync::Arc::new(
                super::regex_token_candidates::TokenCandidates::new(Vec::new()),
            ));
        }
        let pattern = pattern?;
        let parsed = self.parse_candidate_in_pkg(&pattern, pkg)?;
        Some(std::sync::Arc::new(
            super::regex_token_candidates::TokenCandidates::new(vec![(parsed, pkg, None)]),
        ))
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
        let qq_prefix = crate::meta_ns::MetaNs::RegexQq.prefix();
        for (key, value) in scope.iter() {
            // A `"..."` atom's qq thunk is bound to its result, which the
            // pre-pass splices in while the pattern is parsed (see
            // `regex_qq_token_scope`); one that throws is left out.
            let value = if key.starts_with(qq_prefix) {
                match self.call_sub_value(value.clone(), Vec::new(), false) {
                    Ok(result) => Value::str(result.to_string_value()),
                    Err(_) => continue,
                }
            } else {
                value.clone()
            };
            saved.push((key.clone(), self.env.get(key).cloned()));
            self.env.insert(key.clone(), value);
        }
        (!saved.is_empty()).then_some(saved)
    }

    /// Bind `arg_values` to the signature a Regex value carries and render the
    /// resulting pattern. Mirrors the named-rule path
    /// (`resolve_one_token_pattern_with_args`): the parameters are bound over
    /// an isolated copy of the env (`run_regex_sub_eval`), then baked into the
    /// pattern's code blocks and interpolated into its text, because both run against the *caller's* env
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
        let mut env = self.env.clone();
        // A regex is a closure over the scope its literal was written in; the
        // body of an anonymous declarator returned from a sub (HomoGlypher's
        // `tokenize`) routinely reads such a lexical.
        if let Some(scope) = value.regex_closure_scope() {
            for (key, captured) in scope.iter() {
                env.insert(key.clone(), captured.clone());
            }
        }
        self.run_regex_sub_eval(env, None, |interp| {
            let names: Vec<String> = param_defs.iter().map(|pd| pd.name.clone()).collect();
            if let Err(err) = interp.bind_function_args_values(param_defs, &names, arg_values) {
                super::regex_arg_purity::note_opaque_read();
                super::super::regex_parse::PENDING_REGEX_ERROR.with(|slot| {
                    *slot.borrow_mut() = Some(err);
                });
                return None;
            }
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
        })
    }
}
