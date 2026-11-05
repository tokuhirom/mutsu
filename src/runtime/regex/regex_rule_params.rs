//! A rule's lexical parameters as match-time variables (ADR-12917,
//! direction 2).
//!
//! ```raku
//! token map(Str $indent) { <map-entry($indent)>+ % [ <.newline> $indent ] }
//! ```
//!
//! Rakudo compiles a rule's body once: `$indent` is an interpolation node that
//! reads the parameter when the cursor reaches it, and `<map-entry($indent)>`
//! evaluates its argument there too. mutsu used to bind the arguments in a
//! scratch env and splice the values into the body's *source* — the pattern
//! text, its code blocks and its call arguments — so every distinct argument
//! produced a different pattern, parsed and compiled on its own. The rule's
//! call edges then depended on the values, so the left-recursion proof
//! (`regex_left_call_graph`) could not go through and every call of such a rule
//! took the eager growing-seed path.
//!
//! For a rule that qualifies ([`def_runtime_param_names`]) the resolver now
//! hands out the body unchanged, prefixed with a declaration block
//! ([`mark_rule_params`]): the parameter names between two
//! [`RULE_PARAMS_MARK`]s. The interpolation pre-pass and the structural parser
//! treat the names exactly like an in-regex `:my` lexical — a bare `$name` is
//! left in the text and lowered to a match-time [`RegexAtom::VarInterp`] — so
//! the parse is the same for every argument and the text-keyed parse caches
//! stay sound (the declaration is part of the key). The values are bound in
//! the env for the call's window ([`Interpreter::install_runtime_rule_params`]),
//! where `VarInterp`, code blocks and call arguments read them, through the
//! same install/restore discipline as a `$*` parameter.
//!
//! [`RegexAtom::VarInterp`]: crate::runtime::regex_types::RegexAtom::VarInterp

use std::cell::RefCell;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use super::super::*;
use super::regex_dynparams::SavedDynParams;
use crate::ast::{FunctionDef, ParamDef};

/// Opens and closes the declaration block of a rule's runtime parameters at
/// the start of its pattern text. A reserved control character, like
/// `NON_DECLARATIVE_INTERP_MARK`: it never appears in written pattern source
/// and never survives the structural parser.
pub(crate) const RULE_PARAMS_MARK: char = '\u{2}';

/// Set the first time a rule declaring a positional parameter is registered;
/// gates [`Interpreter::runtime_rule_params`] so a program without one pays a
/// relaxed load per call and nothing else.
pub(crate) static ANY_POSITIONAL_TOKEN_PARAM: AtomicBool = AtomicBool::new(false);

/// Set the first time a rule declaring a positional `is rw` parameter is
/// registered; gates [`Interpreter::mark_rw_regex_args`].
pub(crate) static ANY_RW_TOKEN_PARAM: AtomicBool = AtomicBool::new(false);

/// Starts the key of a [`SavedDynParams`] entry that is not a shadowed
/// binding but the write-back of an `is rw` runtime parameter to the caller's
/// variable ([`rw_alias_key`]).
const RW_ALIAS_MARK: char = '\u{3}';

/// The [`SavedDynParams`] key that sends the final value of the runtime
/// parameter `param` back to the caller's variable `caller` when the window is
/// torn down (`restore_subrule_dynamic_params`).
// Cost: O(n), n = the names' total length.
fn rw_alias_key(param: &str, caller: &str) -> String {
    format!("{RW_ALIAS_MARK}{param}{RW_ALIAS_MARK}{caller}")
}

/// `(param, caller)` of an [`rw_alias_key`], `None` for an ordinary key.
// Cost: O(n), n = the key's length.
pub(crate) fn split_rw_alias_key(key: &str) -> Option<(&str, &str)> {
    key.strip_prefix(RW_ALIAS_MARK)?.split_once(RW_ALIAS_MARK)
}

/// `(pkg, rule name) -> its runtime parameter names` under the token
/// generation the entry was built for; `None` when the rule does not qualify.
type RuntimeParamsCache =
    rustc_hash::FxHashMap<(Symbol, Symbol), (u64, Option<Arc<[String]>>)>;

thread_local! {
    static RUNTIME_RULE_PARAMS: RefCell<RuntimeParamsCache> =
        RefCell::new(rustc_hash::FxHashMap::default());
}

/// `body` with the declaration block of `names` in front of it.
// Cost: O(n + b), n = the names' total length, b = the body's.
pub(crate) fn mark_rule_params(names: &[String], body: &str) -> String {
    let mut out = String::with_capacity(body.len() + 2 + names.len() * 8);
    out.push(RULE_PARAMS_MARK);
    out.push_str(&names.join(" "));
    out.push(RULE_PARAMS_MARK);
    out.push_str(body);
    out
}

/// `text` with `names` added to its leading declaration block, which is
/// created when the text has none. A regex's leading `:my $x …;` prefix is
/// evaluated into the env before the rest of the pattern is parsed
/// (`regex_match_with_captures`), so the rest carries its names the way a
/// rule body carries its parameters: a later bare `$x` then reads the
/// variable when the cursor reaches it, after any code block that assigned
/// it, instead of being spliced with its value at the start of the match.
// Cost: O(n + b + k·m), n = the names' total length, b = the text's length,
// k = the new names, m = the names already declared.
pub(crate) fn add_runtime_names(names: &[String], text: &str) -> String {
    if names.is_empty() {
        return text.to_string();
    }
    let Some((existing, rest)) = split_rule_params(text) else {
        return mark_rule_params(names, text);
    };
    let mut all: Vec<String> = existing.map(str::to_string).collect();
    for name in names {
        if !all.contains(name) {
            all.push(name.clone());
        }
    }
    mark_rule_params(&all, rest)
}

/// Split a pattern text into the runtime parameter names its leading
/// declaration block declares and the text after the block; `None` when it
/// has no block.
// Cost: O(n), n = the block's length.
pub(crate) fn split_rule_params(text: &str) -> Option<(impl Iterator<Item = &str>, &str)> {
    let rest = text.strip_prefix(RULE_PARAMS_MARK)?;
    let end = rest.find(RULE_PARAMS_MARK)?;
    Some((
        rest[..end].split(' ').filter(|n| !n.is_empty()),
        &rest[end + RULE_PARAMS_MARK.len_utf8()..],
    ))
}

/// Note a freshly registered rule's signature (see
/// [`ANY_POSITIONAL_TOKEN_PARAM`]).
pub(crate) fn note_token_def_positional_params(param_defs: &[ParamDef]) {
    if param_defs.iter().any(|pd| !pd.named && !pd.is_invocant) {
        ANY_POSITIONAL_TOKEN_PARAM.store(true, Ordering::Relaxed);
    }
    if param_defs
        .iter()
        .any(|pd| !pd.named && !pd.is_invocant && is_rw_param(pd))
    {
        ANY_RW_TOKEN_PARAM.store(true, Ordering::Relaxed);
    }
}

/// Whether `pd` is a positional parameter whose only trait is `rw`.
// Cost: O(1).
fn is_rw_param(pd: &ParamDef) -> bool {
    !pd.is_does_target() && matches!(pd.traits.as_slice(), [t] if t == "rw")
}

/// Whether `pd` can be bound by position straight into the call's window:
/// a required, plain `$` scalar with at most a nominal type, and nothing that
/// runs code or reshapes the value while binding (a default, a `where`, a
/// coercion, a sub-signature, a trait). The resolver still binds the full
/// signature once per argument list, so a type mismatch or a wrong arity
/// raises as before; the window only re-binds what that binding accepted.
// Cost: O(n), n = the parameter's name length.
fn param_binds_positionally(pd: &ParamDef) -> bool {
    !pd.named
        && !pd.slurpy
        && !pd.double_slurpy
        && !pd.sigilless
        && !pd.optional_marker
        && !pd.is_invocant
        && pd.default.is_none()
        && pd.where_constraint.is_none()
        && pd.sub_signature.is_none()
        && pd.outer_sub_signature.is_none()
        && pd.code_signature.is_none()
        && pd.literal_value.is_none()
        && pd.type_capture.is_none()
        && pd.shape_constraints.is_none()
        && (pd.no_traits() || is_rw_param(pd))
        && pd
            .type_constraint
            .as_deref()
            .is_none_or(|t| !t.contains(['(', '[', ':']))
        && pd
            .name
            .chars()
            .next()
            .is_some_and(|c| c.is_alphabetic() || c == '_')
        && pd
            .name
            .chars()
            .all(|c| c.is_alphanumeric() || c == '_' || c == '-')
}

/// Whether binding `value` to a plain `$` parameter leaves it unchanged, so
/// the window can bind the argument itself: a number, a string, a boolean, a
/// type object, `Nil`, or a `Regex` (which the match-time interpolation
/// enters as a pattern). A container value is not — binding itemizes it
/// (`$p.raku` is `${…}`) — and neither is an object the old path treats as
/// opaque; such an argument list keeps the spliced-source path.
// Cost: O(1).
pub(crate) fn arg_binds_by_identity(value: &Value) -> bool {
    let value = value.unwrap_varref().clone().into_deref();
    matches!(
        value.view(),
        ValueView::Int(_)
            | ValueView::BigInt(_)
            | ValueView::Num(_)
            | ValueView::Str(_)
            | ValueView::Bool(_)
            | ValueView::Rat(..)
            | ValueView::FatRat(..)
            | ValueView::BigRat(..)
            | ValueView::Package(_)
            | ValueView::Nil
            | ValueView::Regex(_)
            | ValueView::RegexWithAdverbs(_)
    )
}

/// The parameter names of `def` when its body is a plain regex literal and
/// every parameter binds positionally ([`param_binds_positionally`]); `None`
/// otherwise, and the rule keeps the spliced-source path.
// Cost: O(p), p = the parameters.
fn def_runtime_param_names(def: &FunctionDef) -> Option<Vec<String>> {
    let constant_body = def.body.iter().all(|stmt| {
        matches!(
            stmt,
            crate::ast::Stmt::SetLine(..) | crate::ast::Stmt::Expr(crate::ast::Expr::Literal(_))
        )
    });
    if !constant_body
        || def.param_defs.is_empty()
        || !def.param_defs.iter().all(param_binds_positionally)
    {
        return None;
    }
    Some(def.param_defs.iter().map(|pd| pd.name.clone()).collect())
}

impl Interpreter {
    /// The runtime parameter names of the rule `name` called from `pkg`: the
    /// name resolves to exactly one rule, and that rule qualifies
    /// ([`def_runtime_param_names`]). Several candidates (a proto, a multi)
    /// keep the spliced-source path: the window would have to bind each
    /// candidate's own names.
    // TODO: bind per candidate at its entry, so candidates with differing
    // names, or a default/`where`/coerced parameter, get a value-independent
    // body too.
    // Cost: O(1) expected once memoized for the token generation; the first
    // call per (pkg, name) resolves the rule's definitions, O(s * m), s =
    // enclosing scopes, m = MRO length.
    pub(crate) fn runtime_rule_params(&self, name: &str, pkg: Symbol) -> Option<Arc<[String]>> {
        if !ANY_POSITIONAL_TOKEN_PARAM.load(Ordering::Relaxed) {
            return None;
        }
        let generation =
            crate::runtime::regex_parse::TOKEN_DEFS_GEN.load(std::sync::atomic::Ordering::Relaxed);
        let key = (pkg, Symbol::intern(name));
        if let Some(hit) = RUNTIME_RULE_PARAMS.with(|c| {
            c.borrow()
                .get(&key)
                .filter(|(cached, _)| *cached == generation)
                .map(|(_, names)| names.clone())
        }) {
            return hit;
        }
        let defs = self.resolve_token_defs_in_pkg(name, pkg);
        // Every candidate of a multi must declare the same names, so one
        // window binds whichever candidate the dispatcher picks.
        let names = defs
            .iter()
            .map(|def| def_runtime_param_names(def))
            .reduce(|acc, next| if acc == next { acc } else { None })
            .flatten()
            .map(Arc::from);
        RUNTIME_RULE_PARAMS.with(|c| {
            c.borrow_mut().insert(key, (generation, names.clone()));
        });
        names
    }

    /// Bind the runtime parameters of the rule `name` to the call's
    /// positional `args` in the env, for the call's window, appending what
    /// they shadow to `prior` so one restore undoes both. A call whose
    /// argument count does not fit binds nothing: the resolver raises the
    /// arity error and no candidate runs.
    // Cost: O(1) when the rule has no runtime parameters (after the memoized
    // eligibility probe); else O(p), p = its parameters.
    pub(crate) fn install_runtime_rule_params(
        &mut self,
        name: &str,
        pkg: Symbol,
        args: &[Value],
        prior: Option<SavedDynParams>,
    ) -> Option<SavedDynParams> {
        if args.is_empty() {
            return prior;
        }
        let Some(names) = self.runtime_rule_params(name, pkg) else {
            return prior;
        };
        if names.len() != args.len() || !args.iter().all(arg_binds_by_identity) {
            return prior;
        }
        let mut saved = prior.unwrap_or_default();
        saved.reserve(names.len());
        for (param, value) in names.iter().zip(args) {
            saved.push((param.clone(), self.env.get(param).cloned()));
            self.env.insert(
                param.clone(),
                value.unwrap_varref().clone().into_deref(),
            );
            // An `is rw` parameter aliases the caller's variable: the rule's
            // code blocks write the parameter, and the window's end hands the
            // final value back (`restore_subrule_dynamic_params`).
            if let Some((caller, _)) = value.as_varref() {
                saved.push((rw_alias_key(param, &caller.resolve()), None));
            }
        }
        Some(saved)
    }

    /// `values` (the evaluated argument list of the call `spec` from `pkg`)
    /// with each argument bound to an `is rw` parameter and written as a bare
    /// variable wrapped in the variable's name, the way an ordinary call
    /// passes an lvalue (`WrapVarRef`). The binder then accepts it for the
    /// `is rw` parameter, and the call's window knows which variable to write
    /// the parameter's final value to. Any other argument stays a value, so
    /// binding it to an `is rw` parameter raises `X::Parameter::RW`, as for a
    /// routine.
    // Cost: O(1) when no rule declares an `is rw` positional parameter; else
    // O(s * m + a), s = enclosing scopes, m = MRO length, a = the arguments.
    pub(crate) fn mark_rw_regex_args(
        &self,
        spec: &super::regex_helpers::NamedRegexLookupSpec,
        pkg: Symbol,
        values: Vec<Value>,
    ) -> Vec<Value> {
        if !ANY_RW_TOKEN_PARAM.load(Ordering::Relaxed)
            || values.is_empty()
            || spec.arg_exprs.len() != values.len()
        {
            return values;
        }
        let defs = self.resolve_token_defs_in_pkg(&spec.lookup_name, pkg);
        values
            .into_iter()
            .enumerate()
            .map(|(i, value)| {
                let rw_here = defs.iter().any(|def| {
                    def.param_defs
                        .iter()
                        .filter(|pd| !pd.named && !pd.is_invocant)
                        .nth(i)
                        .is_some_and(is_rw_param)
                });
                match bare_scalar_name(&spec.arg_exprs[i]) {
                    Some(name) if rw_here => {
                        Value::varref(Symbol::intern(name), value, None)
                    }
                    _ => value,
                }
            })
            .collect()
    }

    /// The keys of a call window's bindings `saved` whose final values are
    /// filed on the callee's Match for its action: every binding but the
    /// rule's runtime parameters, which are lexicals of its own body, not
    /// dynamic variables an action reads.
    // Cost: O(b * p), b = the bindings, p = the rule's parameters.
    pub(crate) fn window_attach_keys(
        &self,
        saved: &SavedDynParams,
        name: &str,
        pkg: Symbol,
    ) -> Vec<String> {
        let runtime = if saved.is_empty() {
            None
        } else {
            self.runtime_rule_params(name, pkg)
        };
        saved
            .iter()
            .map(|(key, _)| key)
            .filter(|key| split_rw_alias_key(key).is_none())
            .filter(|key| runtime.as_ref().is_none_or(|names| !names.contains(key)))
            .cloned()
            .collect()
    }

    /// The one body of the rule `name` (called from `pkg`) whose parameters
    /// are bound at run time, parsed as every call of it runs it, for the
    /// call-graph analyses: `None` when the rule has no runtime parameters;
    /// `Some(None)` when it has but its body is not knowable ahead of a call
    /// — it does not parse, or it splices a list ([`body_splices_list`]),
    /// whose elements can add calls.
    // Cost: O(1) expected for the eligibility probe; then the body's parse
    // (cached by its text) and O(t) to scan it, t = its tokens.
    pub(super) fn runtime_param_rule_body(
        &mut self,
        name: &str,
        pkg: Symbol,
    ) -> Option<Option<(Arc<RegexPattern>, Symbol)>> {
        Some(self.runtime_param_rule_text(name, pkg)?.and_then(|(marked, sub_pkg)| {
            self.parse_candidate_in_pkg(&marked, sub_pkg)
                .map(|p| (p, sub_pkg))
        }))
    }

    /// The text [`Self::runtime_param_rule_body`] parses: the rule's one body
    /// with its parameter declaration block in front, and the package it is
    /// parsed in. Same `None` / `Some(None)` answers, except that a body that
    /// would not parse is still `Some(Some(..))` here.
    // Cost: O(1) expected for the eligibility probe; then O(b) to scan and
    // mark the body, b = its length.
    pub(super) fn runtime_param_rule_text(
        &self,
        name: &str,
        pkg: Symbol,
    ) -> Option<Option<(String, Symbol)>> {
        let names = self.runtime_rule_params(name, pkg)?;
        let raw = self.resolve_token_patterns_static_in_pkg(name, pkg);
        let [(text, sub_pkg, _)] = &raw[..] else {
            return Some(None);
        };
        if body_splices_list(text) {
            return Some(None);
        }
        Some(Some((mark_rule_params(&names, text), *sub_pkg)))
    }
}

/// `name` when `expr` is exactly a plain scalar variable `$name`.
// Cost: O(n), n = the expression's length.
fn bare_scalar_name(expr: &str) -> Option<&str> {
    let name = expr.trim().strip_prefix('$')?;
    let mut chars = name.chars().peekable();
    let first = chars.next()?;
    if !(first.is_alphabetic() || first == '_') {
        return None;
    }
    while let Some(c) = chars.next() {
        let kebab = c == '-' && chars.peek().is_some_and(|n| n.is_alphabetic() || *n == '_');
        if !(c.is_alphanumeric() || c == '_' || kebab) {
            return None;
        }
    }
    Some(name)
}

/// Whether the body text `text` interpolates a list (`@list`, `@$x`,
/// `<@list>`) outside its code blocks: the list's `Regex` elements are spliced
/// in as pattern source, which can add calls. A scalar's value cannot: a plain
/// value is a literal, and a `Regex` value is reached through `<$x>` or a
/// match-time `$x`, whose target the call-graph walk already treats as
/// unknown. Conservative: an `@` written inside a quoted literal counts too.
// Cost: O(n), n = the text's length.
fn body_splices_list(text: &str) -> bool {
    let chars: Vec<char> = text.chars().collect();
    let mut i = 0usize;
    while i < chars.len() {
        match chars[i] {
            '\\' => i += 2,
            '{' => {
                let mut depth = 1usize;
                i += 1;
                while i < chars.len() && depth > 0 {
                    match chars[i] {
                        '\\' => i += 1,
                        '{' => depth += 1,
                        '}' => depth -= 1,
                        _ => {}
                    }
                    i += 1;
                }
            }
            '@' if chars
                .get(i + 1)
                .is_some_and(|c| c.is_alphabetic() || matches!(c, '_' | '$' | '*')) =>
            {
                return true;
            }
            _ => i += 1,
        }
    }
    false
}
