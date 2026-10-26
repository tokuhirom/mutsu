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
        && pd.traits.is_empty()
        && !pd.does_target
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
    let value = value.clone().into_deref();
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
    // TODO: bind per candidate at its entry, so proto and multi rules with
    // parameters get a value-independent body too.
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
        let names = match &defs[..] {
            [def] => def_runtime_param_names(def).map(Arc::from),
            _ => None,
        };
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
            self.env.insert(param.clone(), value.clone().into_deref());
        }
        Some(saved)
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
