//! Dynamically-scoped (`$*`/`@*`/`%*`) parameters of a `token`/`rule`/`regex`.
//!
//! Raku lets a rule parameterize the subrules it calls through the dynamic
//! scope rather than through arguments:
//!
//! ```raku
//! token value($*STOPPER = '"') { \" [ \" | <char>+ \" ] }
//! token char { <?{ $*STOPPER eq '"' }> <!["]> . }
//! ```
//!
//! `value` binds `$*STOPPER` for the duration of *its* match, and everything
//! reached from its body — its own code blocks and interpolations, a subrule it
//! calls, a subrule that subrule calls — resolves `$*STOPPER` to that binding
//! until `value` returns. The same mechanism binds a start rule's `$*` params
//! from `.parse(..., :args(...))`.
//!
//! mutsu resolves a `$*` variable through the interpreter env (the key is the
//! parameter's own name: `$*S` → `"*S"`, `@*A` → `"@*A"`), so establishing the
//! binding is a matter of writing it into `self.env` around the subrule's
//! resolution *and* match — resolution too, because the rule's own pattern may
//! interpolate the variable (`rule added-words { $*word $*extra }`) — and
//! restoring the previous binding afterwards so nesting tears down correctly.
//!
//! Before this, a `$*` parameter was only ever bound inside the throwaway
//! scratch interpreter that turns a rule body into a pattern string, and then
//! textually baked into *that rule's own* code blocks; nothing reached the
//! dynamic scope, so a subrule saw `Nil` and a `.parse(:args(...))` whose start
//! rule declared `$*` parameters failed outright.

use std::cell::RefCell;
use std::sync::atomic::{AtomicBool, Ordering};

use super::super::*;
use crate::ast::ParamDef;
use crate::env::is_dynamic_var_name;

/// Set the first time a `token`/`rule`/`regex` carrying a dynamically-scoped
/// parameter is registered. Almost no program has one, and every subrule
/// reference would otherwise pay a memo probe, so this gates the whole
/// mechanism behind a relaxed atomic load.
pub(crate) static ANY_DYNAMIC_TOKEN_PARAM: AtomicBool = AtomicBool::new(false);

/// One dynamically-scoped parameter of a rule: its index among the rule's
/// positional parameters, and the parameter itself (whose `name` is the env key
/// and whose `default` supplies the value when no argument is passed).
type DynParam = (usize, ParamDef);

/// The env keys a rule's dynamic parameters bound, with whatever they shadowed.
pub(crate) type SavedDynParams = Vec<(String, Option<Value>)>;

/// Cache slot: the `TOKEN_DEFS_GEN` the entry was built under + the parameters.
type CachedDynParams = (u64, Arc<Vec<DynParam>>);

thread_local! {
    /// (pkg, rule name) → its dynamic parameters, under the `TOKEN_DEFS_GEN`
    /// the entry was built for (same invalidation discipline as the parsed
    /// candidate caches next door). Resolving the defs walks the registry, so
    /// this keeps a parameterized grammar from re-deriving the signature at
    /// every match position.
    static TOKEN_DYNAMIC_PARAMS: RefCell<
        rustc_hash::FxHashMap<(String, String), CachedDynParams>,
    > = RefCell::new(rustc_hash::FxHashMap::default());

    /// The rule-parameter bindings currently in force, innermost last. A plain
    /// `{ … }` block that mentions a `$*` variable is deferred to the reduce
    /// walk (that is what makes a `:my $*x` per-match binding work), so by the
    /// time it runs the rule that bound the parameter has long returned — the
    /// values have to travel with the block, like its `:my` lexicals do.
    static ACTIVE_DYN_PARAMS: RefCell<Vec<(String, Value)>> = const { RefCell::new(Vec::new()) };
}

/// The rule-parameter dynamic bindings a code block collected right now should
/// be replayed under, innermost binding winning. Empty for every grammar that
/// declares no dynamically-scoped rule parameter.
pub(crate) fn active_dynamic_params() -> Vec<(String, Value)> {
    if !ANY_DYNAMIC_TOKEN_PARAM.load(Ordering::Relaxed) {
        return Vec::new();
    }
    ACTIVE_DYN_PARAMS.with(|stack| {
        let stack = stack.borrow();
        let mut out: Vec<(String, Value)> = Vec::new();
        for (key, value) in stack.iter() {
            match out.iter_mut().find(|(k, _)| k == key) {
                Some(slot) => slot.1 = value.clone(),
                None => out.push((key.clone(), value.clone())),
            }
        }
        out
    })
}

/// Note a freshly registered rule's signature, arming [`ANY_DYNAMIC_TOKEN_PARAM`]
/// when it declares a dynamically-scoped parameter.
pub(crate) fn note_token_def_params(param_defs: &[ParamDef]) {
    if param_defs
        .iter()
        .any(|pd| !pd.named && !pd.slurpy && is_dynamic_var_name(&pd.name))
    {
        ANY_DYNAMIC_TOKEN_PARAM.store(true, Ordering::Relaxed);
    }
}

impl Interpreter {
    fn subrule_dynamic_params(&mut self, name: &str, pkg: &str) -> Arc<Vec<DynParam>> {
        let tok_gen =
            crate::runtime::regex_parse::TOKEN_DEFS_GEN.load(std::sync::atomic::Ordering::Relaxed);
        let key = (pkg.to_string(), name.to_string());
        if let Some(hit) = TOKEN_DYNAMIC_PARAMS.with(|c| {
            c.borrow()
                .get(&key)
                .filter(|(cached_gen, _)| *cached_gen == tok_gen)
                .map(|(_, v)| Arc::clone(v))
        }) {
            return hit;
        }
        // A proto/multi rule may spread its candidates over several defs; the
        // dynamic parameters are a property of the signature, so take them from
        // the first candidate that declares any.
        let params = self
            .resolve_token_defs_in_pkg(name, pkg)
            .into_iter()
            .find_map(|def| {
                let collected: Vec<DynParam> = def
                    .param_defs
                    .iter()
                    .filter(|pd| !pd.named && !pd.is_invocant)
                    .enumerate()
                    .filter(|(_, pd)| is_dynamic_var_name(&pd.name))
                    .map(|(idx, pd)| (idx, pd.clone()))
                    .collect();
                (!collected.is_empty()).then_some(collected)
            })
            .unwrap_or_default();
        let arc = Arc::new(params);
        TOKEN_DYNAMIC_PARAMS.with(|c| {
            c.borrow_mut().insert(key, (tok_gen, Arc::clone(&arc)));
        });
        arc
    }

    /// Establish the dynamically-scoped parameters of rule `name` for the
    /// duration of one invocation, taking each value from the matching
    /// positional argument or, failing that, from the parameter's default.
    /// Returns `None` (and touches nothing) when the rule declares none, which
    /// is the overwhelmingly common case.
    pub(crate) fn install_subrule_dynamic_params(
        &mut self,
        name: &str,
        pkg: &str,
        arg_values: &[Value],
    ) -> Option<SavedDynParams> {
        if !ANY_DYNAMIC_TOKEN_PARAM.load(Ordering::Relaxed) {
            return None;
        }
        let params = self.subrule_dynamic_params(name, pkg);
        if params.is_empty() {
            return None;
        }
        // Named arguments (`:args(:x(1),)`) never fill a positional slot.
        let positional: Vec<&Value> = arg_values
            .iter()
            .filter(|v| !matches!(v.view(), ValueView::Pair(..) | ValueView::ValuePair(..)))
            .collect();
        let mut saved: SavedDynParams = Vec::with_capacity(params.len());
        for (idx, pd) in params.iter() {
            let value = match positional.get(*idx) {
                Some(v) => (*v).clone(),
                None => match pd.default.as_ref() {
                    Some(default) => match self.eval_param_default(pd, default) {
                        Ok(v) => v,
                        Err(_) => continue,
                    },
                    None => continue,
                },
            };
            saved.push((pd.name.clone(), self.env.get(&pd.name).cloned()));
            ACTIVE_DYN_PARAMS.with(|stack| {
                stack.borrow_mut().push((pd.name.clone(), value.clone()));
            });
            self.env.insert(pd.name.clone(), value);
        }
        (!saved.is_empty()).then_some(saved)
    }

    /// Tear the bindings [`Self::install_subrule_dynamic_params`] made back down,
    /// restoring whatever they shadowed (an enclosing rule's binding of the same
    /// name, most often).
    pub(crate) fn restore_subrule_dynamic_params(&mut self, saved: SavedDynParams) {
        ACTIVE_DYN_PARAMS.with(|stack| {
            let mut stack = stack.borrow_mut();
            let keep = stack.len().saturating_sub(saved.len());
            stack.truncate(keep);
        });
        for (key, prior) in saved.into_iter().rev() {
            match prior {
                Some(value) => {
                    self.env.insert(key, value);
                }
                None => {
                    self.env.remove(&key);
                }
            }
        }
    }
}
