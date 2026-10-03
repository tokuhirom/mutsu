//! Resolving a `<subrule>` call for the compiled engine (ADR-0135 D3).
//!
//! A call is a frame in the run's own loop when the callee is a plain rule, or
//! a proto whose candidates all have programs, and a *bridge* to the walk's
//! producer otherwise (D5). The frame shapes are the ones the walk resolves
//! with one end per candidate, with the one difference the compiled engine
//! allows: a rule that calls itself is fine as long as it cannot re-enter *at
//! the same position* (`subrule_cannot_left_reenter`), because a frame, unlike
//! the walk's stream, needs no left-recursion activation to stay sound.

use std::sync::Arc;

use super::super::regex_lr_state::lr_name_active;
use super::super::regex_token_candidates::TokenCandidates;
use super::RxProgram;
use super::rx_entry::program_for;
use super::rx_scope::CallWindow;
use crate::runtime::Interpreter;
use crate::runtime::regex::regex_dynparams::{
    ANY_DYNAMIC_TOKEN_PARAM, SavedDynParams, regex_args_have_opaque,
};
use crate::runtime::regex::regex_helpers::{grammar_dynvar_scope_pop, grammar_dynvar_scope_push};
use crate::runtime::regex_types::{NamedAtom, RegexAtom, RegexCaptures};
use crate::symbol::Symbol;
use crate::value::Value;

/// (rule, caller package, caller `:i`) → (token generation, the call's target).
type TargetCache = rustc_hash::FxHashMap<(Symbol, Symbol, bool), (u64, CallVerdict)>;

/// A call's frame, or why it takes the bridge (`MUTSU_VM_STATS`'s
/// `regex-walk:` line, `bridged=`).
pub(super) type CallVerdict = Result<CallTarget, &'static str>;

thread_local! {
    /// The verdict for a call, per (rule, caller package, caller `:i`),
    /// stamped with the token generation it was reached under — the inline
    /// cache of ADR-0135 D3. Kept only for a rule whose candidates the
    /// argument-less memo holds (a fully static one); anything else is
    /// resolved afresh at every call.
    static TARGETS: std::cell::RefCell<TargetCache> =
        std::cell::RefCell::new(rustc_hash::FxHashMap::default());
}

/// A `<subrule>` call, resolved ([`Interpreter::rx_call_resolve`]).
pub(super) struct ResolvedCall {
    /// The frame it runs as, or why it bridges.
    pub(super) verdict: CallVerdict,
    /// The evaluated arguments of a call with arguments.
    pub(super) args: Option<Vec<Value>>,
    /// The binding window installed for a frame's callee. The caller owns its
    /// uninstall from here on.
    pub(super) window: Option<CallWindow>,
}

/// What a `<subrule>` call runs as a frame.
#[derive(Clone)]
pub(super) enum CallTarget {
    /// One plain rule: its program and the package its body matches in (a rule
    /// is matched in the package that defines it).
    Plain(Arc<RxProgram>, Symbol),
    /// A proto: the `:sym<…>` candidates, ranked at the call by the walk's own
    /// LTM measurement (ADR-0046), of which the first that matches wins.
    Proto(Arc<TokenCandidates>),
    /// No rule of that name: a builtin (`<.ws>`, `<wb>`, `<alpha>`, …) the walk's
    /// single-candidate arm decides, with at most one end.
    Single,
    /// No rule of that name, but a plain grammar METHOD of it: the method runs
    /// once on the calling frame's cursor and answers at most one end
    /// (`regex_grammar_method_end`).
    Method,
    /// A call evaluated eagerly by the growing-seed loop (`subrule_seed_ends`),
    /// every end up front, and entered highest priority first; the reason is
    /// its `MUTSU_VM_STATS` leaf. Taken by a rule that may re-enter itself at
    /// the same position, or one called while an evaluation of the same name
    /// is live, whose re-entries read the seed (`lr-seed`); and by a callee
    /// with no program of its own (`declined-callee`), most often a `:m` rule,
    /// whose ends the all-ends entry finds by running its mark-stripped
    /// program (anything it does walk, that entry counts as walked).
    Eager(Arc<TokenCandidates>, &'static str),
}

impl Interpreter {
    /// Resolve the `<subrule>` call `name` made from `pkg` at a position where
    /// the caller's captures are `caps`. A call with arguments evaluates them
    /// here, once, and a bridged call hands them to the walk's producer, so
    /// user code in an argument never runs twice. `None` when an argument
    /// fails to evaluate: the call does not match, as in the walk.
    ///
    /// A frame that needs a binding window (the callee's `$*` parameters, or
    /// an object or closure argument baking cannot carry into its code blocks)
    /// comes back with the window installed (`ResolvedCall::window`): the
    /// caller records it in the run's scopes (`rx_scope`), so backtracking
    /// across the frame removes and re-installs it, and the callee's return
    /// uninstalls it. The window is installed before the callee is resolved,
    /// because its pattern may interpolate a `$*` parameter
    /// (`rule r($*w) { $*w }`), as in the walk.
    // Cost: O(1) expected for an argument-less call without a window (one
    // memoized candidate probe, plus the memoized call-graph verdicts); with
    // arguments, their evaluation and the candidate resolution (memoized per
    // argument list); O(c) more for a proto of c candidates (a program probe
    // each); O(b) for a window of b bindings.
    pub(super) fn rx_call_resolve(
        &mut self,
        name: &NamedAtom,
        pkg: Symbol,
        ic: bool,
        caps: &crate::runtime::regex_types::RegexCaptures,
    ) -> Option<ResolvedCall> {
        let spec = name.spec();
        if let Err(why) = self.rx_call_blockers(name, pkg) {
            // Bridged without evaluating: the producer evaluates them itself.
            return Some(ResolvedCall {
                verdict: Err(why),
                args: None,
                window: None,
            });
        }
        let args = if spec.arg_exprs.is_empty() {
            None
        } else {
            Some(self.eval_regex_arg_list(&spec.arg_exprs, caps)?)
        };
        let window = self.rx_call_window(name, pkg, args.as_deref().unwrap_or(&[]));
        let verdict = match &args {
            None => self.rx_call_target_checked(name, pkg, ic),
            Some(args) => {
                let (candidates, raw_empty) = self.parsed_subrule_candidates(spec, pkg, args);
                // No candidate for these arguments: a grammar method runs with
                // them. Anything else — a builtin, or a rule none of whose
                // candidates binds them (a type error the producer raises) —
                // is the walk's producer's to dispatch.
                if raw_empty {
                    if self.subrule_names_user_method(spec, pkg) {
                        Ok(CallTarget::Method)
                    } else {
                        Err("args-unbound")
                    }
                } else {
                    self.call_target_from_candidates(name, pkg, ic, candidates)
                }
            }
        };
        // An evaluation of this name is live (a growing-seed loop further up):
        // this call may be its re-entry, which the loop's bookkeeping answers.
        let verdict = match verdict {
            Ok(CallTarget::Plain(..) | CallTarget::Proto(_)) if lr_name_active(spec.lookup_sym) => {
                let (candidates, _) =
                    self.parsed_subrule_candidates(spec, pkg, args.as_deref().unwrap_or(&[]));
                Ok(CallTarget::Eager(candidates, "lr-seed"))
            }
            verdict => verdict,
        };
        // Only a call the engine evaluates keeps the window: the producer
        // installs its own.
        let window = match (&verdict, window) {
            (Ok(CallTarget::Plain(..) | CallTarget::Proto(_)), window) => {
                self.rx_call_rule_frame(name, pkg, window)
            }
            // The seed loop pushes the routine frame itself, around each
            // candidate's evaluation (`subrule_candidate_ends_with_frame`).
            (Ok(CallTarget::Eager(..)), window) => self
                .rx_call_rule_frame(name, pkg, window)
                .map(|w| CallWindow { routine: None, ..w }),
            (_, Some(saved)) => {
                self.restore_subrule_dynamic_params(saved);
                None
            }
            (_, None) => None,
        };
        Some(ResolvedCall {
            verdict,
            args,
            window,
        })
    }

    /// Install the binding window a call of `name` with `args` runs its callee
    /// in, when it needs one: what the walk's producer installs around the
    /// callee's whole match (`install_subrule_dynamic_params`).
    // Cost: O(1) when no rule declares a `$*` parameter, no argument is opaque
    // and the call cannot name a lexical; else O(a + b), a = the arguments,
    // b = the bindings installed.
    fn rx_call_window(
        &mut self,
        name: &NamedAtom,
        pkg: Symbol,
        args: &[Value],
    ) -> Option<SavedDynParams> {
        let spec = name.spec();
        // `<&r>` naming a lexical Regex: its defining scope joins the window,
        // since its code blocks read it where the cursor reaches them.
        if Self::may_name_lexical_regex(spec) {
            return self.install_subrule_dynamic_params(spec, pkg, args);
        }
        if !ANY_DYNAMIC_TOKEN_PARAM.load(std::sync::atomic::Ordering::Relaxed)
            && !regex_args_have_opaque(args)
        {
            return None;
        }
        self.install_subrule_dynamic_params_named(&spec.lookup_name, pkg, args, None)
    }

    /// The whole window of a call that runs as a frame: `params` (what
    /// [`Self::rx_call_window`] installed) and then the callee's own `:my $*x`
    /// declarations, initialized for this invocation as the walk does at rule
    /// entry (`enter_grammar_rule_dynvars`). The declarations are entered only
    /// once the call is known to be a frame, so a bridged call, whose producer
    /// enters its own, never runs an initializer twice.
    // Cost: O(1) when the program's grammar declares no `:my $*x`; else one
    // hash probe, plus the initializers' evaluation for a rule that declares
    // some.
    fn rx_call_rule_frame(
        &mut self,
        name: &NamedAtom,
        pkg: Symbol,
        params: Option<SavedDynParams>,
    ) -> Option<CallWindow> {
        let rule_frame = if self.regex_state.grammar_rule_dynvar_decls.is_empty()
            || !self
                .regex_state
                .grammar_rule_dynvar_decls
                .contains_key(&name.spec().lookup_name)
        {
            None
        } else {
            self.enter_grammar_rule_dynvars(&name.spec().lookup_name)
        };
        let routine = self
            .has_any_wrap_chains()
            .then(|| (pkg, name.spec().lookup_sym));
        if params.is_none() && rule_frame.is_none() && routine.is_none() {
            return None;
        }
        let mut saved = params.unwrap_or_default();
        let mut attach: Vec<String> = saved.iter().map(|(key, _)| key.clone()).collect();
        let scope_keys = rule_frame.map(|frame| {
            let (frame_saved, keys) = Self::into_window_parts(frame);
            saved.extend(frame_saved);
            attach.extend(keys.iter().cloned());
            keys
        });
        Some(CallWindow {
            saved,
            attach,
            scope_keys,
            routine,
        })
    }

    /// The frame `<name>` called from `pkg` runs as, or `Err(why)` when the
    /// call must take the bridge. `ic` is the caller's `:i`, which the walk scopes
    /// over the callee's body.
    // Cost: O(1) expected: one memoized candidate probe, plus the memoized
    // call-graph verdicts for the rule, per call; O(c) more for a proto of c
    // candidates (a program probe each).
    fn rx_call_target(&mut self, name: &NamedAtom, pkg: Symbol, ic: bool) -> CallVerdict {
        let spec = name.spec();
        // `<&r>` may name a lexical Regex, a value of this call's scope: never
        // cached.
        if Self::may_name_lexical_regex(spec) {
            return self.resolve_call_target(name, pkg, ic);
        }
        let generation =
            crate::runtime::regex_parse::TOKEN_DEFS_GEN.load(std::sync::atomic::Ordering::Relaxed);
        let key = (spec.lookup_sym, pkg, ic);
        if let Some(hit) = TARGETS.with(|c| {
            c.borrow()
                .get(&key)
                .filter(|(cached, _)| *cached == generation)
                .map(|(_, target)| target.clone())
        }) {
            return hit;
        }
        let target = self.resolve_call_target(name, pkg, ic);
        if Self::parsed_candidates_are_memoized(spec.lookup_sym, pkg) {
            TARGETS.with(|c| {
                c.borrow_mut().insert(key, (generation, target.clone()));
            });
        }
        target
    }

    /// What keeps any call of `<name>` off the compiled engine, whatever its
    /// arguments: a name resolved per call, or dispatch the engine does not
    /// model.
    // Cost: O(1) while no method is wrapped anywhere; else O(m) for the
    // caller package's MRO of m classes (`token_method_wrap_chain`).
    fn rx_call_blockers(&self, name: &NamedAtom, pkg: Symbol) -> Result<(), &'static str> {
        let spec = name.spec();
        // `<::(EXPR)>`: the rule's name is computed per call.
        if spec.lookup_name == "::" {
            return Err("symbolic-name");
        }
        // Dispatch the compiled engine does not model: a wrapped token (its
        // wrapper is user code around the rule's invocation), a custom HOW.
        if self.token_method_has_wrap_chain(pkg.as_str(), &spec.lookup_name) {
            return Err("wrapped");
        }
        if !self.registry().grammar_custom_how.is_empty() {
            return Err("custom-how");
        }
        Ok(())
    }

    /// [`Self::rx_call_target`] with the one verdict a method definition can
    /// change, and so is never cached: a call with no rule of its name that
    /// names a plain grammar METHOD calls the method.
    // Cost: O(1) expected.
    fn rx_call_target_checked(&mut self, name: &NamedAtom, pkg: Symbol, ic: bool) -> CallVerdict {
        let target = self.rx_call_target(name, pkg, ic)?;
        if matches!(target, CallTarget::Single) && self.subrule_names_user_method(name.spec(), pkg)
        {
            return Ok(CallTarget::Method);
        }
        Ok(target)
    }

    /// [`Self::rx_call_target`]'s cache miss: resolve the rule's candidates and
    /// decide the shape of the call.
    fn resolve_call_target(&mut self, name: &NamedAtom, pkg: Symbol, ic: bool) -> CallVerdict {
        let spec = name.spec();
        let (candidates, raw_empty) = self.parsed_subrule_candidates(spec, pkg, &[]);
        if raw_empty {
            return Ok(CallTarget::Single);
        }
        self.call_target_from_candidates(name, pkg, ic, candidates)
    }

    /// The shape of a call to the resolved `candidates`: a plain rule, a proto,
    /// or why it bridges.
    fn call_target_from_candidates(
        &mut self,
        name: &NamedAtom,
        pkg: Symbol,
        ic: bool,
        candidates: Arc<TokenCandidates>,
    ) -> CallVerdict {
        let spec = name.spec();
        if candidates.is_empty() {
            return Err("no-candidates");
        }
        // `:m` remaps positions across the whole result set, which the all-ends
        // entry does over the mark-stripped subject.
        if candidates.iter().any(|(parsed, _, _)| parsed.ignore_mark) {
            return Ok(CallTarget::Eager(candidates, "ignoremark-callee"));
        }
        // Several candidates without a proto dedup their ends across each
        // other; a mix of both is not a shape the walk's proto dispatch names.
        let proto = candidates.iter().all(|(_, _, sym)| sym.is_some());
        if !proto && (candidates.len() != 1 || candidates[0].2.is_some()) {
            return Err("multi-candidate");
        }
        // A wrapped proto candidate (`^find_method('p:sym<a>').wrap(..)`) is
        // user code around that candidate's invocation, like a wrapped rule.
        if proto
            && candidates
                .iter()
                .filter_map(|(_, _, sym)| sym.as_deref())
                .any(|k| self.proto_candidate_has_wrap_chain(pkg, &spec.lookup_name, k))
        {
            return Err("wrapped");
        }
        // The walk's eager arm scopes the caller's `:i` over a proto candidate's
        // body (`subrule_candidate_ends`), which needs the body compiled under it:
        // that call bridges. A plain call is the walk's streamed shape, which
        // does not inherit `:i` — and neither does rakudo.
        if proto && ic && candidates.iter().any(|(parsed, _, _)| !parsed.ignore_case) {
            return Err("proto-inherited-i");
        }
        if self.subrule_has_qq_thunks(&spec.lookup_name, pkg) {
            return Err("qq-thunks");
        }
        if !self.subrule_cannot_left_reenter(spec.lookup_sym, pkg) {
            return Ok(CallTarget::Eager(candidates, "lr-seed"));
        }
        if candidates
            .iter()
            .any(|(parsed, _, _)| program_for(parsed).is_none())
        {
            return Ok(CallTarget::Eager(candidates, "declined-callee"));
        }
        if proto {
            return Ok(CallTarget::Proto(candidates));
        }
        let (parsed, sub_pkg, _) = &candidates[0];
        Ok(CallTarget::Plain(
            Arc::clone(program_for(parsed).ok_or("callee-declined")?),
            *sub_pkg,
        ))
    }

    /// Evaluate a [`CallTarget::Eager`] call: every end of the callee at `pos`
    /// through the growing-seed loop, with the call's `window` installed for
    /// the evaluation only (it is eager, so nothing resumes in it), and the
    /// window's final values filed on each end's Match for its action, as the
    /// walk's producer files them. LOWEST PRIORITY FIRST.
    // Cost: the seed loop's (`subrule_seed_ends`), plus O(b + e·b) to install,
    // read back and file a window of b bindings on e ends.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn rx_lr_call_ends(
        &mut self,
        atom: &RegexAtom,
        candidates: &TokenCandidates,
        window: Option<CallWindow>,
        args: &[Value],
        chars: &[char],
        pos: usize,
        pkg: Symbol,
        options: (bool, bool),
    ) -> Vec<(usize, RegexCaptures)> {
        let RegexAtom::Named(name) = atom else {
            return Vec::new();
        };
        if let Some(keys) = window.as_ref().and_then(|w| w.scope_keys.as_ref()) {
            grammar_dynvar_scope_push(keys.iter().cloned());
        }
        let mut out = self.subrule_seed_ends(
            name.spec(),
            candidates,
            chars,
            pos,
            pkg,
            args,
            false,
            options,
        );
        let Some(window) = window else {
            return out;
        };
        let values: Vec<(String, Value)> = window
            .attach
            .iter()
            .filter_map(|key| self.env.get(key).map(|v| (key.clone(), v.clone())))
            .collect();
        if window.scope_keys.is_some() {
            grammar_dynvar_scope_pop();
        }
        self.restore_subrule_dynamic_params(window.saved);
        if !values.is_empty() {
            for (_, caps) in out.iter_mut() {
                Self::attach_grammar_dynvars_to_named_caps(caps, atom, &values);
            }
        }
        out
    }
}
