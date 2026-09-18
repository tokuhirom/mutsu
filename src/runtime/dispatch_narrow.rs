//! Declared-base-type narrowing for multi-sub dispatch
//! ([#8696](https://github.com/tokuhirom/mutsu/issues/8696) step 2).
//!
//! `func_multi_dispatch_type_cacheable` answers a question about a whole
//! *family*: is every candidate of this `multi` name type+arity deterministic?
//! One `where` / `subset` / literal candidate anywhere in the family makes the
//! answer `false`, and the sound resolution cache
//! (`func_multi_resolve_cache`) is then withheld from **every** call of that
//! name — so each call re-gathers, re-ranks and re-binds the whole candidate
//! list. That is why a family distinguished by `subset` constraints cost
//! O(candidates) per call while rakudo's cost is flat.
//!
//! The decomposition here is rakudo's. A `subset S of Int` cannot match a `Str`
//! argument, and that is decidable from the argument's *type* alone — no
//! predicate has to run, and the answer is the same for every call with those
//! argument types. So the family-level verdict is refined to a per-argument-type
//! one: if every value-dependent candidate is excluded by its **declared
//! nominal types** (or by arity) for these argument types, then the winner among
//! the candidates that remain is a pure function of the argument types after all,
//! and the resolution cache applies.
//!
//! Soundness rests on two rules, both enforced in
//! [`Interpreter::candidate_nominally_excluded`]:
//!
//! 1. **Only a check that runs no user code may exclude.** A nominal type test
//!    is a `type_matches_value` against a declared base type; it never evaluates
//!    a `where` clause, a subset predicate or a coercion.
//! 2. **The exclusion must be reached before any user code would have run.**
//!    `args_match_param_types_inner` walks the positional parameters left to
//!    right and returns at the first failure, testing a parameter's nominal type
//!    before its `where`. So a candidate only counts as excluded when the
//!    excluding parameter sits at or before the first parameter whose check
//!    could run user code. Without that rule, caching would skip a side effect
//!    that the uncached path performs (`multi f(Int $a where {say 1}, Int $b)`
//!    called as `f(1, "s")`).
//!
//! Everything this cannot prove stays on the uncached path, so the refinement
//! can only add cache hits, never change which candidate a call reaches.

use super::*;
use crate::ast::ParamDef;
use crate::runtime::types::unwrap_varref_value;

impl Interpreter {
    /// Whether the value-dependence that made `name`'s family uncacheable is
    /// *reachable* for arguments of these types — i.e. whether any
    /// value-dependent candidate could still bind. `false` licenses
    /// `func_multi_resolve_cache` for this one argument-type key.
    ///
    /// Memoized per `(package, name, argument type keys)`: the walk it performs
    /// is O(candidates), which is exactly the cost this exists to stop paying
    /// per call.
    pub(crate) fn func_multi_argkeys_cacheable(
        &mut self,
        pkg_sym: Symbol,
        name_sym: Symbol,
        name: &str,
        args: &[Value],
        arg_keys: &[Symbol],
    ) -> bool {
        // The family gate refuses a compunit-private name outright, because
        // this cache is keyed by (package, name) and such a name resolves
        // differently depending on which unit is asking. The refinement must
        // refuse it for the same reason — it is not a value-dependence verdict
        // it can narrow away.
        if self.is_unit_scoped_routine_name(name) {
            return false;
        }
        let memo_key = (pkg_sym, name_sym, arg_keys.to_vec());
        if let Some(&c) = self.func_multi_argkey_cacheable.get(&memo_key) {
            return c;
        }
        let candidates = self.resolve_all_multi_candidates_indexed(name);
        let mut cacheable = !candidates.is_empty();
        if cacheable {
            for def in &candidates {
                if !self.def_has_value_dependent_param(def) {
                    continue;
                }
                if !self.candidate_nominally_excluded(def, args) {
                    cacheable = false;
                    break;
                }
            }
        }
        self.func_multi_argkey_cacheable.insert(memo_key, cacheable);
        cacheable
    }

    /// Whether any parameter of `def` makes the enclosing multi's winner depend
    /// on something other than the `(type, definedness)` pair the resolve caches
    /// key on.
    ///
    /// This is the per-candidate half of `func_multi_dispatch_type_cacheable`'s
    /// loop, lifted out so the family gate and the per-argument-type refinement
    /// cannot drift apart: the refinement is only sound if it narrows away
    /// exactly the candidates that made the family gate say `false`.
    pub(crate) fn def_has_value_dependent_param(&self, def: &FunctionDef) -> bool {
        for pd in &def.param_defs {
            if pd.where_constraint.is_some() || pd.literal_value.is_some() {
                return true;
            }
            // A code-signature callback param (`&cb:(Int)`) or a capture
            // subsignature (`|c($a, $b)`) dispatches on the argument's
            // signature/shape, not its `value_type_name`.
            if pd.code_signature.is_some() || pd.sub_signature.is_some() {
                return true;
            }
            // A CONSTRAINED `&`-sigil parameter dispatches on the passed
            // routine's declared RETURN type, which no argument type key
            // records.
            if pd.name.starts_with('&') && pd.type_constraint.is_some() {
                return true;
            }
            // An `is rw` candidate matches only a writable-lvalue argument — a
            // call-site property, not an argument-type one.
            if pd.traits.iter().any(|t| t == "rw") {
                return true;
            }
            if let Some(tc) = &pd.type_constraint
                && self.type_constraint_is_value_dependent(tc)
            {
                return true;
            }
        }
        false
    }

    /// Whether `def` provably cannot bind these arguments, using only checks
    /// that run no user code and only up to the point where the real matcher
    /// would start running some.
    ///
    /// A `true` answer means the candidate is out of the running for *every*
    /// call with these argument types, so its value-dependence cannot influence
    /// the winner. A `false` answer means "not proven" — never "it matches".
    pub(crate) fn candidate_nominally_excluded(
        &mut self,
        def: &FunctionDef,
        args: &[Value],
    ) -> bool {
        // An auto-param sub (`$^a`) carries its parameters in `params` with an
        // empty `param_defs`; there is no declared type to read.
        if def.param_defs.is_empty() {
            return false;
        }
        // The same positional split `args_match_param_types_inner` uses: a
        // slurpy hash collects *named* arguments and is not a positional slot.
        let positional_params: Vec<&ParamDef> = def
            .param_defs
            .iter()
            .filter(|p| !(p.named || p.slurpy && p.name.starts_with('%')))
            .collect();
        let positional_args: Vec<&Value> = args
            .iter()
            .filter(|a| !a.unwrap_varref().is_string_pair_value())
            .collect();

        // Arity. Computed exactly as the matcher computes it, and decided
        // before any parameter is bound, so it can never skip user code.
        let mut required = 0usize;
        let mut max = 0usize;
        let mut variadic = false;
        for pd in &positional_params {
            if Self::param_is_variadic_slot(pd) {
                variadic = true;
                continue;
            }
            max += 1;
            if pd.default.is_none()
                && !pd.optional_marker
                && !pd.name.starts_with('@')
                && !pd.name.starts_with('%')
            {
                required += 1;
            }
        }
        if positional_args.len() < required {
            return true;
        }
        if !variadic && positional_args.len() > max {
            return true;
        }

        // Nominal types, left to right, stopping where the real matcher could
        // start running user code.
        for (slot, pd) in positional_params.iter().enumerate() {
            let Some(arg) = positional_args.get(slot) else {
                break;
            };
            if Self::param_is_variadic_slot(pd) {
                break;
            }
            // A `::T` capture is *bound* before the nominal check at the same
            // parameter, so skipping the candidate would skip the binding too.
            if pd.type_capture.is_some() {
                break;
            }
            if let Some(base) = self.param_nominal_base(pd) {
                let arg = unwrap_varref_value((*arg).clone()).deref_container();
                if !self.type_matches_value(&base, &arg) {
                    return true;
                }
            }
            // From here on the matcher may evaluate a `where`, a subset
            // predicate, a coercion or a default, so an exclusion found at a
            // later parameter is not one the uncached path would reach without
            // side effects.
            if self.param_check_runs_user_code(pd) {
                break;
            }
        }
        false
    }

    /// A positional slot that absorbs an unbounded number of arguments.
    fn param_is_variadic_slot(pd: &ParamDef) -> bool {
        pd.is_variadic()
            || pd.name == "_capture"
            || (pd.slurpy && pd.sigilless)
            || pd.is_capture_subsignature()
    }

    /// The declared nominal type an argument in `pd`'s slot must conform to,
    /// with a `subset` resolved to the base type it refines — or `None` when
    /// no such type can be read off the declaration without running code.
    ///
    /// `None` is always the safe answer: it only costs a cache miss.
    fn param_nominal_base(&self, pd: &ParamDef) -> Option<String> {
        let tc = pd.type_constraint.as_deref()?;
        // A `@`/`%`-sigil parameter's constraint names the ELEMENT type
        // (`Int @a`), not the argument's own type; a `&`-sigil parameter's
        // names the routine's RETURN type.
        if pd.name.starts_with('@') || pd.name.starts_with('%') || pd.name.starts_with('&') {
            return None;
        }
        let (base, _smiley) = crate::runtime::types::strip_type_smiley(tc);
        // A coercion (`Int(Str)`) accepts whatever it can coerce, and coercing
        // can run user code.
        if base.contains('(') {
            return None;
        }
        // A `::`-qualified refinement (an enum value, `Foo::Bar`) is matched by
        // identity rather than by a nominal type.
        if base.contains(':') {
            return None;
        }
        let mut root = base.split(['[', ' ']).next().unwrap_or(base).to_string();
        // A subset chain (`subset A of B`, `subset B of Int`) resolves to the
        // type at its end. Bounded so a cyclic declaration cannot spin.
        for _ in 0..16 {
            let Some(next) = self
                .registry()
                .subsets
                .get(&root)
                .map(|sd| sd.base.trim().to_string())
            else {
                break;
            };
            if next.is_empty() || next == root {
                return None;
            }
            let (next_base, _) = crate::runtime::types::strip_type_smiley(&next);
            if next_base.contains('(') || next_base.contains(':') {
                return None;
            }
            root = next_base
                .split(['[', ' '])
                .next()
                .unwrap_or(next_base)
                .to_string();
        }
        if self.registry().subsets.contains_key(&root) {
            // Still a subset after the walk: a cycle, or deeper than the bound.
            return None;
        }
        // Only a name whose meaning this interpreter actually models may
        // exclude. An unmodelled name would make `type_matches_value` answer
        // `false` for every argument and exclude candidates that do match.
        if !self.nominal_type_is_known(&root) {
            return None;
        }
        Some(root)
    }

    /// Whether the interpreter models `root` as a type, so a negative
    /// `type_matches_value` against it is real information rather than a
    /// missing registration.
    fn nominal_type_is_known(&self, root: &str) -> bool {
        if root.is_empty() {
            return false;
        }
        crate::runtime::utils::is_known_type_constraint(root)
            || self.registry().classes.contains_key(root)
            || self.registry().enum_types.contains_key(root)
            || self.is_role_type_name(root)
    }

    /// Whether checking `pd` against an argument can run user code — a `where`
    /// clause, a subset predicate, a coercion, a sub-/code-signature match, or
    /// the evaluation of a default for an unsupplied optional.
    fn param_check_runs_user_code(&self, pd: &ParamDef) -> bool {
        if pd.where_constraint.is_some()
            || pd.sub_signature.is_some()
            || pd.outer_sub_signature.is_some()
            || pd.code_signature.is_some()
            || pd.default.is_some()
        {
            return true;
        }
        pd.type_constraint
            .as_deref()
            .is_some_and(|tc| self.type_constraint_is_value_dependent(tc))
    }
}
