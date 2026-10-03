//! The candidate-matching fast path for a plain positional signature
//! ([#10107](https://github.com/tokuhirom/mutsu/issues/10107)).
//!
//! `args_match_param_types_inner` answers "does this candidate accept these
//! arguments" for every signature shape there is: named and slurpy
//! parameters, captures, sub-signatures, coercions, type captures, `where`
//! blocks that read sibling parameters. It pays for that generality on every
//! candidate of every value-dependent multi call: an env snapshot and scoped
//! child, four scratch vectors, and a sibling-parameter bind per `where`.
//!
//! Most value-dependent candidates are nothing like that: a few scalar
//! positionals, each a nominal type, a `subset`, or a one-argument
//! WhateverCode `where` (`multi size(Small $x)`, `multi size(Int $x where * <
//! 10_000)`). For those the answer needs only the type checks and the
//! predicates, each of which reads nothing but the argument itself, so this
//! path runs exactly those and declines everything else back to the general
//! walk.
use super::*;

impl Interpreter {
    /// Match `args` against a plain positional signature, or `None` when the
    /// signature or the call has a shape only the general walk handles.
    ///
    /// A plain signature is scalar positional parameters (`$x`, typed or
    /// not), none optional, defaulted, slurpy, sigilless, trait-carrying,
    /// captured, destructured or literal. A parameter's type may be a nominal
    /// type or a `subset`, and its `where` only a one-argument WhateverCode
    /// the compiler precompiled into an inline predicate (ADR-0133): such a
    /// predicate reads `$_` alone, so it needs neither the env snapshot nor the
    /// earlier parameters bound. A call qualifies when every argument is
    /// positional and there is one per parameter.
    // Cost: O(p) type checks plus the predicates, p = parameters.
    pub(super) fn args_match_simple_positional(
        &mut self,
        args: &[Value],
        param_defs: &[ParamDef],
        multi_dispatch: bool,
        candidate_package: Option<Symbol>,
    ) -> Option<bool> {
        if args.len() != param_defs.len()
            || !param_defs.iter().all(Self::is_simple_positional_param)
            || args
                .iter()
                .any(|arg| arg.unwrap_varref().is_string_pair_value())
        {
            return None;
        }
        // A constraint that still needs resolving (a type capture, a package
        // alias, a generic) is the general walk's to resolve.
        if param_defs.iter().any(|pd| {
            pd.type_constraint
                .as_deref()
                .is_some_and(|tc| self.try_resolved_type_capture_name(tc).is_some())
        }) {
            return None;
        }
        if !Self::simple_where_reads_no_parameter(param_defs) {
            return None;
        }
        for (idx, (pd, raw)) in param_defs.iter().zip(args).enumerate() {
            let arg = unwrap_varref_value_for_dispatch(raw);
            match pd.type_constraint.as_deref() {
                Some(tc) => {
                    if multi_dispatch
                        && !self.native_dispatch_arg_matches(tc, args, Some(idx), &arg)
                    {
                        return Some(false);
                    }
                    if !self.param_constraint_accepts(tc, &arg) {
                        return Some(false);
                    }
                }
                // An untyped `$` parameter is implicitly `Any`: it rejects a
                // Junction (autothreading happens above dispatch) and `Mu`.
                None => {
                    if !self.type_matches_value("Any", &arg) {
                        return Some(false);
                    }
                }
            }
            if pd.where_constraint.is_some()
                && !self.with_candidate_package(candidate_package, |this| {
                    this.simple_where_holds(pd, arg)
                })
            {
                return Some(false);
            }
        }
        Some(true)
    }

    /// Whether no parameter's precompiled `where` names one of the
    /// signature's own parameters (`$x where * > $lo`): such a predicate needs
    /// the earlier ones bound, which only the general walk does.
    pub(crate) fn simple_where_reads_no_parameter(param_defs: &[ParamDef]) -> bool {
        !param_defs.iter().any(|pd| {
            pd.code
                .get()
                .and_then(|chunks| chunks.where_chunk.as_ref())
                .is_some_and(|chunk| {
                    chunk.code.free_var_syms.iter().any(|sym| {
                        sym.with_str(|name| param_defs.iter().any(|other| other.name == name))
                    })
                })
        })
    }

    /// Run a plain positional parameter's inline `where` predicate against
    /// `value`, with `$_` bound to it for the duration. An inline predicate
    /// answers `Ok(Bool)`, and a throw inside it already came back as `False`.
    pub(crate) fn simple_where_holds(&mut self, pd: &ParamDef, value: Value) -> bool {
        let topic = crate::symbol::wk::topic();
        let saved_topic = self.env.get_sym(topic).cloned();
        self.env.insert_sym(topic, value);
        let verdict = self.eval_param_where_value(pd, false);
        match saved_topic {
            Some(saved) => self.env.insert_sym(topic, saved),
            None => self.env.remove_sym(topic),
        };
        verdict.is_ok_and(|v| v.truthy())
    }

    pub(crate) fn is_simple_positional_param(pd: &ParamDef) -> bool {
        // A scalar parameter's name carries no sigil (`$x` is `x`); `@`/`%`/`&`
        // parameters and the synthetic `_capture` / `__type_only__` names do.
        !pd.name.is_empty()
            && !pd.name.starts_with(['@', '%', '&'])
            && !pd.name.starts_with("__")
            && pd.name != "_capture"
            && !pd.named
            && !pd.slurpy
            && !pd.double_slurpy
            && !pd.onearg
            && !pd.sigilless
            && !pd.is_invocant
            && !pd.optional_marker
            && pd.default.is_none()
            && pd.literal_value.is_none()
            && pd.type_capture.is_none()
            && pd.traits.is_empty()
            && pd.sub_signature.is_none()
            && pd.outer_sub_signature.is_none()
            && pd.code_signature.is_none()
            && pd.shape_constraints.is_none()
            && pd.type_constraint.as_deref().is_none_or(|tc| {
                // A smiley, coercion, parameterization or `::` form keeps the
                // general walk's handling of it.
                !tc.is_empty() && !tc.contains([':', '(', '[', '{'])
            })
            && (pd.where_constraint.is_none()
                || pd
                    .code
                    .get()
                    .is_some_and(|chunks| chunks.where_inline_predicate))
    }
}

/// The value a plain positional parameter's checks apply to: through the
/// `VarRef` tag of a by-variable argument and through a shared cell.
pub(crate) fn unwrap_varref_value_for_dispatch(raw: &Value) -> Value {
    unwrap_varref_value(raw.clone()).deref_container()
}
