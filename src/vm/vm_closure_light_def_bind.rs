//! The **light `ParamDef` closure bind** (#10702).
//!
//! [`super::vm_closure_light_bind`] serves a closure
//! whose signature has no `ParamDef`s at all (a single-parameter pointy block).
//! A WhateverCode (`* + *`) or a multi-parameter pointy block (`-> $a, $b`)
//! *does* carry `ParamDef`s, so every call of one went through the general
//! signature binder's real path — which for an untyped positional `$`
//! parameter reaches one branch, and pays for the whole binder to get there:
//! two filtered argument `Vec`s, a `String` clone of each parameter name into
//! `raw_nonlvalue_params`, a `Symbol::intern` per parameter (the legacy-syms
//! entry point hands the binder no baked `ParamDef` symbols), an implicit-`Any`
//! `type_matches_value`, and the per-parameter type-constraint and readonly
//! passes. Measured on #10702's sequence-generator benchmark that was ~8k of
//! the ~19.4k instructions a `* + *` call cost.
//!
//! Like its sibling this is *not* a second binder. [`light_def_params`]
//! accepts a signature only when the general binder's per-parameter answers
//! depend on the declaration alone, and settles them once per code object;
//! [`Interpreter::closure_light_def_bind`] declines (so the general binder runs
//! and keeps its diagnostics) for every argument list whose binding would
//! depend on more than that.

use super::*;
use crate::ast::{ParamDef, ReadonlyKind};

/// One parameter of a signature [`light_def_params`] accepted, with the
/// answers the general binder derives from its `ParamDef` on every call.
#[derive(Debug)]
pub(crate) struct LightDefParam {
    /// `ParamDef::name`, interned.
    sym: Symbol,
    /// What binding does to the value's itemization: `Itemize` for a plain
    /// `$x`, `Keep` for `$x is raw` (and an itemization-exempt name).
    bind: ScalarParamBind,
    /// The mark the general binder's readonly pass leaves on the parameter.
    /// A plain `$x` is a readonly alias; an `is raw` one bound to a value with
    /// no caller container behind it (the only kind
    /// [`Interpreter::closure_light_def_bind`] accepts) is an immutable value.
    readonly: ReadonlyKind,
}

/// The [`LightDefParam`]s of `param_defs`, or `None` when the light bind may
/// not serve this signature.
///
/// Every parameter must be a plain positional `$` parameter with no default,
/// type, type capture, literal, `where`, sub-signature, code signature or
/// shape, and with no trait but `is raw`. Each of those has general-binder
/// behaviour this path does not reproduce: a type check or coercion, a
/// default to evaluate, `is rw`/`is copy` container handling, a sigilless or
/// `@`/`%`/`&` binding, named-argument matching, slurping. The struct is
/// destructured exhaustively so a new `ParamDef` field has to be classified
/// here before this compiles.
// Cost: O(p + t), p = parameter count, t = the parameters' total trait count.
pub(crate) fn light_def_params(param_defs: &[ParamDef]) -> Option<Box<[LightDefParam]>> {
    if param_defs.is_empty() {
        return None;
    }
    param_defs.iter().map(light_def_param).collect()
}

// Cost: O(t + n), t = the parameter's trait count, n = its name length.
fn light_def_param(pd: &ParamDef) -> Option<LightDefParam> {
    let ParamDef {
        name,
        default,
        multi_invocant: _,
        required: _,
        named,
        named_alias,
        slurpy,
        double_slurpy,
        onearg,
        sigilless,
        type_constraint,
        type_capture,
        literal_value,
        sub_signature,
        where_constraint,
        traits,
        trait_args,
        optional_marker,
        outer_sub_signature,
        code_signature,
        is_invocant,
        shape_constraints,
        // Only the implicit nominal type of an *unpassed* optional depends on
        // it, and an optional parameter is refused below.
        block_param: _,
        // The compiled `where` / default / shape chunks; their source
        // expressions are refused below.
        code: _,
    } = pd;
    let plain = !named
        && !named_alias
        && !slurpy
        && !double_slurpy
        && !onearg
        && !sigilless
        && !optional_marker
        && !is_invocant
        && default.is_none()
        && type_constraint.is_none()
        && type_capture.is_none()
        && literal_value.is_none()
        && sub_signature.is_none()
        && where_constraint.is_none()
        && outer_sub_signature.is_none()
        && code_signature.is_none()
        && shape_constraints.is_none()
        && trait_args.is_empty()
        && traits.iter().all(|t| t == "raw")
        && is_plain_scalar_param_name(name);
    if !plain {
        return None;
    }
    let is_raw = !traits.is_empty();
    Some(LightDefParam {
        sym: Symbol::intern(name),
        bind: Interpreter::scalar_param_bind(pd),
        readonly: if is_raw {
            ReadonlyKind::Immutable
        } else {
            ReadonlyKind::Alias
        },
    })
}

/// Whether a `ParamDef::name` is a plain `$` scalar's sigil-less name.
///
/// That rejects every sigil (`@x`, `%x`, `&x`) and twigil (`!x`, `.x`, `^x`,
/// `:x`), the topic `_`, `self` (which the binder mirrors onto the reserved
/// lexical key) and the parser's synthetic `__type_only__` / `__subsig__` /
/// `__type_capture__…` names. A WhateverCode's `__wc_N` and other
/// `_`-prefixed identifiers are accepted: their itemization (or exemption from
/// it) is [`Interpreter::scalar_param_bind`]'s to decide.
// Cost: O(n), n = the name's length.
fn is_plain_scalar_param_name(name: &str) -> bool {
    let mut chars = name.chars();
    match chars.next() {
        Some(c) if c.is_alphabetic() || c == '_' => {}
        _ => return false,
    }
    chars.all(|c| c.is_alphanumeric() || c == '_')
        && name != "_"
        && name != "self"
        && name != "__type_only__"
        && name != "__subsig__"
        && !name.starts_with("__type_capture__")
}

impl Interpreter {
    /// Bind `args` to a signature [`light_def_params`] accepted, without the
    /// general signature binder, when the arguments make that faithful.
    ///
    /// Returns `true` when the parameters are bound, `false` when nothing was
    /// done and the general binder must run. Exact arity is required, and
    /// every argument must be a plain value of a kind that is always `Any`:
    ///
    /// * a `Pair` / `ValuePair` is a named (or ambiguously named) argument;
    /// * a `VarRef`, or a call-site source name in `pending_call_arg_sources`,
    ///   names a caller variable — an `is raw` parameter then aliases it (with
    ///   writeback), and a plain one takes over its type constraint;
    /// * a `ContainerRef` / `HashEntryRef` cell is a writable location an
    ///   `is raw` parameter binds as such;
    /// * a `Junction`, a type object or an instance may fail the implicit
    ///   `Any` check (`Mu`, a class that `is Mu`), whose error is the binder's.
    ///
    /// For what is left, each parameter's binding is exactly the general
    /// binder's: the value with its precomputed itemization, a `&`-sigiled
    /// alias for a `Callable`, no type constraint (an untyped parameter drops
    /// any inherited one), the precomputed readonly mark, and every positional
    /// argument in `@_`.
    // Cost: O(p), p = parameter count.
    pub(super) fn closure_light_def_bind(
        &mut self,
        data: &crate::value::SubData,
        params: &[LightDefParam],
        args: &[Value],
    ) -> bool {
        if args.len() != params.len() || !args.iter().all(light_def_arg_is_plain_value) {
            return false;
        }
        if self.pending_call_arg_sources().is_some_and(|sources| {
            sources.len() == args.len() && sources.iter().any(Option::is_some)
        }) {
            return false;
        }

        // The one-shot channels the general binder consumes on every call
        // (see `closure_light_bind`); `pending_skip_where_recheck` describes
        // only the bind it was set for, and there is no `where` here to skip.
        self.pending_skip_where_recheck = false;
        self.take_pending_call_arg_sources();
        self.pending_call_arg_source_slots.clear();

        // The real path publishes every positional argument as `@_` (the
        // legacy path, by contrast, publishes only the unconsumed ones).
        self.env_mut().insert_sym(
            crate::symbol::wk::positional_slurpy(),
            Value::array(args.to_vec()),
        );

        for ((param, arg), pd) in params.iter().zip(args).zip(data.param_defs.iter()) {
            let val = Self::apply_scalar_param_bind(param.bind, arg.clone());
            // `bind_param_value_sym`'s only branch a plain `$` name can reach
            // besides the store itself.
            if matches!(
                val.view(),
                ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
            ) {
                self.env_mut().insert(format!("&{}", pd.name), val.clone());
            }
            self.env_mut().insert_sym_noting(param.sym, val);
            // `bind_param_type_constraint_sym(.., None)`: an untyped parameter
            // shadows a same-named typed lexical, so its inherited constraint
            // entry is dropped. When no constraint was ever registered under
            // this name there is no entry to drop.
            if Self::env_type_constraint_seen_for(param.sym) {
                self.env_mut()
                    .remove_sym(Self::type_meta_key_for_sym(param.sym));
            }
            self.mark_readonly_sym_with(param.sym, param.readonly);
        }
        true
    }
}

/// Whether an argument is a plain value that binds to an untyped `$`
/// parameter the same way whatever the parameter's traits — see
/// [`Interpreter::closure_light_def_bind`] for each refused kind.
// Cost: O(1).
fn light_def_arg_is_plain_value(arg: &Value) -> bool {
    !arg.is_varref()
        && matches!(
            arg.view(),
            ValueView::Int(_)
                | ValueView::BigInt(_)
                | ValueView::Num(_)
                | ValueView::Str(_)
                | ValueView::Bool(_)
                | ValueView::Rat(..)
                | ValueView::FatRat(..)
                | ValueView::BigRat(..)
                | ValueView::Complex(..)
                | ValueView::Array(..)
                | ValueView::Hash(_)
                | ValueView::Seq(_)
                | ValueView::Range(..)
                | ValueView::RangeExcl(..)
                | ValueView::RangeExclStart(..)
                | ValueView::RangeExclBoth(..)
                | ValueView::GenericRange { .. }
                | ValueView::Sub(_)
                | ValueView::WeakSub(_)
                | ValueView::Routine { .. }
        )
}
