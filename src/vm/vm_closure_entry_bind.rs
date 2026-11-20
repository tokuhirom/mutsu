//! The **closure entry bind** (#18267, stage 2 of O2): the full closure entry
//! (`call_compiled_closure_in_unit`) binds a plain positional signature
//! without the general signature binder when the light binds
//! ([`super::vm_closure_light_bind`], [`super::vm_closure_light_def_bind`])
//! decline it.
//!
//! The light def bind serves only arguments whose binding cannot depend on
//! anything but the declaration: it refuses an instance or a type object
//! (the implicit `Any` check), a caller variable (an argument source), and a
//! sigilless parameter. Those are exactly the shapes of the small helper
//! subs and callbacks that code passes around as values — AttrX::Mooish's
//! `my sub get-attr-obj(Mu \obj, $attr) is raw` called as
//! `get-attr-obj(self, $attr)`, a `FETCH`/`STORE` closure on a Proxy — and
//! each of those calls paid the whole general binder, ~21k instructions of
//! a ~38k closure call for `(Mu \obj, $attr)`.
//!
//! This is *not* a second binder. [`crate::closure_entry_params::entry_params`] accepts a signature
//! whose every parameter is a plain positional `$` scalar (optionally
//! `is raw`) or a sigilless `\x` / `Mu \x`, and settles once per code object
//! what the general binder derives from each `ParamDef`.
//! [`Interpreter::closure_entry_bind`] then reproduces, per argument, the one
//! branch of the general binder that argument reaches — and declines (so the
//! general binder runs and keeps its diagnostics) whenever the binding would
//! depend on more:
//!
//! * a plain `$x` given a caller variable takes over that variable's type
//!   constraint; accepted only when the variable provably has none (its
//!   `VarRef` says so, or no typed lexical was ever declared);
//! * an `is raw` or sigilless parameter given a caller variable aliases its
//!   container (with writeback) — declined; given a value, or `self`, it binds
//!   the value immutably, which this path reproduces;
//! * a sigilless parameter bound to a `Callable`, a container cell, a
//!   Junction or a `Nil` reaches other arms — declined.

use super::*;
use crate::closure_entry_params::{EntryParam, EntryParamKind};

/// Whether an argument binds to an [`EntryParam`] through the one general
/// binder arm this path reproduces: a plain value (see
/// `light_def_arg_is_plain_value`), an instance or a type object. Containers,
/// Pairs, Junctions, `Nil` and Proxies reach other arms.
// Cost: O(1).
fn entry_arg_kind_ok(arg: &Value) -> bool {
    super::vm_closure_light_def_bind::light_def_arg_is_plain_value(arg)
        || matches!(
            arg.view(),
            ValueView::Instance { .. } | ValueView::Package(_)
        )
}

impl Interpreter {
    /// Bind `args` to a signature [`crate::closure_entry_params::entry_params`] accepted (see the module
    /// docs), or return `false` with nothing done.
    // Cost: O(p), p = parameter count.
    pub(super) fn closure_entry_bind(
        &mut self,
        data: &crate::value::SubData,
        params: &[EntryParam],
        args: &[Value],
    ) -> bool {
        if args.len() != params.len() {
            return false;
        }
        let sources = self
            .pending_call_arg_sources()
            .filter(|sources| sources.len() == args.len());
        for (i, (param, arg)) in params.iter().zip(args).enumerate() {
            let source = sources
                .as_ref()
                .and_then(|s| s.get(i))
                .and_then(|s| s.as_deref());
            let value = arg.unwrap_varref();
            if !entry_arg_kind_ok(value) {
                return false;
            }
            let from_variable = arg.is_varref() || source.is_some_and(|s| s != "self");
            match param.kind {
                EntryParamKind::Scalar { raw, .. } => {
                    // An Array/Hash from a variable is shared through a cell
                    // with an exit writeback (the general binder's
                    // `scalar_container_share`).
                    if from_variable
                        && (raw
                            || matches!(value.view(), ValueView::Array(..) | ValueView::Hash(_))
                            || !Self::entry_source_untyped(arg))
                    {
                        return false;
                    }
                }
                EntryParamKind::Sigilless { .. } => {
                    // A variable is aliased; a Callable also gets an
                    // `&`-alias the meta-key arm does not reproduce; a type
                    // object with a source is the "source type object" arm.
                    if from_variable
                        || matches!(
                            value.view(),
                            ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
                        )
                        || (source.is_some() && matches!(value.view(), ValueView::Package(_)))
                    {
                        return false;
                    }
                }
            }
        }
        // The implicit `Any` of an untyped parameter, after the source checks
        // above have released the borrow of the argument sources.
        for (param, arg) in params.iter().zip(args) {
            if param.implicit_any && !self.light_arg_is_any(arg.unwrap_varref()) {
                return false;
            }
        }

        // The one-shot channels the general binder consumes on every call
        // (see `closure_light_def_bind`).
        self.pending_skip_constraint_recheck = false;
        self.discard_pending_call_arg_sources();
        self.pending_call_arg_source_slots.clear();

        if Self::call_owns_args_array(data) {
            let plain: Vec<Value> = args.iter().map(|a| a.unwrap_varref().clone()).collect();
            self.env_mut()
                .insert_sym(crate::symbol::wk::positional_slurpy(), Value::array(plain));
        }

        for (param, arg) in params.iter().zip(args) {
            let arg = arg.unwrap_varref().clone();
            match param.kind {
                EntryParamKind::Scalar { raw, readonly } => {
                    // An untyped `$` is an item binding; `is raw` binds as passed.
                    let bind = if raw {
                        ScalarParamBind::Keep
                    } else {
                        ScalarParamBind::Itemize
                    };
                    let val = Self::apply_scalar_param_bind(bind, arg);
                    if matches!(
                        val.view(),
                        ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
                    ) {
                        self.env_mut()
                            .insert(format!("&{}", param.name), val.clone());
                    }
                    self.env_mut().insert_sym_noting(param.storage, val);
                    if Self::env_type_constraint_seen_for(param.storage) {
                        self.env_mut()
                            .remove_sym(Self::type_meta_key_for_sym(param.storage));
                    }
                    self.mark_readonly_sym_with(param.storage, readonly);
                }
                EntryParamKind::Sigilless {
                    alias_key,
                    readonly_key,
                } => {
                    // The general binder's value arm for a sigilless
                    // parameter (`implicit_raw_veto`): no alias, readonly.
                    self.lexicals.sigilless_alias_seen = true;
                    let val = if arg.needs_hllize_for_binding() {
                        arg.hllize_for_binding()
                    } else {
                        arg
                    };
                    let env = self.env_mut();
                    env.remove_sym(alias_key);
                    env.insert_sym_noting(readonly_key, Value::TRUE);
                    env.insert_sym_noting(param.storage, val);
                    if Self::env_type_constraint_seen_for(param.storage) {
                        self.env_mut()
                            .remove_sym(Self::type_meta_key_for_sym(param.storage));
                    }
                }
            }
        }
        true
    }

    /// Whether a caller variable handed to a plain `$` parameter carries no
    /// type constraint the parameter would take over.
    // Cost: O(1).
    fn entry_source_untyped(arg: &Value) -> bool {
        match arg.varref_declared() {
            Some(crate::value::VarRefDeclared::Resolved(declared)) => declared.is_none(),
            _ => !Self::env_type_constraint_seen(),
        }
    }
}
