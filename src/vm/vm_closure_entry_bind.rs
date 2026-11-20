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
//! This is *not* a second binder. [`entry_params`] accepts a signature
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
use crate::ast::{ParamDef, ReadonlyKind};
use crate::light_closure_shape::is_plain_scalar_param_name;

/// What binding one parameter does, settled from its `ParamDef`.
#[derive(Debug)]
enum EntryParamKind {
    /// A positional `$` scalar: its itemization and the readonly mark the
    /// general binder's readonly pass leaves on it.
    Scalar {
        bind: ScalarParamBind,
        readonly: ReadonlyKind,
        /// `is raw`: a caller variable would be aliased.
        raw: bool,
    },
    /// A sigilless `\x`: bound as passed under its term key, with the two
    /// sigilless meta keys the general binder writes for a value binding.
    Sigilless {
        alias_key: Symbol,
        readonly_key: Symbol,
    },
}

/// One parameter of a signature [`entry_params`] accepted.
#[derive(Debug)]
pub(crate) struct EntryParam {
    /// The env key the parameter is stored under: the name for a `$`
    /// parameter, the term key for a sigilless one.
    storage: Symbol,
    /// `ParamDef::name`, for the `&`-sigiled alias of a `Callable`.
    name: Box<str>,
    kind: EntryParamKind,
    /// The parameter is implicitly `Any` (untyped); `false` for `Mu \x`.
    implicit_any: bool,
}

/// The [`EntryParam`]s of `param_defs`, or `None` when the entry bind may not
/// serve this signature.
// Cost: O(p + t), p = parameters, t = their total trait count.
pub(crate) fn entry_params(param_defs: &[ParamDef]) -> Option<Box<[EntryParam]>> {
    if param_defs.is_empty() {
        return None;
    }
    param_defs.iter().map(entry_param).collect()
}

// Cost: O(t + n), t = the parameter's trait count, n = its name length.
fn entry_param(pd: &ParamDef) -> Option<EntryParam> {
    let plain_positional = !pd.named
        && !pd.named_alias
        && !pd.slurpy
        && !pd.double_slurpy
        && !pd.onearg
        && pd.default.is_none()
        && pd.type_capture.is_none()
        && pd.literal_value.is_none()
        && pd.sub_signature.is_none()
        && pd.where_constraint.is_none()
        && pd.trait_args.is_empty()
        && !pd.optional_marker
        && pd.outer_sub_signature.is_none()
        && pd.code_signature.is_none()
        && !pd.is_invocant
        && !pd.does_target
        && pd.shape_constraints.is_none()
        && !pd.block_param;
    if !plain_positional || !is_plain_scalar_param_name(&pd.name) || pd.declares_self_lexical() {
        return None;
    }
    if pd.sigilless {
        // `\x` or `Mu \x`; no trait (a sigilless parameter is raw already).
        if !pd.traits.is_empty() || !matches!(pd.type_constraint.as_deref(), None | Some("Mu")) {
            return None;
        }
        let name = crate::symbol::Symbol::intern(&pd.name);
        return Some(EntryParam {
            storage: crate::term_names::term_key_sym(name),
            name: pd.name.as_str().into(),
            kind: EntryParamKind::Sigilless {
                alias_key: crate::runtime::sigilless_alias_key(&pd.storage_name()),
                readonly_key: crate::runtime::sigilless_readonly_key(&pd.storage_name()),
            },
            implicit_any: pd.type_constraint.is_none(),
        });
    }
    // A `$` scalar: untyped, with no trait but `is raw`.
    let raw = match pd.traits.as_slice() {
        [] => false,
        [t] if t == "raw" => true,
        _ => return None,
    };
    if pd.type_constraint.is_some() {
        return None;
    }
    Some(EntryParam {
        storage: Symbol::intern(&pd.name),
        name: pd.name.as_str().into(),
        kind: EntryParamKind::Scalar {
            bind: Interpreter::scalar_param_bind(pd),
            readonly: if raw {
                ReadonlyKind::Immutable
            } else {
                ReadonlyKind::Alias
            },
            raw,
        },
        implicit_any: true,
    })
}

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
    /// Bind `args` to a signature [`entry_params`] accepted (see the module
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
                EntryParamKind::Scalar { bind, readonly, .. } => {
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
