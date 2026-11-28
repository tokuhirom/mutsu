//! A construction phase step (`BUILD`/`TWEAK`) run in the lean method frame
//! (`vm_method_lean_frame`, `LeanShapes::PhaseStep`) instead of the general
//! fast binder (#19399).
//!
//! `submethod TWEAK(:%!meta, :@!resources --> Nil) { ... }` is called once per
//! construction through `run_resolved_method_celled` -> `call_compiled_method`
//! -> the fast-path gate -> `call_compiled_method_fast_in_unit`, which asks on
//! every call about every feature a method might use. The answers are a
//! function of the submethod and its class -- the same reasoning that lets a
//! planned method call run in the lean frame (`vm_method_call_plan`) -- so the
//! construction plan decides them once ([`Interpreter::phase_step_lean_plan`]).
//!
//! # The gate's verdict, replayed
//!
//! The lean frame needs the gate's `fast` and `can_skip_merge`. For a
//! signature of the invocant, the implicit `*%_` and optional, untyped,
//! default-less attributive named parameters only, the gate's reading of the
//! arguments reduces to four per-call questions, which
//! [`Interpreter::phase_step_lean_args_ok`] asks on every call: every argument
//! a string-keyed Pair (`has_unmodeled_pair_arg`, `has_arg_mismatch`) whose key
//! is no internal `__mutsu_*` one, whose value is an `Any` (`named_mu_only_arg`)
//! and no `Proxy`, no `Seq` argument for an `@` parameter
//! (`method_binds_seq_into_array_param`), and no alias metadata on the
//! object's attribute cell (`has_attr_aliases`). Everything else the gate reads
//! is the signature and the compiled code. `can_skip_merge` is
//! `!cc.has_env_writes && !cc.has_calls` for such a signature (no aliasable,
//! shared or rw parameter), and the merge, when kept, is the runtime-name
//! carry only, which the lean frame performs. Debug builds run the real gate
//! on every lean call and assert it agrees.

use super::*;
use crate::vm::vm_method_lean_frame::{LeanMethodCall, LeanShapes};

/// What a construction plan records for a phase step the lean frame runs.
#[derive(Debug, Clone, Copy)]
pub(crate) struct PhaseStepLean {
    /// The class level the step runs at (its owner and its frame's receiver).
    owner: Symbol,
    /// `BUILD` or `TWEAK`.
    method_sym: Symbol,
    /// The gate's `can_skip_merge` for every call the per-call check admits.
    can_skip_merge: bool,
    /// `method_body_package(owner, owner, def)`, answered once.
    body_package: Option<Symbol>,
}

impl Interpreter {
    /// The lean-frame plan for the pinned phase-step candidate `def` of the
    /// class `owner` (see the module doc), or `None` when its shape needs the
    /// general binder. Decided once per construction plan.
    // Cost: O(p + l), p = parameters, l = local slots.
    pub(crate) fn phase_step_lean_plan(
        &mut self,
        owner: Symbol,
        method_sym: Symbol,
        def: &crate::runtime::MethodDef,
    ) -> Option<PhaseStepLean> {
        let cc = def.compiled_code.as_ref()?;
        let shape_ok = def.param_defs.iter().all(|pd| {
            if pd.is_invocant {
                return pd.implicit_invocant
                    && pd.type_constraint.is_none()
                    && pd.type_capture.is_none()
                    && pd.traits.is_empty()
                    && !pd.sigilless
                    && !crate::runtime::raw_invocant::param_is_raw_invocant(pd);
            }
            (pd.slurpy && pd.name == "%_" && pd.type_constraint.is_none())
                || super::vm_method_lean_frame::lean_attributive_named_ok(pd)
        });
        if !shape_ok
            || def.delegation.is_some()
            || def.deprecated_message.is_some()
            || self.cstruct_class_name(owner.as_str()).is_some()
            || self.types.published_mixins.may_hold(owner.as_str())
        {
            return None;
        }
        let can_skip_merge = !cc.has_env_writes && !cc.has_calls;
        // The receiver a phase step's frame sees is its own class level
        // (`run_resolved_method_celled(mro_class, ...)`).
        if !super::vm_method_lean_frame::lean_frame_eligible_with(
            self,
            def,
            cc,
            owner,
            owner,
            true,
            LeanShapes::PhaseStep,
        ) {
            return None;
        }
        Some(PhaseStepLean {
            owner,
            method_sym,
            can_skip_merge,
            body_package: self.method_body_package(owner.as_str(), owner, def),
        })
    }

    /// The per-call half of the gate's verdict for a lean phase step (see the
    /// module doc); `cell_has_alias` is whether the object's attribute cell
    /// carries sigilless-alias metadata.
    // Cost: O(a), a = arguments.
    fn phase_step_lean_args_ok(
        &mut self,
        def: &crate::runtime::MethodDef,
        args: &[Value],
        cell_has_alias: bool,
    ) -> bool {
        if cell_has_alias || Self::method_binds_seq_into_array_param(def, args) {
            return false;
        }
        for arg in args {
            if !arg.is_string_pair_value() {
                return false;
            }
            let (internal, value) = match arg.view() {
                ValueView::Pair(key, value) => (key.starts_with("__mutsu_"), value.clone()),
                _ => return false,
            };
            if internal || value.is_proxy_value() || !self.light_arg_is_any(&value) {
                return false;
            }
        }
        true
    }

    /// Run the pinned phase-step candidate `def` of the plan's class level
    /// in the lean frame, against the object under construction `inv`, as
    /// `run_resolved_method_celled` runs it on the general binder. `None` when
    /// this call's arguments need the general binder (nothing has run).
    // Cost: O(a), a = arguments, to decide; then the frame and the body.
    pub(crate) fn run_phase_step_lean(
        &mut self,
        plan: PhaseStepLean,
        def: &crate::runtime::MethodDef,
        inv: &Value,
        args: &[Value],
        cell_has_alias: bool,
    ) -> Option<Result<(Value, Option<AttrMap>), RuntimeError>> {
        let PhaseStepLean {
            owner, method_sym, ..
        } = plan;
        let cc = def.compiled_code.as_ref()?;
        if !self.phase_step_lean_args_ok(def, args, cell_has_alias) {
            return None;
        }
        #[cfg(debug_assertions)]
        {
            let cell = Self::self_instance_attrs(inv)?;
            let gate = self.method_fast_gate(
                def,
                cc,
                owner.as_str(),
                inv,
                crate::vm::vm_method_call_attrs::CallAttrs::Cell(&cell),
                args,
            );
            debug_assert!(
                gate.fast
                    && !gate.has_rw_params
                    && !gate.has_attr_aliases
                    && gate.can_skip_merge == plan.can_skip_merge
                    && (gate.can_skip_merge || gate.merge_is_name_carry_only),
                "phase-step lean plan disagrees with the fast-path gate: {gate:?}"
            );
        }
        // `run_resolved_method_celled`'s bracket: the sibling writes queued
        // for the caller are kept and merged with this body's own, and
        // `call_compiled_method_in_unit` starts the body's list empty.
        let saved_pending = std::mem::take(&mut self.pending_rw_writeback_sources);
        let empty_fns = CompiledFns::default();
        let fns_ref = def.compiled_fns.as_deref().unwrap_or(&empty_fns);
        let call = LeanMethodCall {
            receiver_class: owner,
            owner_sym: owner,
            method_sym,
            method_def: def,
            cc,
            compiled_fns: fns_ref,
            body_package: Some(plan.body_package),
            can_skip_merge: plan.can_skip_merge,
            slot_body: None,
        };
        let result = self.call_compiled_method_lean(&call, args.to_vec(), inv.clone());
        let mut merged = saved_pending;
        for src in std::mem::take(&mut self.pending_rw_writeback_sources) {
            if !merged.contains(&src) {
                merged.push(src);
            }
        }
        self.pending_rw_writeback_sources = merged;
        Some(match result {
            Ok((v, reconciled)) => {
                if def.is_submethod
                    && let Some(mut err) = self.failure_to_runtime_error_if_unhandled(&v)
                {
                    err.control = Some(crate::value::Control::Fail);
                    Err(err)
                } else {
                    Ok((v, reconciled))
                }
            }
            Err(e) => Err(e),
        })
    }
}
