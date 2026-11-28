//! Construction phase steps whose whole effect is their attributive named
//! parameters: `submethod BUILD(:$!a, :@!b, :%!c) { }` and the same `TWEAK`.
//!
//! Such a submethod has an empty body, so running it binds the attributes and
//! does nothing else. The general path still pushed a method frame for it
//! (the call frame, the env overlay, the routine and method-class frames, the
//! parameter list, the result finalize): about 20k instructions per
//! construction for a step that writes one attribute (#19399,
//! `bench-ctor`'s `Spec::TWEAK(:$!spec) { }`).
//!
//! A bind-only step binds through the very code the fast binder runs
//! (`bind_named_param_fast`, the fast binders' shared named arm), and only when
//! the fast-path gate admits the call, so the attributes end up exactly as
//! the frame would have left them; any call the gate refuses (a typed or
//! defaulted parameter, a `Mu` argument, alias metadata on the cell, ...)
//! takes the full path. What the frame would have done besides binding is
//! nothing: the body has no ops, so it reads nothing, calls nothing and
//! returns `Nil`, which the return spec the plan admits (none, or `--> Nil`)
//! leaves as it is.

use super::*;
use crate::vm::vm_method_call_attrs::CallAttrs;

/// Whether `def` (pinned for a phase step of `owner`) is bind-only: its
/// compiled body has no ops and every parameter is the invocant, the implicit
/// `*%_`, or an optional, untyped, trait-less attributive named parameter
/// with no default. Decided once at plan build.
// Cost: O(p), p = parameters.
pub(super) fn phase_step_bind_only(def: &MethodDef) -> bool {
    let Some(cc) = def.compiled_code.as_ref() else {
        return false;
    };
    cc.ops.is_empty()
        && cc.closure_compiled_codes.is_empty()
        && def
            .return_type
            .as_deref()
            .is_none_or(|spec| spec == "Nil")
        && def.param_defs.iter().all(|pd| {
            if pd.is_invocant {
                return pd.implicit_invocant && pd.type_constraint.is_none();
            }
            if pd.slurpy && pd.name == "%_" {
                return true;
            }
            pd.named
                && crate::value::attr_twigil_base(&pd.name).is_some()
                && Interpreter::named_param_fast_match_key(pd).is_some()
                && !pd.required
                && pd.default.is_none()
                && pd.type_constraint.is_none()
                && pd.where_constraint.is_none()
                && pd.sub_signature.is_none()
                && pd.outer_sub_signature.is_none()
                && pd.code_signature.is_none()
                && pd.captured_type_name().is_none()
                && pd.literal_value.is_none()
                && pd.shape_constraints.is_none()
                && !pd.has_traits()
                && !pd.slurpy
                && !pd.double_slurpy
                && !pd.sigilless
        })
}

impl Interpreter {
    /// Run a bind-only phase step (see the module doc): bind `def`'s
    /// attributive named parameters from `args` into `cell`, the attribute
    /// cell of the object under construction `inv`. `false`, with nothing
    /// written, when the fast-path gate refuses this call: the caller then
    /// runs the step's frame as before.
    // Cost: O(p * a), p = parameters, a = arguments (the gate and the named
    // matches).
    pub(super) fn run_bind_only_phase_step(
        &mut self,
        owner: &str,
        owner_sym: Symbol,
        def: &MethodDef,
        inv: &Value,
        cell: &crate::gc::Gc<crate::value::InstanceAttrs>,
        args: &[Value],
    ) -> bool {
        let Some(cc) = def.compiled_code.as_ref() else {
            return false;
        };
        // A CStruct keeps its fields in native memory, which the frame
        // re-reads and writes back around the call.
        if self.cstruct_class_name(owner).is_some() {
            return false;
        }
        let gate = self.method_fast_gate(def, cc, owner, inv, CallAttrs::Cell(cell), args);
        if !gate.fast || gate.has_rw_params || gate.has_attr_aliases {
            return false;
        }
        for (idx, pd) in def.param_defs.iter().enumerate() {
            if pd.is_invocant || !pd.named {
                continue;
            }
            // The fast binder's own named arm (`vm_named_param_bind`); the
            // shapes `phase_step_bind_only` admits need nothing of the caller
            // beyond the attribute write it performs.
            self.bind_named_param_fast(owner_sym, def, idx, pd, args, Some(cell));
        }
        true
    }
}
