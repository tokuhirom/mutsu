//! Non-constant parameter defaults on the method fast binder (#18320,
//! ADR-15737 D1).
//!
//! The fast binder (`call_compiled_method_fast_in_unit`) binds every
//! parameter before the method frame exists, so it could only bind a default
//! that is a shareable constant (`CompiledFunction::const_fill_for_param`,
//! `NamedFast::fill`). Any other default -- `:$type = self.package`,
//! `$n = $!size` -- sent the whole call to the general binder, at two to three
//! times the cost of the fast one.
//!
//! Rakudo evaluates a default as part of the callee's signature binding,
//! inside the callee frame. This module gives the fast binder the same shape:
//! the binder leaves an omitted parameter with a precompiled default (ADR-0133)
//! *deferred*, finishes the frame, and the **default prologue** runs the
//! deferred defaults in parameter order right before the body, with the
//! general binder's own per-default steps (`eval_param_default`'s parameter
//! shadow and topic save, the default type check and coercion of
//! `checked_default_param_value`, the scalar itemization, `is copy`'s own
//! container).
//!
//! What the prologue cannot reproduce is the binder's *progress*: when the
//! general binder evaluates the default of parameter `i`, parameters after
//! `i` are not bound yet, so a name the default reads that a later parameter
//! declares resolves outside the signature. In the prologue every parameter
//! is already bound, so a default that names a later parameter (or reads
//! names by string through `EVAL`) is not deferred: such a method keeps the
//! general binder ([`default_prologue_safety`]).

use super::*;
use crate::ast::{Expr, ParamDef};
use crate::ast_visit::{NameKind, Visit, walk_expr};
use crate::vm::vm_method_frame_plan::LocalInit;

/// One omitted parameter whose default the prologue evaluates.
pub(crate) struct DeferredDefault {
    /// Index into the method's `param_defs`.
    pub(crate) param_idx: usize,
    /// The name the binder bound the parameter under.
    pub(crate) sym: Symbol,
}

/// Whether `pd`'s shape is one the default prologue binds: a `$` scalar
/// (positional or named, never an alias or an attributive one) with a
/// precompiled or constant default, no trait but `is copy` / `is raw`, and no
/// constraint the default's binding would have to do more than type-check
/// and coerce.
// Cost: O(t + len(tc)), t = traits, tc = the constraint's spelling.
pub(crate) fn default_prologue_shape_ok(pd: &ParamDef) -> bool {
    pd.default.as_ref().is_some_and(|default| {
        pd.code
            .get()
            .is_some_and(|code| code.default_chunk.is_some())
            || crate::opcode::CompiledFunction::constant_default_value(default).is_some()
    }) && !pd.is_invocant
        && !pd.sigilless
        && !pd.slurpy
        && !pd.double_slurpy
        && !pd.onearg
        && !pd.required
        && !pd.named_alias
        && !pd.is_does_target()
        && pd.where_constraint.is_none()
        && pd.sub_signature.is_none()
        && pd.outer_sub_signature.is_none()
        && pd.code_signature.is_none()
        && pd.literal_value.is_none()
        && pd.shape_constraints.is_none()
        && pd.type_capture.is_none()
        && pd.captured_type_name().is_none()
        && pd.name != "_"
        && pd
            .name
            .as_bytes()
            .first()
            .is_some_and(|b| b.is_ascii_alphabetic() || *b == b'_')
        && pd
            .traits
            .iter()
            .all(|t| matches!(t.as_str(), "copy" | "raw"))
        && pd.type_constraint.as_deref().is_none_or(|tc| {
            // A coercion or a native constraint binds a converted value
            // whose conversion only the general binder performs.
            !tc.contains('(')
                && tc.as_bytes().first().is_some_and(u8::is_ascii_uppercase)
                && !crate::qualified::is_type_capture(tc)
        })
}

/// Per parameter of `param_defs`: whether its default may be deferred to the
/// prologue. Its shape must be one the prologue binds
/// ([`default_prologue_shape_ok`]) and its default must read no name a LATER
/// parameter declares, nor names by string (`EVAL`).
// Cost: O(p * (d + p)), p = parameters, d = size of a default expression;
// computed once per method (`MethodDef::default_prologue`).
pub(crate) fn default_prologue_safety(param_defs: &[ParamDef]) -> Box<[bool]> {
    param_defs
        .iter()
        .enumerate()
        .map(|(i, pd)| {
            if !default_prologue_shape_ok(pd) {
                return false;
            }
            let Some(default) = pd.default.as_ref() else {
                return false;
            };
            let later: Vec<&str> = param_defs[i + 1..]
                .iter()
                .filter(|later| !later.name.is_empty())
                .map(|later| bare_var_name(&later.name))
                .collect();
            let mut scan = DefaultNameScan {
                later: &later,
                unsafe_name: false,
            };
            scan.visit_expr(default);
            !scan.unsafe_name
        })
        .collect()
}

/// A variable name without its sigils and twigil (`@a` → `a`, `$!x` → `x`),
/// so a parameter and a read of it compare equal however each spells it.
// Cost: O(1).
fn bare_var_name(name: &str) -> &str {
    name.trim_start_matches(['$', '@', '%', '&', '!', '.', '*', '^', ':', '?'])
}

/// The walk of [`default_prologue_safety`] (ADR-0137): does a default read a
/// later parameter's name, or reach names by string?
struct DefaultNameScan<'a> {
    later: &'a [&'a str],
    unsafe_name: bool,
}

impl<'ast> Visit<'ast> for DefaultNameScan<'_> {
    fn visit_expr(&mut self, expr: &'ast Expr) {
        if !self.unsafe_name {
            walk_expr(self, expr);
        }
    }

    fn visit_name(&mut self, name: &str, kind: NameKind) {
        match kind {
            NameKind::Call | NameKind::UserRoutineCall if matches!(name, "EVAL" | "EVALFILE") => {
                self.unsafe_name = true;
            }
            NameKind::Var
            | NameKind::ArrayVar
            | NameKind::HashVar
            | NameKind::CodeVar
            | NameKind::AssignTarget
            | NameKind::TempTarget => {
                let bare = bare_var_name(name);
                if self.later.contains(&bare) {
                    self.unsafe_name = true;
                }
            }
            _ => {}
        }
    }
}

impl Interpreter {
    /// Whether `args` carries a named argument for the named parameter `pd`
    /// (by the key the fast binder matches it with).
    // Cost: O(a), a = arguments.
    pub(crate) fn named_param_supplied(pd: &ParamDef, args: &[Value]) -> bool {
        let Some(key) = Self::named_param_fast_match_key(pd) else {
            return false;
        };
        args.iter().any(|a| {
            let a = a.unwrap_varref();
            a.is_string_pair_value()
                && matches!(a.view(), ValueView::Pair(k, _) if k.as_str() == key)
        })
    }

    /// Run the defaults the fast binder deferred (`deferred`, in parameter
    /// order) inside the method frame, and bind each result where the binder
    /// left a placeholder: every local slot planned for the parameter and the
    /// frame env, where the binder put every parameter by name for this
    /// purpose.
    // Cost: O(d * (p + l)), d = deferred defaults, p = parameters, l = locals,
    // plus running each default (a precompiled chunk).
    pub(crate) fn run_method_default_prologue(
        &mut self,
        method_def: &crate::runtime::MethodDef,
        owner_sym: Symbol,
        cc: &CompiledCode,
        deferred: &[DeferredDefault],
    ) -> Result<(), RuntimeError> {
        for d in deferred {
            let pd = &method_def.param_defs[d.param_idx];
            let Some(default_expr) = pd.default.as_ref() else {
                continue;
            };
            // A constant the fast binder could not fill (its constraint is
            // not one `const_fill_for_param` checks: `Capture:D $p = \()`)
            // binds as itself; anything else runs its precompiled chunk.
            let value = match crate::opcode::CompiledFunction::constant_default_value(default_expr)
            {
                Some(value) => value,
                None => {
                    crate::vm::vm_stats::record_param_default_prologue();
                    self.eval_param_default_scoped(pd, default_expr)?
                }
            };
            let value = self.checked_default_param_value(pd, value)?;
            let value = if pd.traits.iter().any(|t| t == "copy") {
                value.detach_shared_container()
            } else {
                value
            };
            let value = Self::itemize_plain_scalar_param(pd, value);
            let plan = method_def.frame_plan(owner_sym, cc);
            for (slot, init) in plan.locals.iter().enumerate() {
                if matches!(init, LocalInit::Param(idx) if *idx as usize == d.param_idx) {
                    self.locals[slot] = value.clone();
                }
            }
            if self.env().contains_key_sym(d.sym) {
                self.env_mut().insert_sym(d.sym, value);
            }
        }
        Ok(())
    }
}
