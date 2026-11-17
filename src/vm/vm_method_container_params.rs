//! Container parameters (`is raw`, sigilless `\x`) on the method fast binder
//! (ADR-15737 D1, the `is raw` slice).
//!
//! A method parameter that binds the caller's container used to send every
//! call to the general binder, which realizes the alias by name (an env alias
//! key plus an exit writeback) at ~30-80k instructions a call against ~12k for
//! the same method with a plain `$x`. The positional-light sub call already
//! binds such a parameter through the container transport ADR-0109 gave
//! `is rw` (`vm_call_light_sigilless.rs`): the caller's variable is promoted
//! to a shared `ContainerRef` cell and the parameter holds that same cell, so
//! every read and write goes through one container and no writeback exists to
//! forget. This module gives the method fast binder the same transport.
//!
//! A method call's arguments arrive as values, with the call site's
//! argument-source names and slots alongside (`pending_call_arg_sources`,
//! `pending_call_arg_source_slots`), so the classification reads those instead
//! of a `VarRef`. It runs in two steps around the fast-path gate:
//!
//! * [`Interpreter::method_container_args_light`] (part of
//!   `method_fast_gate`, read-only) classifies every container parameter's
//!   argument and admits the call only when each has a bind this path
//!   performs;
//! * [`Interpreter::promote_method_container_args`] then rewrites the admitted
//!   arguments in place -- a caller variable becomes its shared cell -- while
//!   the CALLER's frame is still current, before the method frame is pushed.
//!
//! The binder (`call_compiled_method_fast_in_unit`) then binds a cell or a
//! `Proxy` argument as a writable container and anything else as a read-only
//! term. Every shape this module cannot classify with certainty declines, and
//! the call keeps the general binder exactly as before.

use super::*;

/// How one container parameter binds its argument on the method fast path.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum MethodContainerBind {
    /// A value with no container behind it: a read-only term.
    Readonly,
    /// The argument already is a shared cell: bind that cell.
    Relay,
    /// A `Proxy` (a `:=`-bound one in the caller, or an expression's result):
    /// the parameter holds the Proxy itself, as the general binder keeps it
    /// for a raw parameter, so reads FETCH and writes STORE through it.
    Proxy,
    /// A caller lexical whose slot already holds a shared cell.
    RelayHeld(u32),
    /// A caller lexical still held as a plain value: promote its slot.
    Promote(u32),
}

impl MethodContainerBind {
    /// Whether the bind aliases a writable container.
    // Cost: O(1).
    fn writable(self) -> bool {
        !matches!(self, MethodContainerBind::Readonly)
    }
}

impl Interpreter {
    /// Whether the container parameter `pd` has a shape this path binds:
    /// positional, mandatory, no `where`, no sub-signature, a sigilless one
    /// with no trait or a `$`-sigiled one whose only trait is `raw`, and no
    /// native or coercion constraint.
    // Cost: O(t + len(tc)), t = traits, tc = the constraint's spelling.
    pub(crate) fn method_container_param_ok(pd: &crate::ast::ParamDef) -> bool {
        let shape = if pd.sigilless {
            Self::light_sigilless_param_ok(pd)
        } else {
            pd.traits.len() == 1
                && pd.traits[0] == "raw"
                && !pd.name.is_empty()
                && !pd.name.starts_with(['@', '%', '&', '!', '.', '*', '?', '^', ':'])
                && !pd.named
                && !pd.slurpy
                && !pd.double_slurpy
                && !pd.onearg
                && !pd.is_invocant
                && !pd.optional_marker
                && pd.default.is_none()
                && pd.where_constraint.is_none()
                && pd.sub_signature.is_none()
                && pd.outer_sub_signature.is_none()
                && pd.code_signature.is_none()
                && pd.literal_value.is_none()
                && pd.type_capture.is_none()
        };
        shape
            && !pd.is_does_target()
            && pd.type_constraint.as_deref().is_none_or(|tc| {
                !tc.contains('(')
                    && !crate::runtime::native_types::is_native_int_type(tc)
                    && !matches!(tc, "str" | "num" | "num32" | "num64")
            })
    }

    /// Classify argument `arg_idx` (`arg`) of the container parameter `pd`
    /// the way the general binder would bind it, or `None` for every shape
    /// whose binding needs that binder: an attribute, dynamic, topic or
    /// qualified source, a name the general binder aliased by name, a source
    /// with no slot of the running frame, an element lvalue, or a value whose
    /// type the parameter rejects.
    // Cost: O(1) env probes, plus one type check against the parameter's
    // constraint.
    pub(crate) fn method_container_arg_bind(
        &mut self,
        pd: &crate::ast::ParamDef,
        arg_idx: usize,
        arg: &Value,
        arg_count: usize,
    ) -> Option<MethodContainerBind> {
        let source = self
            .pending_call_arg_sources()
            .filter(|sources| sources.len() == arg_count)
            .map(|sources| sources.get(arg_idx).cloned().flatten());
        let bind = match source {
            Some(Some(name)) => self.method_container_source_bind(&name, arg)?,
            // The call site says the argument is no variable.
            Some(None) => Self::method_container_value_bind(arg)?,
            // No source names at all: only a sigilless parameter keeps the
            // read-only bind it always had here; an `is raw` one keeps the
            // general binder, which also consults the caller's env.
            None if pd.sigilless => Self::method_container_value_bind(arg)?,
            None => return None,
        };
        // A Proxy binds itself, which only an unconstrained (`Mu`) parameter
        // does without fetching.
        if bind == MethodContainerBind::Proxy && pd.type_constraint.as_deref() != Some("Mu") {
            return None;
        }
        if bind != MethodContainerBind::Proxy {
            let probe = match bind {
                MethodContainerBind::RelayHeld(slot) => self.locals.get(slot as usize)?.clone(),
                _ => arg.clone(),
            };
            let probe = crate::runtime::types::unwrap_varref_value(probe).deref_container();
            if !self.method_container_type_ok(pd, &probe) {
                return None;
            }
        }
        Some(bind)
    }

    /// The bind of an argument the call site wrote as the variable `name`.
    // Cost: O(1) env probes.
    fn method_container_source_bind(&self, name: &str, arg: &Value) -> Option<MethodContainerBind> {
        let sym = Symbol::lookup(name);
        // The frame's own fixtures (the invocant, `?CLASS`, `?ROLE`) are
        // values, never writable variables.
        if sym.is_some_and(|s| s.is_frame_fixture()) {
            return Some(MethodContainerBind::Readonly);
        }
        // The topic, attributes, dynamics, pseudo-packages and qualified
        // names each have their own rule in the general binder.
        if matches!(name, "" | "_" | "!" | "/")
            || name.starts_with(['?', '*', '@', '%', '&', '!', '.', '^', ':'])
            || sym.is_some_and(crate::qualified::is_qualified)
        {
            return None;
        }
        if matches!(arg.view(), ValueView::HashEntryRef { .. }) {
            return None;
        }
        // A read-only binding (a caller's own sigilless parameter bound to a
        // value, `my $x := 5`) has no writable container to alias.
        if self.name_is_readonly_binding_for(name, sym) {
            return (!arg.unwrap_varref().is_proxy_value())
                .then_some(MethodContainerBind::Readonly);
        }
        // A name the general binder aliased by name has its root container
        // elsewhere.
        if self.lexicals.sigilless_alias_seen
            && let Some(sym) = sym
            && self
                .env()
                .get_sym(Self::sigilless_alias_key_for_sym(sym))
                .is_some()
        {
            return None;
        }
        let slot = *self.pending_call_arg_source_slots.get(&sym?)?;
        let code = self.running_code()?;
        if code.locals.get(slot as usize).map(String::as_str) != Some(name) {
            return None;
        }
        let held = self.locals.get(slot as usize)?;
        if held.is_proxy_value() {
            Some(MethodContainerBind::Proxy)
        } else if held.is_container_ref() {
            Self::method_container_cell_ok(held).then_some(MethodContainerBind::RelayHeld(slot))
        } else if matches!(held.view(), ValueView::Array(..))
            && crate::runtime::types::is_multidim_slice_cells(held)
        {
            None
        } else {
            Some(MethodContainerBind::Promote(slot))
        }
    }

    /// The bind of an argument that names no variable.
    // Cost: O(1); O(n) for an Array argument, n = its elements.
    fn method_container_value_bind(arg: &Value) -> Option<MethodContainerBind> {
        match arg.view() {
            ValueView::ContainerRef(_) => {
                Self::method_container_cell_ok(arg).then_some(MethodContainerBind::Relay)
            }
            ValueView::HashEntryRef { .. } | ValueView::VarRef { .. } => None,
            ValueView::Proxy { .. } => Some(MethodContainerBind::Proxy),
            ValueView::Array(..) if crate::runtime::types::is_multidim_slice_cells(arg) => None,
            _ => Some(MethodContainerBind::Readonly),
        }
    }

    /// A cell that is not a read-only binding (`my $x := 5` holds one, and
    /// only the general binder reports writes through it correctly).
    // Cost: O(1) until a read-only binding cell exists; then O(c), c = cells
    // on the binding chain (bounded).
    fn method_container_cell_ok(cell: &Value) -> bool {
        !crate::value::readonly_binding_cells_possible()
            || !matches!(Self::binding_chain_readonly_kind(cell), Some(Some(_)))
    }

    /// Whether `value` passes `pd`'s constraint the way the fast binder checks
    /// a parameter: a built-in nominal type (with an optional smiley) checked
    /// here, an untyped `$x is raw` as its implicit `Any`. Any other
    /// constraint declines, so the general binder reports the failure.
    // Cost: one type check.
    fn method_container_type_ok(&mut self, pd: &crate::ast::ParamDef, value: &Value) -> bool {
        match pd.type_constraint.as_deref() {
            None => {
                !crate::opcode::FastParamCheck::implicitly_any(pd) || self.light_arg_is_any(value)
            }
            Some("Mu") => true,
            Some(tc) => {
                let base = tc
                    .strip_suffix(":D")
                    .or_else(|| tc.strip_suffix(":U"))
                    .unwrap_or(tc);
                crate::runtime::utils::is_known_type_constraint(base)
                    && self.type_matches_value(tc, value)
            }
        }
    }

    /// Whether every container parameter of `method_def` has an argument this
    /// path binds ([`Self::method_container_arg_bind`]). A body that may
    /// re-dispatch (`callsame`, `samewith`) keeps the general binder whenever
    /// a bind aliases a container: the re-dispatch hands the arguments on as
    /// they arrived.
    // Cost: O(p + a), p = parameters, a = arguments.
    pub(crate) fn method_container_args_light(
        &mut self,
        method_def: &crate::runtime::MethodDef,
        cc: &CompiledCode,
        args: &[Value],
    ) -> bool {
        let mut positional = args
            .iter()
            .enumerate()
            .filter(|(_, a)| !a.unwrap_varref().is_string_pair_value());
        for pd in method_def.param_defs.iter() {
            if pd.is_invocant {
                if pd.binds_caller_container() || pd.is_does_target() {
                    return false;
                }
                continue;
            }
            let consumes = !pd.named && !pd.slurpy && !pd.double_slurpy;
            let arg = if consumes { positional.next() } else { None };
            if !(pd.binds_caller_container() || pd.is_does_target()) {
                continue;
            }
            if !Self::method_container_param_ok(pd) {
                return false;
            }
            let Some((idx, arg)) = arg else {
                return false;
            };
            match self.method_container_arg_bind(pd, idx, arg, args.len()) {
                Some(bind) if bind.writable() && cc.uses_dispatcher => return false,
                Some(_) => {}
                None => return false,
            }
        }
        true
    }

    /// Rewrite the admitted container arguments of a call
    /// [`Self::method_container_args_light`] admitted: a caller variable
    /// becomes its shared cell (promoted in place when it is not one yet).
    /// Runs while the CALLER's frame is current, before the method frame is
    /// pushed, so a promotion indexes the caller's locals.
    // Cost: O(p + a), p = parameters, a = arguments, plus one cell promotion
    // per promoted argument.
    pub(crate) fn promote_method_container_args(
        &mut self,
        method_def: &crate::runtime::MethodDef,
        args: &mut [Value],
    ) {
        let positional: Vec<usize> = args
            .iter()
            .enumerate()
            .filter(|(_, a)| !a.unwrap_varref().is_string_pair_value())
            .map(|(i, _)| i)
            .collect();
        let mut next = positional.into_iter();
        let arg_count = args.len();
        for pd in method_def.param_defs.iter() {
            if pd.is_invocant {
                continue;
            }
            let consumes = !pd.named && !pd.slurpy && !pd.double_slurpy;
            let idx = if consumes { next.next() } else { None };
            if !pd.binds_caller_container() {
                continue;
            }
            let Some(idx) = idx else { continue };
            match self.method_container_arg_bind(pd, idx, &args[idx], arg_count) {
                Some(MethodContainerBind::RelayHeld(slot)) => {
                    args[idx] = self.locals[slot as usize].clone();
                }
                Some(MethodContainerBind::Promote(slot)) => {
                    let Some(Some(name)) = self
                        .pending_call_arg_sources()
                        .and_then(|sources| sources.get(idx).cloned())
                    else {
                        continue;
                    };
                    let Some(code_addr) = (self.current_code != 0).then_some(self.current_code)
                    else {
                        continue;
                    };
                    // SAFETY: `current_code` is the address of the live
                    // bytecode frame's `CompiledCode` -- the caller running the
                    // method call -- which `vm_call_*` keeps alive for the whole
                    // frame; the promotion below does not return into it.
                    let code = unsafe { &*(code_addr as *const CompiledCode) };
                    let inner = crate::runtime::types::unwrap_varref_value(args[idx].clone());
                    let cell = self.capture_rw_arg_cell(code, &name, inner, Some(slot));
                    self.register_container_cell_constraint_for_name(&cell, &name);
                    args[idx] = cell;
                }
                Some(MethodContainerBind::Readonly) => {
                    args[idx] = crate::runtime::types::unwrap_varref_value(args[idx].clone());
                }
                Some(MethodContainerBind::Relay | MethodContainerBind::Proxy) | None => {}
            }
        }
    }
}
