//! Sigilless (`\x`) positional parameters on the positional-light call path
//! (#17335, #17376).
//!
//! A sigilless parameter is implicitly raw: it binds the argument itself, so
//! for a caller variable it aliases that variable's container, and for a value
//! it is a read-only term. The general binder realizes the alias by name (an
//! env alias key plus an exit writeback); the light path instead reuses the
//! container transport ADR-0109 gave `is rw` parameters: the caller's variable
//! is promoted to a shared `ContainerRef` cell and the parameter's slot holds
//! that same cell, so every read and write goes through one container and no
//! writeback exists to forget. A value argument binds as a read-only term, with
//! the same `__mutsu_sigilless_readonly::` mark the general binder writes.
//!
//! Every argument shape this module cannot classify with certainty declines,
//! and the call keeps the general binder exactly as before.

use super::*;

/// How one sigilless parameter binds its argument on the light path.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum LightSigillessBind {
    /// A value with no container behind it (a literal, a call result,
    /// `self`, a type object named by its own name, a read-only source): the
    /// parameter is a read-only term holding the value.
    Readonly,
    /// The argument already is a shared cell (an element lvalue, or a caller
    /// variable an earlier light call promoted): bind that cell.
    Relay,
    /// A caller lexical whose slot already holds the shared cell an earlier
    /// promotion made: bind that cell (already named and constrained).
    RelayHeld(u32),
    /// A caller lexical still held as a plain value: promote its slot to a
    /// shared cell and bind the cell.
    Promote,
    /// The caller's env-held topic `$_` holding a plain, writable value: the
    /// parameter is that value itself (no box, so `v =:= v."name"()` holds,
    /// as the general binder's `bare_topic_value` rule keeps it), writable,
    /// and the call's exit writes a changed parameter back to the caller's
    /// `$_`, as the general binder's exit flush does through its
    /// `__mutsu_sigilless_alias::` chain (#19455).
    TopicValue,
}

/// What [`Interpreter::bind_light_sigilless_args`] bound: the writable
/// parameters, and among them the [`LightSigillessBind::TopicValue`] ones.
#[derive(Clone, Copy, Default)]
pub(super) struct LightSigillessBits {
    pub(super) writable: u64,
    pub(super) topic: u64,
}

impl Interpreter {
    /// Whether the sigilless parameter `pd` has a shape the positional-light
    /// bind can serve: positional, mandatory, no trait, no `where`, no
    /// sub-signature, and a non-native constraint the fast type check knows.
    /// Native constraints keep the general binder (it unboxes and wraps).
    // Cost: O(t + len(tc)), t = traits, tc = the constraint's spelling.
    pub(super) fn light_sigilless_param_ok(pd: &crate::ast::ParamDef) -> bool {
        pd.sigilless
            && !pd.named
            && !pd.slurpy
            && !pd.double_slurpy
            && !pd.onearg
            && !pd.is_invocant
            && !pd.optional_marker
            && pd.default.is_none()
            && pd.no_traits()
            && pd.where_constraint.is_none()
            && pd.sub_signature.is_none()
            && pd.outer_sub_signature.is_none()
            && pd.code_signature.is_none()
            && pd.literal_value.is_none()
            && pd.type_capture.is_none()
            && pd.type_constraint.as_deref().is_none_or(|tc| {
                !crate::runtime::native_types::is_native_int_type(tc)
                    && !matches!(tc, "str" | "num" | "num32" | "num64")
            })
    }

    /// The per-call half of the gate: every sigilless positional parameter's
    /// argument has a bind this path performs ([`Self::light_sigilless_bind`]),
    /// and a promotion has the caller's code to find the variable's slot in.
    // Cost: O(p), p = parameters; O(1) for a routine without a sigilless one.
    pub(super) fn positional_light_sigilless_args_admitted(
        &self,
        cf: &CompiledFunction,
        args: &[Value],
        caller_code: Option<&CompiledCode>,
    ) -> bool {
        if !cf.has_sigilless_positional_param {
            return true;
        }
        cf.param_defs.iter().enumerate().all(|(i, pd)| {
            if !pd.sigilless || pd.named {
                return true;
            }
            let Some(arg) = args.get(i) else {
                return true;
            };
            self.light_sigilless_bind(arg, caller_code).is_some()
        })
    }

    /// Classify one argument of a sigilless parameter the way the general
    /// binder would bind it (`binding_signature.rs`, the positional raw
    /// branch), or `None` for every shape whose binding needs that binder:
    /// a name-aliased sigilless source, a read-only binding cell, a
    /// multi-dimensional slice, a twigilled/dynamic/pseudo source, a
    /// variable that is not a local of `caller_code` at the slot its
    /// `WrapVarRef` site recorded (a promotion boxes exactly that slot; a
    /// captured outer variable has no slot to box), or a `:=`-aliased name.
    // Cost: O(1) env probes; O(n) for an Array argument, n = its elements.
    pub(super) fn light_sigilless_bind(
        &self,
        arg: &Value,
        caller_code: Option<&CompiledCode>,
    ) -> Option<LightSigillessBind> {
        match arg.view() {
            ValueView::ContainerRef(_) | ValueView::HashEntryRef { .. } => {
                Self::light_sigilless_cell_ok(arg).then_some(LightSigillessBind::Relay)
            }
            ValueView::VarRef {
                name, value, index, ..
            } => {
                if index.is_some() {
                    return None;
                }
                let source = name.as_str();
                // The frame's own fixtures (the invocant, `?CLASS`, `?ROLE`)
                // are values, never writable variables.
                if name.is_frame_fixture() {
                    return Some(LightSigillessBind::Readonly);
                }
                if source == "_" {
                    return self.light_sigilless_topic_bind(arg, value);
                }
                // The routine-reset magic, pseudo-packages, dynamics,
                // attributes, aggregates and qualified names each have their
                // own rule in the general binder.
                if source.is_empty()
                    || matches!(source, "!" | "/")
                    || source.starts_with(['?', '*', '@', '%', '&', '!', '.', '^', ':'])
                    || crate::qualified::is_qualified(name)
                {
                    return None;
                }
                let slot = arg.varref_slot()?;
                if slot == u32::MAX {
                    // A bareword type object: a value, not a variable.
                    return matches!(value.view(), ValueView::Package(_))
                        .then_some(LightSigillessBind::Readonly);
                }
                if let ValueView::Package(package) = value.view()
                    && Self::package_names_its_source(package.as_str(), source)
                {
                    return Some(LightSigillessBind::Readonly);
                }
                if Self::light_sigilless_array_is_slice(value) {
                    return None;
                }
                if self.name_is_readonly_binding_for(source, Some(name)) {
                    return Some(LightSigillessBind::Readonly);
                }
                // A name the general binder aliased by name (a caller's own
                // sigilless parameter, a `:=`-bound scalar): its root
                // container is elsewhere.
                if self.lexicals.sigilless_alias_seen
                    && self
                        .env()
                        .get_sym(Self::sigilless_alias_key_for_sym(name))
                        .is_some()
                {
                    return None;
                }
                if value.is_container_ref() {
                    return Self::light_sigilless_cell_ok(value)
                        .then_some(LightSigillessBind::Relay);
                }
                let code = caller_code?;
                if code.locals.get(slot as usize).map(String::as_str) != Some(source) {
                    return None;
                }
                let held = self.locals.get(slot as usize)?;
                if held.is_proxy_value() {
                    None
                } else if !held.is_container_ref() {
                    Some(LightSigillessBind::Promote)
                } else {
                    Self::light_sigilless_cell_ok(held)
                        .then_some(LightSigillessBind::RelayHeld(slot))
                }
            }
            _ => {
                if Self::light_sigilless_array_is_slice(arg) {
                    return None;
                }
                Some(LightSigillessBind::Readonly)
            }
        }
    }

    /// [`Self::light_sigilless_bind`] for a `$_` argument whose `WrapVarRef`
    /// carried `value`, or `None` for a shape the general binder keeps:
    ///
    /// * a topic held in a local slot (only an env-held topic, slot
    ///   `u32::MAX`, has the by-name writeback below);
    /// * a topic the general binder aliases by name already;
    /// * a Proxy or a hash-entry token;
    /// * a plain value while the live `$_` binding is a cell (the two would
    ///   disagree about what the parameter aliases).
    ///
    /// A cell is bound as that cell, as the general binder's shared-cell rule
    /// does. A plain value in a read-only `$_` binding is a read-only term; in
    /// a writable one it is [`LightSigillessBind::TopicValue`].
    // Cost: O(1) env probes, plus [`Self::light_sigilless_cell_ok`] for a cell.
    fn light_sigilless_topic_bind(&self, arg: &Value, value: &Value) -> Option<LightSigillessBind> {
        if arg.varref_slot()? != u32::MAX {
            return None;
        }
        let topic = crate::symbol::wk::topic();
        if self.lexicals.sigilless_alias_seen
            && self
                .env()
                .get_sym(Self::sigilless_alias_key_for_sym(topic))
                .is_some()
        {
            return None;
        }
        if value.is_proxy_value() || matches!(value.view(), ValueView::HashEntryRef { .. }) {
            return None;
        }
        if value.is_container_ref() {
            return Self::light_sigilless_cell_ok(value).then_some(LightSigillessBind::Relay);
        }
        if Self::light_sigilless_array_is_slice(value)
            || self
                .env()
                .get_sym(topic)
                .is_some_and(|live| live.is_container_ref())
        {
            return None;
        }
        Some(if self.name_is_readonly_binding_for("_", Some(topic)) {
            LightSigillessBind::Readonly
        } else {
            LightSigillessBind::TopicValue
        })
    }

    /// A cell that is not a read-only binding (`my $x := 5` holds one, and
    /// only the general binder reports writes through it correctly).
    // Cost: O(1) until a read-only binding cell exists; then O(c), c = cells
    // on the binding chain (bounded).
    fn light_sigilless_cell_ok(cell: &Value) -> bool {
        // No read-only binding cell was ever made: nothing to walk.
        !crate::value::readonly_binding_cells_possible()
            || !matches!(Self::binding_chain_readonly_kind(cell), Some(Some(_)))
    }

    /// An Array the general binder treats as a multi-dimensional slice
    /// lvalue (a list of element cells), which binds writable element-wise.
    // Cost: O(1) for a non-Array; O(n), n = elements, otherwise.
    fn light_sigilless_array_is_slice(value: &Value) -> bool {
        matches!(value.view(), ValueView::Array(..))
            && crate::runtime::types::is_multidim_slice_cells(value)
    }

    /// The general binder's `arg_is_source_type_object` test: the variable
    /// spelled `source` holds the type object named by that same spelling
    /// (`Foo` read as a term), which is a value rather than an lvalue.
    // Cost: O(len(package)).
    fn package_names_its_source(package: &str, source: &str) -> bool {
        package.strip_prefix(source).is_some_and(|suffix| {
            suffix.is_empty()
                || suffix
                    .chars()
                    .next()
                    .is_some_and(|ch| !ch.is_ascii_alphanumeric() && ch != '_')
        }) || crate::qualified::ends_with_segments(package, source)
    }

    /// Bind the sigilless arguments in `stack[args_base..]` in place, while
    /// the CALLER's frame is still current (a promotion indexes its locals).
    /// Returns the writable-parameter bit set for [`Self::mark_light_sigilless_param`],
    /// and the topic parameters [`Self::write_back_light_sigilless_topics`] serves.
    /// Admission ([`Self::positional_light_sigilless_args_admitted`]) already
    /// classified every argument, so a decline here is an internal error.
    // Cost: O(p), p = parameters, plus one cell promotion per promoted argument.
    pub(super) fn bind_light_sigilless_args(
        &mut self,
        cf: &CompiledFunction,
        args_base: usize,
        actual_count: usize,
        caller_code: Option<&CompiledCode>,
        func_name: &str,
    ) -> Result<LightSigillessBits, RuntimeError> {
        let mut writable = 0u64;
        let mut topic = 0u64;
        for (param_idx, pd) in cf.param_defs.iter().enumerate().take(actual_count) {
            if !pd.sigilless || pd.named {
                continue;
            }
            let pos = args_base + param_idx;
            let bind = self.light_sigilless_bind(&self.stack[pos], caller_code);
            match bind {
                Some(LightSigillessBind::Readonly) => {
                    let arg = std::mem::replace(&mut self.stack[pos], Value::NIL);
                    self.stack[pos] = crate::runtime::types::unwrap_varref_value(arg);
                }
                Some(LightSigillessBind::TopicValue) => {
                    let arg = std::mem::replace(&mut self.stack[pos], Value::NIL);
                    self.stack[pos] = crate::runtime::types::unwrap_varref_value(arg);
                    writable |= 1 << param_idx;
                    topic |= 1 << param_idx;
                }
                Some(LightSigillessBind::Relay) => {
                    let arg = std::mem::replace(&mut self.stack[pos], Value::NIL);
                    self.stack[pos] = crate::runtime::types::unwrap_varref_value(arg);
                    writable |= 1 << param_idx;
                }
                Some(LightSigillessBind::RelayHeld(slot)) => {
                    self.stack[pos] = self.locals[slot as usize].clone();
                    writable |= 1 << param_idx;
                }
                Some(LightSigillessBind::Promote) => {
                    let slot_hint = self.stack[pos].varref_slot();
                    let ValueView::VarRef { name, value, .. } = self.stack[pos].view() else {
                        unreachable!("a promotion is only classified for a VarRef");
                    };
                    let name = name.resolve();
                    let inner = (*value).clone();
                    let Some(caller_code) = caller_code else {
                        unreachable!("a promotion is only classified with the caller's code");
                    };
                    let cell = self.capture_rw_arg_cell(caller_code, &name, inner, slot_hint);
                    self.register_container_cell_constraint_for_name(&cell, &name);
                    self.stack[pos] = cell;
                    writable |= 1 << param_idx;
                }
                None => {
                    return Err(RuntimeError::new(format!(
                        "internal error: '{func_name}' reached the light bind with a \
                         sigilless argument its admission did not classify"
                    )));
                }
            }
        }
        Ok(LightSigillessBits { writable, topic })
    }

    /// The bound values of the `topic` parameters (bits as
    /// [`LightSigillessBits::topic`]), read from the callee frame at its
    /// exit, paired with what they were bound to: `(bound, final)`.
    // Cost: O(p * l) over the topic parameters only, p = parameters, l =
    // the callee's local slots; nothing for a call without one.
    pub(super) fn light_sigilless_topic_finals(
        &self,
        cf: &CompiledFunction,
        topic: u64,
        bound: &[Value],
    ) -> Vec<(Value, Value)> {
        let mut out = Vec::new();
        for (param_idx, pd) in cf.param_defs.iter().enumerate() {
            if topic & (1 << param_idx) == 0 {
                continue;
            }
            let storage = pd.storage_name();
            let Some(slot) = cf.code.locals.iter().position(|n| *n == storage) else {
                continue;
            };
            if let (Some(orig), Some(now)) = (bound.get(param_idx), self.locals.get(slot)) {
                out.push((orig.clone(), now.clone()));
            }
        }
        out
    }

    /// The exit half of [`LightSigillessBind::TopicValue`]: with the caller's
    /// env current again, a parameter that no longer holds the value it was
    /// bound to is written to the caller's `$_`, as the general binder's exit
    /// flush writes the final slot value through the parameter's alias.
    // Cost: O(t) env writes, t = changed topic parameters.
    pub(super) fn write_back_light_sigilless_topics(&mut self, finals: Vec<(Value, Value)>) {
        for (orig, now) in finals {
            let now = if now.is_container_ref() {
                now.deref_container()
            } else {
                now
            };
            if crate::value::identity::values_identical(&orig, &now) {
                continue;
            }
            self.set_env_with_main_alias("_", now);
        }
    }

    /// Record a light-bound sigilless parameter's mutability in the callee
    /// frame's env, as the general binder does: a read-only term gets the
    /// `__mutsu_sigilless_readonly::` mark, a writable one shadows any mark or
    /// by-name alias of the same name an outer frame left visible.
    // Cost: O(1) env writes.
    pub(super) fn mark_light_sigilless_param(&mut self, storage_sym: Symbol, writable: bool) {
        if self.lexicals.sigilless_alias_seen {
            self.env_mut()
                .remove_sym(Self::sigilless_alias_key_for_sym(storage_sym));
        }
        if !writable {
            self.lexicals.sigilless_alias_seen = true;
            let readonly_key = Self::sigilless_readonly_key_for_sym(storage_sym);
            self.env_mut().insert_sym_noting(readonly_key, Value::TRUE);
        } else if crate::env::sigilless_readonly_key_possible_for(storage_sym) {
            let readonly_key = Self::sigilless_readonly_key_for_sym(storage_sym);
            self.env_mut().insert_sym_noting(readonly_key, Value::FALSE);
        }
    }
}
