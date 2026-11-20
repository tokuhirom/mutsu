//! The env-free lane for a `$` scalar's `:=` declaration (`my $x := EXPR`),
//! split from `vm_decl_lane.rs`, whose slot predicate it shares
//! ([`Interpreter::env_free_bind_decl_slot`]). ADR-18244 stage B (#18274).
use super::*;

impl Interpreter {
    /// The `SetLocal` store of a `$` scalar's `:=` declaration
    /// (`my $x := EXPR`) whose slot is its variable's only home and whose
    /// value has no container of its own (an `Int`, `Str`, object or code
    /// value) or is a Proxy.
    ///
    /// Such a bind makes the name readonly (rakudo: "Cannot assign to an
    /// immutable value"), and that is all it does. The general store reaches the same verdict through its
    /// cascade -- the Proxy/Seq/decont/alias/array-share arms, the env
    /// mirrors, a by-name probe for other bindings of the name -- every one
    /// of which is inert for this shape on an env-free slot. What it leaves
    /// behind is reproduced here exactly: the name's readonly mark in the
    /// registry and the value seated in a binding cell that carries the kind
    /// (`readonly_value_binding`, ADR-11142), plus the declaration's own
    /// clearing steps (`exec_set_local_decl_fast`'s).
    ///
    /// Declines (`false`, nothing consumed) for anything else: a variable
    /// source (`my $x := $y`, which arrives as a `VarRef`), a container,
    /// Proxy or aggregate to alias, a list-like value, `Nil`, a type object,
    /// a typed declaration, or any pending store flavour beyond the
    /// declaration's.
    // Cost: O(1).
    pub(super) fn exec_set_local_bind_decl_fast(&mut self, code: &CompiledCode, idx: u32) -> bool {
        let idx = idx as usize;
        if !self.mark_ctx.only_scalar_bind_declaration_marks() || self.array_share_source_pending()
        {
            return false;
        }
        let Some(name) = code.locals.get(idx) else {
            return false;
        };
        let Some(desc) = code.simple_scalar_local_desc(idx) else {
            return false;
        };
        if code.declared_constraint(idx) != crate::binding_desc::DeclaredConstraint::Untyped
            || self.env_free_bind_decl_slot(code, name, idx).is_none()
            || !self.set_local_scalar_metadata_lanes_clear(code, idx, desc)
        {
            return false;
        }
        let Some(v) = self.stack.last() else {
            return false;
        };
        // The value shapes the general store binds as the value itself and
        // seats in a readonly binding cell: no container to alias, nothing
        // list-like a cell would itemize, and not `Nil` or a lazy thunk,
        // which take their own arms.
        // A type object is left to the general store too: its assignment
        // error names the type by reading the binding back through the env
        // (`type_object_assign_error`), so that bind keeps its env mirror.
        //
        // A Proxy is bound as itself: the name becomes the Proxy, writable
        // through its STORE, with no readonly mark and no binding cell, and
        // the general store's one Proxy step is
        // `update_proxy_closure_envs` (below).
        let is_proxy = matches!(v.view(), ValueView::Proxy { .. });
        let kind = if is_proxy {
            None
        } else if v.is_nil()
            || v.is_lazy_thunk_value()
            || !Self::binding_cell_keeps_value_semantics(v)
            || Self::bind_source_is_type_object(v)
            || !Self::bind_source_has_no_container(v)
        {
            return false;
        } else {
            Some(crate::ast::ReadonlyKind::Immutable)
        };
        // An object that decomposes on array assignment (`does Iterable`)
        // binds as a non-container: the general store records the decont
        // marker that makes `@a = $bound` iterate it.
        if matches!(v.view(), ValueView::Instance { .. }) {
            let v = v.clone();
            if self.instance_decomposes_on_array_assign(&v) {
                return false;
            }
        }
        if !self.locals[idx].is_plain_scalar_store_slot() {
            return false;
        }
        // -- committed: from here the store cannot fall back --
        let v = self.stack.pop().unwrap_or(Value::NIL);
        self.mark_ctx.consume_for_store();
        // The declaration steps the general store runs for every `my`.
        self.clear_var_default(name);
        let sym = code.locals_sym.get(idx).copied();
        if !self.no_readonly_vars()
            && let Some(sym) = sym
        {
            self.unmark_readonly_sym(sym);
        }
        if !self.lexicals.constant_var_names_seen.is_empty()
            && self
                .lexicals
                .constant_var_names_seen
                .contains(name.as_str())
        {
            self.clear_constant_marker(name);
        }
        self.clear_bind_decl_name_keys(code, idx);
        // A cell or Proxy an earlier binding of the name left in the visible
        // env must not outlive this fresh binding: a later `$x = v` lazy-syncs
        // a Proxy from env and would run ITS STORE (#9244). The general
        // declaration store replaces it the same way; a name that was never
        // an env key cannot have one.
        if let Some(sym) = sym
            && crate::symbol::maybe_env_key(sym)
            && matches!(
                self.env().get_sym(sym).map(Value::view),
                Some(ValueView::ContainerRef(_) | ValueView::Proxy { .. })
            )
        {
            if self.lexicals.our_scalar_cell_names.contains(name) {
                self.env_mut().remove_sym(sym);
            } else {
                self.env_mut().insert_sym(sym, Value::NIL);
            }
        }
        // A bind to a value that is not a container clears a stale
        // decontainerize marker of the name (`update_bound_decont_marker`),
        // probed first so an absent key costs no env copy.
        if self.bound_decont_active().get()
            && let Some(sym) = sym
        {
            let key = crate::meta_ns::MetaNs::BoundDecont.key(sym);
            if self.env().contains_key_sym(key) {
                self.env_mut().remove_sym(key);
            }
        }
        // The bind's own verdict, as the general store applies it.
        let Some(kind) = kind else {
            self.locals[idx] = Self::update_proxy_closure_envs(v, name);
            return true;
        };
        match sym {
            Some(sym) => self.mark_readonly_sym_with(sym, kind),
            None => self.mark_readonly_with(name, kind),
        }
        self.locals[idx] = Self::readonly_value_binding(v, kind);
        true
    }

    /// Clear the name-keyed binding metadata a fresh `my $x := ...` drops,
    /// as the general declaration store does: a sigilless-readonly marker
    /// and a forward `:=` alias key of the name visible from this frame
    /// describe an earlier binding, not this one.
    // Cost: O(d) env probe per key, d = env chain depth; O(1) when no key of
    // the name ever existed.
    fn clear_bind_decl_name_keys(&mut self, code: &CompiledCode, idx: usize) {
        if crate::env::sigilless_readonly_keys_possible()
            && let Some(sym) = code.readonly_sym(idx)
            && crate::symbol::maybe_env_key(sym)
            && self.env().contains_key_sym(sym)
        {
            self.env_mut().remove_sym(sym);
        }
        self.clear_lean_term_alias(code, idx);
    }
}
