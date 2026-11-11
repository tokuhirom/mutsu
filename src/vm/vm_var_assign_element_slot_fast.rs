use super::*;
use crate::binding_desc::DeclaredConstraint;
use crate::meta_ns::MetaNs;

impl Interpreter {
    /// Fast lane for a single-element store into an `Array` that a `$` scalar
    /// of the running chunk holds in its own slot: `$d[$i] = $v` and
    /// `$d[$i] := $x` / `:= &code` / `:= $d[$j]`.
    ///
    /// The `@a[$i] = $v` lane ([`Self::try_fast_array_element_assign_early`])
    /// reads its target from env, which is right for an `@` name; a `$`-held
    /// container's home is its slot (ADR-14632's `StorePlace::Slot`), so that
    /// lane declines it and every such store paid the whole generic path --
    /// ~2.5 us per `$d[$i] = 1` against ~0.2 us for `@d[$i] = 1`, and ~3.5 us
    /// per `$d[$i] := &f`. Terminal::ANSIParser builds its byte dispatch tables
    /// out of a few thousand of those binds per parser (#17429).
    ///
    /// The lane serves exactly what the generic path would do for the shape,
    /// and declines (touching nothing) for anything else:
    /// - a positional, non-negative `Int` subscript, no `=`-element share;
    /// - a plain user lexical `$` name the chunk declares untyped at
    ///   `target_slot`, with no sigilless alias, `:=` bind pair, `is default`
    ///   or readonly binding redirecting it, and whose env mirror (if any)
    ///   holds the same node;
    /// - the slot holds a plain mutable `Array`/`ItemArray` without embedded
    ///   type metadata (no element type, native backing or default);
    /// - an assignment's rvalue is a plain scalar value (the allow-list the `@`
    ///   lane uses: itemizing and `Proxy` fetching are the identity on it), and
    ///   the destination element is not a container the store writes through;
    /// - a bind's source container already exists: the bound value is itself
    ///   a `ContainerRef` (an element source, promoted by the compiler's
    ///   autovivify terminal), or its single source variable already holds
    ///   one. A first bind of a plain variable, which has to promote the
    ///   variable's value into a fresh cell and install it back, stays on the
    ///   generic path.
    // Cost: O(1) amortized (an in-place slot write, or a growth of the array
    // to the index, O(g) for g new slots).
    pub(crate) fn try_fast_slot_array_element_store(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        is_positional: bool,
        target_slot: Option<u32>,
    ) -> Option<Result<(), RuntimeError>> {
        if !is_positional
            || self.threads.shared_vars_active
            || crate::value::container_lock::multi_mutator_threads_live()
            || self.is_thread_clone()
        {
            return None;
        }
        let stack_len = self.stack.len();
        if stack_len < 2 {
            return None;
        }
        let ValueView::Int(idx_i) = self.stack[stack_len - 1].view() else {
            return None;
        };
        if idx_i < 0 {
            return None;
        }
        let i = idx_i as usize;
        let var_name = Self::const_str(code, name_idx);
        // A scalar is keyed without its sigil; `@`/`%`/`&`/`\` names, `$_`,
        // `self`, twigils and system names are all excluded here.
        if var_name.starts_with(['@', '%', '&', '\\', '$'])
            || !crate::env::is_plain_user_lexical(var_name)
        {
            return None;
        }
        let var_sym = code.const_sym(name_idx);
        let (items, kind, slot_home) = match target_slot {
            Some(slot) => {
                let slot = slot as usize;
                if slot >= self.locals.len()
                    || !code.declares_slot(slot)
                    || code.declared_constraint(slot) != DeclaredConstraint::Untyped
                    || code.locals.get(slot).map(String::as_str) != Some(var_name)
                {
                    return None;
                }
                match self.locals[slot].view() {
                    ValueView::Array(items, kind) => (items.clone(), kind, true),
                    _ => return None,
                }
            }
            // A pointy `for` parameter has no slot: the loop binds it by name,
            // and while that loop runs the env binding is the variable
            // (`for @tables -> $t { $t[$i] := ... }`). A compunit file-scope
            // lexical of the same name is reached through its cell instead,
            // so leave any name the unit-lexical store holds to the generic
            // path.
            None => {
                if !self.topic_state.name_bound_by_loop_param(var_name)
                    || !code.local_slots_of(var_sym).is_empty()
                    || !self.lexicals.unit_lexicals.is_empty()
                        && self.lexicals.unit_lexicals.contains_name(var_name)
                {
                    return None;
                }
                match self.env().get_sym(var_sym).map(Value::view) {
                    Some(ValueView::Array(items, kind)) => (items.clone(), kind, false),
                    _ => return None,
                }
            }
        };
        if !matches!(
            kind,
            crate::value::ArrayKind::Array | crate::value::ArrayKind::ItemArray
        ) || items.has_type_meta()
        {
            return None;
        }
        if self
            .env()
            .get_sym(MetaNs::SigillessAlias.key(var_sym))
            .is_some()
            || self.bind_pair_names_container(code, var_name)
            || self.var_default(var_name).is_some()
            || self.is_readonly_sym(var_sym)
        // No unit-lexical or `our` probe for a slot: the generic store
        // reaches a compunit's file-scope cell only through a slot holding
        // that cell (`unit_lexical_container_cell_at`), and the slot here
        // holds a bare Array; a `Slot` place resolves nothing by name.
        {
            return None;
        }
        // The env mirror of a slot, when there is one, must name the same
        // node, so the in-place write below is what a by-name reader sees too.
        if slot_home {
            match self.env().get_sym(var_sym).map(Value::view) {
                None => {}
                Some(ValueView::Array(env_items, _))
                    if crate::gc::Gc::ptr_eq(&items, &env_items) => {}
                Some(_) => return None,
            }
        }
        let rhs = &self.stack[stack_len - 2];
        // `(element to store, rvalue, whether the store writes the slot itself)`.
        let (element, result, plain_slot_store) = if Self::is_bind_index_value(rhs) {
            // Read the marker in place: `split_bind_index_marker` copies the
            // source names into a fresh `Vec<Option<String>>`, an allocation
            // per bind this lane exists to avoid.
            let (value, source) = Self::peek_bind_index_marker(rhs)?;
            if let ValueView::ContainerRef(cell) = value.view() {
                // An element (or otherwise already promoted) source: the
                // destination becomes that very container.
                (Value::container_ref(cell.clone()), value, false)
            } else if let Some(source) = source {
                // A variable source shares its container; only one that
                // already holds a cell is served here.
                let ValueView::Str(source) = source.view() else {
                    return None;
                };
                let source: &str = source.as_str();
                if source.contains('\0') {
                    return None;
                }
                match self.env().get(source).map(Value::view) {
                    Some(ValueView::ContainerRef(cell)) => {
                        (Value::container_ref(cell.clone()), value, false)
                    }
                    _ => return None,
                }
            } else if matches!(value.view(), ValueView::Sub(_) | ValueView::Instance { .. }) {
                // No container to share (`:= &code` of a `my &c := {...}`, a
                // routine, an object): the element holds the value itself.
                // Literals (which bind read-only) and aggregates (which get a
                // fresh cell) are the generic path's.
                (value.clone(), value, true)
            } else {
                return None;
            }
        } else {
            if !matches!(
                rhs.view(),
                ValueView::Int(_)
                    | ValueView::BigInt(_)
                    | ValueView::Num(_)
                    | ValueView::Str(_)
                    | ValueView::Bool(_)
                    | ValueView::Rat(..)
                    | ValueView::FatRat(..)
                    | ValueView::BigRat(..)
                    | ValueView::Complex(..)
                    | ValueView::Enum { .. }
                    | ValueView::Instance { .. }
                    | ValueView::Version { .. }
            ) {
                return None;
            }
            (rhs.clone(), rhs.clone(), true)
        };
        // An existing element the store would write THROUGH (a bound cell, a
        // Proxy, an itemized Scalar, a varref, a bound marker) is the generic
        // path's to handle -- the generic bind writes through one too, when it
        // has no container of its own to install.
        if plain_slot_store && let Some(existing) = items.items().get(i) {
            match existing.view() {
                ValueView::ContainerRef(_)
                | ValueView::Scalar(_)
                | ValueView::Proxy { .. }
                | ValueView::VarRef { .. } => return None,
                ValueView::Pair(name, _) if name.as_str().starts_with("__mutsu_bound") => {
                    return None;
                }
                _ => {}
            }
        }
        // ---- All checks passed; commit. ----
        self.stack.truncate(stack_len - 2);
        // SAFETY: audited aliased in-place container write (see
        // `value::aliased_mut`), the same one the generic store makes for a
        // shared node (`use_inplace`): every holder of the node observes the
        // write. `items` is a `Gc` handle cloned out of the slot's view and no
        // borrow into the node is live across the write; this lane runs on
        // the executing thread only (no second mutator thread, checked above).
        let data: &mut crate::value::ArrayData = unsafe { crate::value::gc_contents_mut(&items) };
        data.store_element(i, element);
        self.stack.push(Self::itemize_value(result));
        Some(Ok(()))
    }

    /// The bound value of a [`Self::bind_index_marker`] and its first
    /// source's name (a non-empty `Str`), read without copying: the store
    /// only ever consults the first source (`split_bind_index_marker`'s
    /// callers). `None` for a value that is no marker or whose payload is not
    /// the `[value, sources]` list the marker builds.
    // Cost: O(1).
    fn peek_bind_index_marker(val: &Value) -> Option<(Value, Option<Value>)> {
        let payload = match val.view() {
            ValueView::Pair(name, payload)
                if name == super::vm_var_assign_ops::BIND_INDEX_MARKER =>
            {
                payload.clone()
            }
            ValueView::ValuePair(name, payload)
                if name.as_str() == Some(super::vm_var_assign_ops::BIND_INDEX_MARKER) =>
            {
                payload.clone()
            }
            _ => return None,
        };
        let ValueView::Array(items, ..) = payload.view() else {
            return None;
        };
        let value = items.first().cloned().unwrap_or(Value::NIL);
        let source = match items.get(1).map(Value::view) {
            Some(ValueView::Array(sources, ..)) => sources
                .first()
                .filter(|src| matches!(src.view(), ValueView::Str(s) if !s.is_empty()))
                .cloned(),
            _ => None,
        };
        Some((value, source))
    }
}
