use super::*;

impl Interpreter {
    /// Enter `cf`'s compilation unit: record it as the unit currently
    /// executing and hand back the caller's, which every exit path restores.
    ///
    /// This is what makes user-declared operator scoping lexical. `?FILE` is
    /// NOT usable for the same purpose: the env entry tracks the unit being
    /// *loaded*, so inside a module routine called at runtime it still names
    /// the main script. `EVAL` is the exception — it sets both, because an
    /// EVAL unit exists only at runtime (see `Interpreter::user_infix_override`
    /// and `runtime::note_eval_unit_parent`).
    ///
    /// `None` (an AOT-compiled body) means the main script.
    #[inline]
    pub(super) fn enter_compilation_unit(&mut self, cf: &CompiledFunction) -> Symbol {
        // `CompiledFunction::source_file_sym` interns the declaring path ONCE
        // per routine. Going through the `&str` form re-hashed the whole path
        // (often 60+ bytes) on every named call (#7736).
        let unit = self.unit_of_source_sym(cf.source_file_sym());
        std::mem::replace(&mut self.current_unit, unit)
    }

    /// The compilation-unit key for a routine/closure's recorded source file.
    /// `None` (AOT-compiled) and `Some(program_path)` (compiled on the fly from
    /// the running script) are the same unit: the main script.
    #[inline]
    pub(crate) fn unit_of_source(&self, source_file: Option<&str>) -> Symbol {
        match (source_file, self.program_path.as_deref()) {
            (None, _) => crate::runtime::main_unit(),
            (Some(file), Some(prog)) if file == prog => crate::runtime::main_unit(),
            (Some(file), _) => Symbol::intern(file),
        }
    }

    /// Snapshot the lexical pragma state (`use fatal`, `use strict`,
    /// `use newline`, `use MONKEY-TYPING`) before entering a function body.
    /// Must be paired with `restore_pragma_state` on every exit path to prevent
    /// the callee's pragmas from leaking into the caller's scope.
    ///
    /// In real Raku, pragmas are compile-time and lexically scoped per
    /// compilation unit.  mutsu approximates this at runtime: save on entry,
    /// restore on exit, so `use fatal` inside a sub never outlives that sub.
    #[inline]
    pub(super) fn save_pragma_state(
        &self,
    ) -> (bool, bool, crate::runtime::NewlineMode, bool, bool) {
        (
            self.fatal_mode,
            self.strict_mode,
            self.newline_mode,
            self.monkey_typing,
            self.lexical_fatal_mode,
        )
    }

    /// Restore pragma state saved by `save_pragma_state`.
    #[inline]
    pub(super) fn restore_pragma_state(
        &mut self,
        state: (bool, bool, crate::runtime::NewlineMode, bool, bool),
    ) {
        self.fatal_mode = state.0;
        self.strict_mode = state.1;
        self.newline_mode = state.2;
        self.monkey_typing = state.3;
        self.lexical_fatal_mode = state.4;
    }

    /// Enforce a `ContainerRef` cell's registered `of`-type constraint before a
    /// write-through (`$ref = v` on a `:=`-bound typed slot — a typed rw
    /// attribute accessor bind, or a `my T $` anonymous typed scalar). Mirrors
    /// the Pair.value enforcement in `methods_mut_method_lvalue.rs`.
    pub(crate) fn check_container_cell_constraint(
        &mut self,
        cell: &crate::gc::Gc<crate::value::ContainerCell>,
        val: &Value,
    ) -> Result<(), RuntimeError> {
        // An element bound to a bare value (`%h.BIND-KEY($k, 42)`) has no
        // container to assign into.
        if cell.is_readonly() {
            return Err(RuntimeError::immutable_value());
        }
        if let Some(c) = crate::value::lookup_cell_constraint(cell)
            && !matches!(c.ty.as_str(), "Any" | "Mu")
            && !val.is_nil()
            && !self.type_matches_value(&c.ty, val)
        {
            // An ELEMENT's cell blames the container, exactly as a direct
            // `@a[0] = v` store does ("Type check failed for an element of
            // @a"); a plain typed scalar's cell keeps the assignment wording.
            return Err(match c.element_of {
                Some(owner) => self.type_check_element_failure(&owner, &c.ty, val),
                // `assign_to` is the name the cell was promoted from, so a write
                // arriving through an alias or from another frame still reads
                // "in assignment to $a" like rakudo's descriptor-carried wording.
                None => self.typecheck_assignment_failure(&c.ty, val, c.assign_to.as_deref()),
            });
        }
        Ok(())
    }

    /// Carry a declared scalar `of` constraint onto a cell created while
    /// promoting that scalar for a closure or named-sub capture. Once a scalar
    /// is boxed, the cell is the authoritative write target and name metadata
    /// is not enough to protect writes from a different frame.
    pub(crate) fn register_container_cell_constraint_for_name(
        &mut self,
        value: &Value,
        name: &str,
    ) {
        let ValueView::ContainerRef(cell) = value.view() else {
            return;
        };
        let constraint = self
            .var_type_constraint(name)
            .or_else(|| self.var_type_constraint(name.trim_start_matches('$')));
        if let Some(constraint) = constraint {
            let display = if name.starts_with(['$', '@', '%', '&']) {
                name.to_string()
            } else {
                format!("${name}")
            };
            crate::value::register_container_constraint_named(&cell, &constraint, &display);
        }
        self.register_container_cell_default_for_name(&cell, name);
    }

    /// The `is default(...)` of the container `slot` aliases, when `slot`
    /// holds a shared cell that carries one.
    // Cost: O(1).
    pub(crate) fn container_cell_default(slot: &Value) -> Option<Value> {
        match slot.view() {
            ValueView::ContainerRef(cell) => cell.default_value(),
            _ => None,
        }
    }

    /// What a `Nil` assigned through the sigilless alias in slot `idx` decays
    /// to, or `None` when the slot is not such an alias.
    ///
    /// A sigilless name (`sub t(\p)`, `my \g := $f`) IS the container it was
    /// bound to, but mutsu reaches that container through the by-name
    /// `__mutsu_sigilless_alias::` chain rather than a shared cell, so the
    /// alias's own name carries none of the container's metadata. The decay
    /// must follow the chain to its root and use the ROOT's `is default`, its
    /// type object, or `Any` -- exactly what a direct `$root = Nil` stores
    /// (#11110; the cell-based aliases got the same rule in #9831).
    ///
    /// A `:D`-constrained root is left alone: its Nil assignment is an error,
    /// which the root's own store reports.
    // Cost: O(h) expected, h = alias-chain length (one env probe per hop).
    pub(crate) fn sigilless_alias_nil_decay(
        &mut self,
        code: &CompiledCode,
        idx: usize,
    ) -> Option<Value> {
        if !crate::env::closure_meta_keys_possible() {
            return None;
        }
        let first = code
            .alias_sym(idx)
            .and_then(|sym| self.env().get_sym(sym))
            .and_then(Self::alias_target_name)?;
        self.sigilless_alias_root_nil_decay(first)
    }

    /// [`Self::sigilless_alias_nil_decay`] for a store that reaches the alias
    /// by name (a closure writing a captured sigilless name via `SetGlobal`).
    // Cost: O(h) expected, h = alias-chain length.
    pub(crate) fn sigilless_alias_nil_decay_by_name(&mut self, name: &str) -> Option<Value> {
        if !crate::env::closure_meta_keys_possible() {
            return None;
        }
        let first = self
            .env()
            .get_sym(runtime::sigilless_alias_key(name))
            .and_then(Self::alias_target_name)?;
        self.sigilless_alias_root_nil_decay(first)
    }

    // Cost: O(1).
    fn alias_target_name(v: &Value) -> Option<String> {
        match v.view() {
            ValueView::Str(name) => Some(name.to_string()),
            _ => None,
        }
    }

    /// Follow the sigilless alias chain from `first` to its root and decay a
    /// `Nil` against the root's container (see
    /// [`Self::sigilless_alias_nil_decay`]).
    // Cost: O(h) expected, h = alias-chain length.
    fn sigilless_alias_root_nil_decay(&mut self, first: String) -> Option<Value> {
        let mut root = first;
        let mut seen = std::collections::HashSet::new();
        while seen.insert(root.clone()) {
            match self
                .env()
                .get_sym(runtime::sigilless_alias_key(&root))
                .and_then(Self::alias_target_name)
            {
                Some(next) => root = next,
                None => break,
            }
        }
        // A `:=` to an element (`my $r := @a[0]`) records an internal temp as
        // its alias root, not a variable: the element's cell is the container
        // there, and carries its own default (#9831).
        if root.starts_with(['@', '%', '&']) || root.contains("__mutsu") {
            return None;
        }
        if let Some(def) = self.var_default(&root) {
            return Some(def.clone());
        }
        match loan_env!(self, var_type_constraint(&root)) {
            Some(constraint) if self.is_definite_constraint(&constraint) => None,
            Some(constraint) if constraint == "Nil" => None,
            Some(constraint) => Some(self.typed_scalar_nil_seed_value(&root, &constraint)),
            None => Some(self.reset_nil_untyped_scalar(&root, Value::NIL)),
        }
    }

    /// Carry a declared scalar's `is default(...)` onto the cell it was just
    /// promoted to, for the same reason the `of`-type travels
    /// ([`Self::register_container_cell_constraint_for_name`]): once the
    /// scalar is boxed, a `Nil` stored through an alias of it (an `is rw`
    /// parameter, a `for` alias, a `:=` binding) must decay to the
    /// container's default, which no alias name carries (#9831).
    // Cost: O(1) expected (at most two env probes, none when no lexical
    // `is default` was ever declared).
    pub(crate) fn register_container_cell_default_for_name(
        &self,
        cell: &crate::gc::Gc<crate::value::ContainerCell>,
        name: &str,
    ) {
        if !self.has_var_defaults() {
            return;
        }
        let def = self
            .var_default(name)
            .or_else(|| self.var_default(name.trim_start_matches('$')));
        if let Some(def) = def {
            cell.set_default(def.clone());
        }
    }

    /// Materialize a deferred vivification token's terminal slot into a fresh
    /// shared `ContainerCell` holding `val`, and install it at the slot.
    ///
    /// The cell carries the container's element constraint (ADR-0036 slice 4)
    /// and this first write is checked against it, so a `:=` bind to a slot
    /// that did not exist yet (`my Str @a; my $r := @a[5]; $r = 42`) is
    /// refused exactly like the in-range bind and the direct store are. The
    /// terminal is only written when the check passes.
    pub(crate) fn materialize_entry_cell(
        &mut self,
        terminal: &crate::value::EntryTerminal,
        val: Value,
    ) -> Result<crate::gc::Gc<crate::value::ContainerCell>, RuntimeError> {
        let cell = crate::gc::Gc::new(crate::value::ContainerCell::new(val.clone()));
        if let Some((ty, owner)) = terminal.element_constraint() {
            crate::value::register_element_constraint(&cell, &ty, owner);
            self.check_container_cell_constraint(&cell, &val)?;
        }
        terminal.insert(Value::container_ref(cell.clone()));
        Ok(cell)
    }

    /// Enforce a NAME-keyed scalar's registered `var_type_constraint` before a
    /// write reaches it through the `__mutsu_sigilless_alias::` forward chain
    /// (a sigilless `\x := $a` bind, a sigilless routine parameter aliasing a
    /// caller variable, or a `for LIST -> \x, $value {}` loop-param bind).
    ///
    /// A DIRECT write to a typed scalar's own slot is already checked at its
    /// `SetLocal`/expression-assignment chokepoint (which consults
    /// `var_type_constraint(name)` for that variable's OWN name). But when the
    /// write instead reaches the typed variable *through* a sigilless alias —
    /// `x`'s own name carries no constraint, so that chokepoint sees nothing —
    /// the forward chain walk mirrors the raw value into the alias target's
    /// storage with no check at all
    /// (see `todo/deep/sigilless-alias-assignment-skips-type-constraint.md`).
    /// This closes that gap by re-running the SAME name-keyed constraint
    /// lookup against the alias TARGET's name at the point the value is
    /// mirrored into its storage, so it fires uniformly regardless of which of
    /// mutsu's several alias-propagation call sites performs the write.
    ///
    /// Cheap on the untyped-variable common case: `var_type_constraint` is a
    /// plain name-keyed map lookup, and every caller of this function is
    /// already gated behind a "has any sigilless alias ever been created"
    /// fast-path check, so a program with no sigilless binds pays nothing.
    pub(super) fn check_sigilless_alias_target_constraint(
        &mut self,
        target_name: &str,
        val: &Value,
    ) -> Result<(), RuntimeError> {
        if target_name.starts_with(['@', '%', '&']) {
            return Ok(());
        }
        let Some(constraint) = loan_env!(self, var_type_constraint(target_name)) else {
            return Ok(());
        };
        if matches!(constraint.as_str(), "Any" | "Mu") {
            return Ok(());
        }
        // A bound alias may itself hold a `ContainerRef` cell (a further bind
        // layered on top); the constraint applies to the CONTAINED value.
        let check_val = val.deref_container();
        if check_val.is_nil() {
            return Ok(());
        }
        if !self.type_matches_value(&constraint, &check_val) {
            return Err(self.type_check_assignment_failure(target_name, &constraint, &check_val));
        }
        Ok(())
    }

    /// Slice 2a: clear the aggregate held inside a shared `ContainerRef` cell
    /// (`undefine @ary` where `my $r = @ary` promoted `@ary` to a cell). Uses
    /// `Arc::make_mut` so a copy taken out of the cell (`my @copy = @ary`) is
    /// detached rather than emptied; every alias of the cell observes the clear.
    pub(super) fn clear_aggregate_cell(cell: &crate::gc::Gc<crate::value::ContainerCell>) {
        let mut guard = cell.lock().unwrap();
        if (*guard)
            .with_array_mut(|arc, _| crate::gc::Gc::make_mut(arc).items_mut().clear())
            .is_none()
            && (*guard)
                .with_hash_mut(|arc| crate::gc::Gc::make_mut(arc).map.clear())
                .is_none()
        {
            *guard = Value::NIL;
        }
    }

    /// Build a Backtrace Value from a pre-formatted backtrace string.
    /// Parses the string lines to extract frame info (best-effort).
    /// `is_runtime` stamps the result — see
    /// [`Self::build_backtrace_value_with_runtime`].
    pub(crate) fn backtrace_value_from_string_with_runtime(
        bt_str: &str,
        is_runtime: bool,
    ) -> Value {
        use crate::symbol::Symbol;
        use std::collections::HashMap;

        let mut frames = Vec::new();
        for line in bt_str.lines() {
            let trimmed = line.trim();
            // Parse lines like "  in sub foo at file.raku line 5"
            // or "  in block <unit> at -e line 1"
            let subname;
            let rest;
            if let Some(after_sub) = trimmed.strip_prefix("in sub ") {
                if let Some(at_pos) = after_sub.find(" at ") {
                    subname = after_sub[..at_pos].to_string();
                    rest = &after_sub[at_pos..];
                } else {
                    subname = after_sub.to_string();
                    rest = "";
                }
            } else if let Some(after_block) = trimmed.strip_prefix("in block ") {
                if let Some(at_pos) = after_block.find(" at ") {
                    subname = after_block[..at_pos].to_string();
                    rest = &after_block[at_pos..];
                } else {
                    subname = after_block.to_string();
                    rest = "";
                }
            } else {
                continue;
            }

            let mut file = String::new();
            let mut line_no: i64 = 0;
            if let Some(at_rest) = rest.strip_prefix(" at ") {
                if let Some(line_pos) = at_rest.rfind(" line ") {
                    file = at_rest[..line_pos].to_string();
                    if let Ok(n) = at_rest[line_pos + 6..].parse::<i64>() {
                        line_no = n;
                    }
                } else {
                    file = at_rest.to_string();
                }
            }

            let mut frame_attrs = HashMap::new();
            frame_attrs.insert("subname".to_string(), Value::str(subname));
            frame_attrs.insert("file".to_string(), Value::str(file));
            frame_attrs.insert("line".to_string(), Value::int(line_no));
            frames.push(Value::make_instance(
                Symbol::intern("Backtrace::Frame"),
                frame_attrs,
            ));
        }

        let mut bt_attrs = HashMap::new();
        bt_attrs.insert("frames".to_string(), Value::array(frames));
        bt_attrs.insert("text".to_string(), Value::str(bt_str.to_string()));
        // Parsed from a captured backtrace string -- runtime unless the caller
        // says otherwise (a compile-time diagnosis; see the sibling builder).
        bt_attrs.insert("is-runtime".to_string(), Value::truth(is_runtime));
        Value::make_instance(Symbol::intern("Backtrace"), bt_attrs)
    }

    /// Attach the current call-stack backtrace (string form on the error, and
    /// structured `Backtrace` + line/file attributes on the exception instance
    /// if any) to a runtime error that does not carry one yet. `die`/`fail`
    /// build theirs at the throw site; this generalizes the same information to
    /// every other runtime error (method-not-found, type-check, ...) so CLI
    /// output and `$!.backtrace` report the failing line for all of them.
    // Cost: O(1) amortized; the backtraces render only when read (see
    // `attach_lazy_backtrace_to_error`).
    pub(super) fn attach_backtrace_to_error(&self, err: &mut RuntimeError) {
        self.attach_lazy_backtrace_to_error(err, &[]);
    }
}
