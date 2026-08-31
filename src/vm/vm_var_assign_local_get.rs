use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Load the live scalar container denoted by `take-rw $var`.
    ///
    /// Normal `GetLocal`/`GetGlobal` reads decontainerize cells, which is right
    /// for value context but loses the alias a gather must retain.  When the
    /// scalar has not yet been boxed, promote its authoritative store and make
    /// env agree so closure and dynamic reads see the same cell.
    pub(super) fn exec_get_scalar_container_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        local_idx: Option<u32>,
    ) {
        let name = Self::const_str(code, name_idx);
        // The loop topic is refreshed in env for every iteration.  Its compiled
        // local slot belongs to the enclosing gather body and can therefore be
        // a stale prior item; prefer the live topic binding.
        let current = if name == "_" {
            self.env()
                .get(name)
                .cloned()
                .or_else(|| local_idx.and_then(|idx| self.locals.get(idx as usize).cloned()))
        } else {
            local_idx
                .and_then(|idx| self.locals.get(idx as usize).cloned())
                .or_else(|| self.get_env_with_main_alias(name))
        }
        .unwrap_or(Value::NIL);
        let cell = if current.is_container_ref() {
            current
        } else {
            current.into_container_ref()
        };

        if let Some(idx) = local_idx {
            self.locals[idx as usize] = cell.clone();
            let sym = code.locals_sym.get(idx as usize).copied();
            self.set_env_with_main_alias_sym(name, sym, cell.clone());
        } else {
            self.set_env_with_main_alias(name, cell.clone());
        }
        self.stack.push(cell);
    }

    /// Like `exec_get_local_op` but does NOT resolve HashEntryRef.
    /// Pushes the raw local value, preserving container references for `=:=` checks.
    pub(super) fn exec_get_local_raw_op(&mut self, idx: u32) {
        let idx = idx as usize;
        let val = self.locals[idx].clone();
        self.stack.push(val);
    }

    /// Read a read-only scalar upvalue (see `OpCode::GetUpvalue`). The fast path
    /// reads this frame's installed upvalue array by index, dereferencing a
    /// shared `ContainerRef` cell so the value tracks the creator's container.
    /// When `index` is out of range — a non-standard path (control handler /
    /// phaser / register-reuse run) executed this closure's ops without installing
    /// its upvalue array — it falls back to a by-name env read; env is retained as
    /// the capture source, so the fallback is always correct for the plain scalar
    /// lexicals that are ever upvalue-promoted.
    pub(super) fn exec_get_upvalue_op(
        &mut self,
        code: &CompiledCode,
        index: u32,
        name_idx: u32,
        ip: &mut usize,
    ) -> Result<(), RuntimeError> {
        // An `our sub` declared in a bare block reads its captured block lexical
        // by-name with no upvalue array of its own; `self.upvalues` may still hold a
        // STALE entry from an unrelated prior closure call, so resolve such a capture
        // through its persisted shared cell FIRST (see `escaping_our_read`), ignoring
        // the upvalue slot entirely. Gated on a non-empty name set so ordinary
        // closures pay only an `is_empty` check on this hot path.
        let val = if !self.escaping_our_lexical_names.is_empty()
            && let Some(v) = self.escaping_our_read(Self::const_str(code, name_idx))
        {
            v
        } else {
            match self.upvalues.get(index as usize) {
                Some(Some(v)) => v.clone(),
                // `None` entry (non-cell capture) or out-of-range (non-standard
                // path): read the captured scalar live from env by name. A
                // method body has no upvalue array installed, so a free read of
                // an enclosing module's `our`/block lexical lands here — resolve
                // an env miss through the enclosing package chain
                // (`package_chain_var_fallback`), mirroring GetGlobal.
                _ => {
                    let name = Self::const_str(code, name_idx);
                    // NB: a module sub's free read of its own compunit's
                    // file-scope `my` resolves inside `get_env_with_main_alias`,
                    // which does not consult `env` for such a name (`unit_lexicals`).
                    self.get_env_with_main_alias(name)
                        .or_else(|| self.package_chain_var_fallback(name))
                        .unwrap_or(Value::NIL)
                }
            }
        };
        // Tag-probed first: a `view()` would materialize a lazy Match (see
        // `exec_get_local_op` below).
        let val = if val.is_lazy_thunk_value()
            && let ValueView::LazyThunk(thunk_data) = val.view()
        {
            let thunk_data = thunk_data.clone();
            self.force_lazy_thunk(&thunk_data)?
        } else {
            val
        };
        self.stack.push(val.into_deref());
        *ip += 1;
        Ok(())
    }

    pub(super) fn exec_get_local_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        self.exec_get_local_op_inner(code, idx, false)
    }

    /// `OpCode::GetLocalDeferred`: `GetLocal` that keeps a deferred
    /// `HashEntryRef` bind token unresolved (see the opcode's doc).
    pub(super) fn exec_get_local_deferred_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        self.exec_get_local_op_inner(code, idx, true)
    }

    fn exec_get_local_op_inner(
        &mut self,
        code: &CompiledCode,
        idx: u32,
        keep_deferred_entry: bool,
    ) -> Result<(), RuntimeError> {
        let idx = idx as usize;
        // Check if this variable has a binding alias (e.g. from $CALLER::foo := $other_var)
        // Borrow the name from the compiled code (it is independent of `self`),
        // avoiding a String clone on every GetLocal.
        let name: &str = code.locals.get(idx).map(|s| s.as_str()).unwrap_or("");
        if let Some(bound_to) = self.resolve_binding(name) {
            let bound_to = bound_to.to_string();
            if let Some(val) = self.env().get(&bound_to).cloned() {
                self.stack.push(val);
                return Ok(());
            }
        }
        // Attribute locals (!attr) modified by CAS: env holds the authoritative
        // value since sync_locals_from_env skips !-prefixed names for performance.
        if name.starts_with('!')
            && self.is_shared_var_dirty(name)
            && let Some(val) = self.env().get(name).cloned()
        {
            self.locals[idx] = val.clone();
            self.stack.push(val);
            return Ok(());
        }
        // Atomic-variable read: skip entirely (a `format!` plus two
        // `var_type_constraint` lookups) when no atomic storage has ever been
        // registered — the common case on this hot local-read path.
        if self.atomic_var_seen() {
            let atomic_name = name.strip_prefix('$').unwrap_or(name);
            let atomic_name_key = format!("__mutsu_atomic_name::{atomic_name}");
            // Only use the scalar atomic fast path for scalar ($) variables.
            // Array (@) variables with `atomicint` constraint are element-wise
            // atomic and should go through the normal array read path.
            let is_atomic_int = !name.starts_with('@')
                && (loan_env!(self, var_type_constraint(&name)).as_deref() == Some("atomicint")
                    || loan_env!(self, var_type_constraint(atomic_name)).as_deref()
                        == Some("atomicint")
                    || self.get_shared_var(&atomic_name_key).is_some());
            if is_atomic_int {
                let fetched = loan_env!(
                    self,
                    builtin_atomic_fetch_var(&[Value::str(atomic_name.to_string())])
                )?;
                self.locals[idx] = fetched.clone();
                self.stack.push(fetched);
                return Ok(());
            }
        }
        // Atomic array/hash CAS stores the authoritative container under an
        // internal shared key. Check it first so reads pick up the latest
        // CAS'd value (for `%`: a thread's own `%h{$k} = $v` lands in the
        // atomic entry, and the base-key snapshot below is stale).
        //
        // None of that applies to a name this lineage RE-DECLARED: the store's
        // entries under it describe the shadowed outer binding, so preferring
        // them would resurrect a foreign container over this frame's own `my
        // @a` (the write side is masked in `set_shared_var_sym`; this is its
        // read twin, mirroring the scalar gate further down).
        let container_redeclared = self.container_name_is_redeclared(name);
        if container_redeclared {
            // fall through to the local/env read
        } else if name.starts_with('@') {
            let atomic_key = format!("__mutsu_atomic_arr::{name}");
            if let Some(shared_val) = self.get_shared_var(&atomic_key) {
                self.locals[idx] = shared_val.clone();
                self.stack.push(shared_val);
                return Ok(());
            }
        } else if name.starts_with('%') {
            let atomic_key = format!("__mutsu_atomic_hash::{name}");
            if let Some(shared_val) = self.get_shared_var(&atomic_key) {
                self.locals[idx] = shared_val.clone();
                self.stack.push(shared_val);
                return Ok(());
            }
        }
        // Shared @/% variables may be mutated by sibling threads while this Interpreter
        // still holds an old local snapshot. Prefer the shared copy so reads
        // observe the latest value without forcing array COW on every push.
        if !container_redeclared
            && (name.starts_with('@') || name.starts_with('%'))
            && let Some(shared_val) = self.get_shared_var(name)
        {
            self.stack.push(shared_val);
            return Ok(());
        }
        // Lazy sync: if the local is not a ContainerRef but env has one
        // (e.g., a cross-scope `:=` binding was established during a function/method
        // call and propagated back to env but not to locals), adopt the ContainerRef.
        // Skip for type objects and complex values that should not be replaced.
        //
        // Overlay-only lookup (`overlay_get`/`overlay_get_sym`), NOT `get`/`get_sym`:
        // this frame's own local slot must never adopt a same-named ANCESTOR call
        // frame's container. `call_compiled_function_positional_light` (and the
        // other scoped-env call paths) chain the callee's env as a *scoped child*
        // of the live caller env for perf (no per-call flatten/clone); when a
        // recursive call's own by-name env mirror is skipped for this param
        // (`needs_env_sync` false — the common case for a plain scalar param only
        // ever read via its slot), a plain `get`/`get_sym` here falls through the
        // parent chain and can find the CALLER's own same-named variable instead —
        // e.g. a recursive `sub rec($n) { my @v = ($n,); ... rec($n - 1) ... }`
        // where the trailing-comma list literal boxes `$n`'s slot into a shared
        // `ContainerRef` (so `@v`'s element aliases `$n`'s container) and mirrors
        // it into env: the callee's fresh `$n = 0` binding got silently replaced
        // by the caller's own boxed `$n` cell (still holding `1`), which never
        // decremented — an Raku-level infinite recursion that overflowed the
        // native Rust stack (`todo/deep/recursive-sub-trailing-comma-array-
        // literal-of-own-param-stack-overflow.md`). `overlay_get`/`overlay_get_sym`
        // read only this frame's own overlay (still enough for the *intended*
        // same-call-frame propagation case in the comment above, since a plain
        // function/method body runs under a single env tier — nested blocks do
        // not push their own `scoped_child`), so an ancestor frame's container can
        // never be picked up here.
        if !self.locals[idx].is_container_ref()
            // A lazy Match counts as an Instance here — probed by tag so this
            // per-GetLocal check cannot materialize it.
            && !self.locals[idx].is_lazy_match_value()
            && !matches!(
                self.locals[idx].view(),
                ValueView::Package(_)
                    | ValueView::Array(..)
                    | ValueView::Hash(..)
                    | ValueView::Sub(..)
                    | ValueView::Instance { .. }
            )
            // Probe via the pre-interned Symbol (this read runs on every
            // GetLocal — a by-name lookup would re-intern per read).
            && let Some(env_hit) = code.locals_sym.get(idx).map_or_else(
                || self.env().overlay_get(name),
                |sym| self.env().overlay_get_sym(*sym),
            )
            && let Some(arc) = match env_hit.view() {
                ValueView::ContainerRef(arc) => Some(arc.clone()),
                _ => None,
            }
        {
            self.locals[idx] = Value::container_ref(arc);
        }
        // Phase 3 Stage 2 (scalar slice): scalar instance attributes read straight
        // from `self`'s shared cell, so a mutation made in a nested method frame
        // is visible here. Gated on a non-container slot so `$!x := outer`
        // bindings keep their ContainerRef handling. The cell lookup returns None
        // for non-attribute names and when `self` is not an instance.
        if !self.locals[idx].is_container_ref()
            // Slot form: the attribute `Symbol` is pre-resolved per chunk, so
            // this read parses no twigil and interns no string (ADR-0006 §2.4).
            && let Some(cell_val) = self.read_self_attr_cell_slot(code, idx)
        {
            self.locals[idx] = cell_val.clone();
            self.stack.push(cell_val);
            return Ok(());
        }
        // Method frames seed attribute locals from their declarations. On a
        // type-object invocant those defaults are metadata, not instance
        // storage: reading `$!attr` must fail before the seeded local can make
        // the type object look like a constructed instance.
        if name.starts_with('!')
            && name.len() > 1
            && !name.starts_with("__")
            && let Some(self_val) = self.get_env_with_main_alias("self")
            && !self_val.with_deref(|value| {
                matches!(
                    value.view(),
                    ValueView::Instance { .. } | ValueView::Mixin(..)
                )
            })
        {
            let class_name = match self_val.view() {
                ValueView::Package(name) => name.resolve(),
                _ => crate::value::what_type_name(&self_val),
            };
            return Err(RuntimeError::new(format!(
                "Cannot look up attributes in a {class_name} type object. Did you forget a '.new'?"
            )));
        }
        let val = self.locals[idx].clone();
        // Resolve a deferred bind token to its current value (Any if the path
        // doesn't exist). The raw local slot is unchanged, so a later write still
        // materializes it; `=:=` reads the raw slot via GetLocalRaw.
        if val.is_hash_entry_ref_value() {
            if keep_deferred_entry {
                self.stack.push(val);
            } else {
                self.stack.push(val.hash_entry_read());
            }
            return Ok(());
        }
        // Force lazy thunks transparently on access. Both this and the
        // HashEntryRef resolve above are tag-probed: they run on every
        // GetLocal, and a `view()` would materialize a lazy Match.
        if val.is_lazy_thunk_value()
            && let ValueView::LazyThunk(thunk_data) = val.view()
        {
            let thunk_data = thunk_data.clone();
            let forced = self.force_lazy_thunk(&thunk_data)?;
            self.stack.push(forced);
            return Ok(());
        }
        // Auto-deref ContainerRef: read the inner value for stack use (ContainerRef
        // axis of the decont family). Gate on is_container_ref() to preserve the
        // early return AND keep the non-container hot path move-only (into_deref is
        // never reached for non-ContainerRef values).
        if val.is_container_ref() {
            // A deferred array entry carried by a bound slice lives inside a
            // temporary cell until its first write. In value context unwrap it
            // one more step so callers see the array hole, while lvalue context
            // keeps the cell for `store_through_cell` to materialize.
            if !keep_deferred_entry {
                let inner = val.with_deref(|inner| inner.clone());
                if matches!(inner.view(), ValueView::HashEntryRef { .. }) {
                    self.stack.push(inner.hash_entry_read());
                    return Ok(());
                }
            }
            // In container mode, an EMPTY cell is a live link of the lvalue
            // chain, not a value: `my @a; my $x := @a[0]; my $y := $x<k>`
            // promoted the array hole to a cell holding `Any`, and dereferencing
            // it here hands the subscript a bare `Any` with no way back to the
            // storage — the eventual write went nowhere. Hand the cell itself to
            // the subscript so it can anchor a deferred path (`EntryRoot::Cell`).
            // A cell holding a real container still derefs: the container shares
            // its `Gc`, so the chain continues through it unchanged.
            if keep_deferred_entry && !crate::runtime::types::value_is_defined(&val) {
                self.stack.push(val);
                return Ok(());
            }
            self.stack.push(val.into_deref());
            return Ok(());
        }
        // Fast path: non-Nil values are always valid — skip env lookup
        if val.is_nil() {
            // The cross-thread shared store is keyed by BARE NAME and (within a
            // spawn lineage, ADR-0010) chains to ancestors, so its `depends`
            // entry may belong to an ancestor scope's lexical that some earlier
            // `start`/Proc::Async spawn migrated in (`clone_for_thread` seeds
            // every env var it can see). A
            // name this frame re-declared is a fresh binding that shadows it, and
            // its Nil is a real Nil — not a stale snapshot to refresh from the
            // shared store. `set_shared_var_sym` already masks the WRITE side on
            // exactly this set; without the same gate here the read resurrects the
            // foreign value (`my $x := f()` yielding Nil would see the other
            // scope's `$x`).
            if !self.thread_redeclared_vars.borrow().contains(name)
                && let Some(shared_val) = self.get_shared_var(name)
            {
                self.stack.push(shared_val);
                return Ok(());
            }
            let is_internal = name.starts_with("__");
            let is_special = matches!(name, "_" | "/" | "!" | "¢");
            // Private attribute locals (!attr) are populated directly from
            // instance attributes in fast-path method calls; they may not be
            // in the env (when skip_env_setup is active) but are still valid.
            let is_private_attr =
                name.starts_with('!') && name.len() > 1 && !name.starts_with("__");
            // Rakudo parity: a private-attribute read on a concrete invocant
            // whose class does not carry the attribute throws (P6opaque
            // no-such-attribute) instead of yielding Nil.
            if is_private_attr && let Some(err) = self.missing_private_attr_read_error(name) {
                return Err(err);
            }
            if !is_internal && !is_special && !is_private_attr && !self.env().contains_key(name) {
                return Err(RuntimeError::undeclared_variable(name));
            }
            // `is default(...)`: return the default value instead of Nil.
            if let Some(def) = self.var_default(name) {
                self.stack.push(def.clone());
                return Ok(());
            }
            // Deliberately the global-map-only fast probe: an env-scoped
            // constraint (`SetVarTypeScoped` / a typed param bind) must NOT
            // convert a Nil read into the type object here — a `Mu $b = Nil`
            // parameter's default really is Nil. A typed routine LEXICAL never
            // reads Nil in the first place: its declaration seeds the type
            // object and a Nil assignment resets to it in the SetLocal store
            // path (`typed_scalar_nil_seed_value`).
            if let Some(constraint) = self.var_type_constraint_fast(name).cloned() {
                let nominal = loan_env!(self, nominal_type_object_name_for_constraint(&constraint));
                // Nil type constraint: the type object for Nil is the Nil value
                // itself, not a "Nil" Package type object.
                if nominal == "Nil" {
                    self.stack.push(Value::NIL);
                } else {
                    self.stack.push(Value::package(Symbol::intern(&nominal)));
                }
                return Ok(());
            }
        }
        self.stack.push(val);
        Ok(())
    }

    /// Box the just-declared scalar local at `idx` into a shared `ContainerRef`
    /// cell placed in BOTH the slot and the env entry, so a directly-nested named
    /// sub that writes the lexical by name (via `SetGlobal` through env) and the
    /// owner that reads it by slot observe one cell — enabling cross-call
    /// accumulation without the `env_dirty` blanket reconcile. The skips mirror
    /// `box_captured_lexicals` exactly: scalars only (`@`/`%`/`&` share already),
    /// never an already-shared cell, never a reference/identity-bearing value,
    /// and never a name the legacy atomic lane currently owns. (The
    /// type/`where`-constraint refusal that used to be listed here was retired
    /// with ADR-0055 slice 1 — the constraint belongs to the container now, so
    /// a write reaching the scalar through its cell re-checks it.)
    pub(crate) fn box_decl_local_cell(&mut self, code: &CompiledCode, idx: usize) {
        let name = &code.locals[idx];
        if name.starts_with('&') {
            return;
        }
        // `@`/`%` containers captured-and-mutated in place by a nested named sub
        // (e.g. a user `trait_mod:<is>` pushing to an outer `@names`) are boxed as
        // a whole-container cell so the sub's by-name mutation and the owner's
        // by-name read alias one cell (docs/captured-outer-cell-sharing.md §7.2).
        if name.starts_with('@') || name.starts_with('%') {
            self.box_decl_local_container_cell(code, idx);
            return;
        }
        if self.locals[idx].is_container_ref() {
            return;
        }
        // Mirrors `box_captured_lexicals`: decline while the name-keyed legacy
        // atomic lane owns this binding's value (see `legacy_atomic_lane_owns`).
        if self.legacy_atomic_lane_owns(name.trim_start_matches('$')) {
            return;
        }
        let cur = self.locals[idx].clone();
        // The Any type object (uninitialized-scalar seed, PLAN 8.5 step 3) is
        // boxed like the old Nil seed; other reference/identity-bearing values
        // are skipped (mirrors `box_captured_lexicals`, including its
        // Seq/HyperSeq/RaceSeq/Slip exclusion --
        // `news/2026-08/atomic-cell-shape-refusal-asymmetry-resolved.md`).
        // ADR-0055 slice 1 (2026-08-28): `Package`, `Array` and `Hash` left this
        // list, and the type-constraint refusal below it went entirely -- an
        // unboxed captured-and-mutated lexical is precisely the residue the
        // vouch/cell dichotomy has to cover.
        if !cur.is_any_type_object()
            && matches!(
                cur.view(),
                ValueView::Sub(..)
                    | ValueView::Proxy { .. }
                    | ValueView::Seq(..)
                    | ValueView::HyperSeq(..)
                    | ValueView::RaceSeq(..)
                    | ValueView::Slip(..)
            )
        {
            return;
        }
        let container = cur.into_container_ref();
        self.locals[idx] = container.clone();
        let nm = code.locals[idx].clone();
        self.env_mut().insert(nm.clone(), container.clone());
        // Track C: keep a running thread's shared snapshot pointing at the cell
        // (mirrors box_captured_lexicals).
        if self.shared_vars_active {
            loan_env!(self, set_shared_var(&nm, container.clone()));
        }
    }

    /// Box a just-declared `@`/`%` container local into a shared `ContainerRef`
    /// cell placed in BOTH the slot and the env entry (the array/hash `:=` cell
    /// shape), so a nested named sub that mutates the container by name and the
    /// owner that reads it by name observe one cell. The mutating-method and
    /// element-assign write-back paths already descend through the cell
    /// (`try_native_array_mut` / `try_native_hash_mut_bound` / `env_root_descended_mut`),
    /// and `GetArrayVar`/`GetHashVar` `into_deref()` the cell on read.
    pub(crate) fn box_decl_local_container_cell(&mut self, code: &CompiledCode, idx: usize) {
        if self.locals[idx].is_container_ref() {
            return;
        }
        if !matches!(
            self.locals[idx].view(),
            ValueView::Array(..) | ValueView::Hash(..)
        ) {
            return;
        }
        let name = code.locals[idx].clone();
        // Typed containers must keep flowing through the assignment chokepoint.
        if loan_env!(self, var_type_constraint(&name)).is_some()
            || loan_env!(
                self,
                var_type_constraint(name.trim_start_matches(['@', '%']))
            )
            .is_some()
        {
            return;
        }
        let container = self.locals[idx].clone().into_container_ref();
        self.locals[idx] = container.clone();
        self.env_mut().insert(name.clone(), container.clone());
        if self.shared_vars_active {
            loan_env!(self, set_shared_var(&name, container.clone()));
        }
    }
}
