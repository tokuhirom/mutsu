//! Bare-name resolution for a package's `our`-declared variables.
//!
//! `our @arr` inside `unit module UFL3` is a PACKAGE variable. Its canonical
//! storage is the package-qualified mirror `@UFL3::arr`: the declaration
//! compiles to `Dup; SetLocalDecl(slot); SetGlobal("@UFL3::arr")`, and the
//! `SetGlobal` arm mirrors every package-qualified write into `our_vars`
//! (`vm_exec_dispatch.rs`).
//!
//! The module's OWN routines, however, reference it by the BARE name. A sub
//! body compiles in a fresh `Compiler` whose `current_package` was overwritten
//! with the mangled state-scope name `UFL3::&arr-push/1`
//! (`compiler/helpers_sub_body.rs`), and `qualify_variable_name`
//! (`compiler/mod.rs`) returns the name verbatim for any package containing
//! `::&`. So `Expr::ArrayVar` emits a bare `GetArrayVar("@arr")`.
//!
//! Resolved against `env` alone, that bare key belongs to whatever scope
//! loaded the module — so a consumer's own `my @arr` hijacked every read and
//! every mutation the module made, while `@UFL3::arr` sat holding the correct
//! value nobody consulted. This module supplies the missing step: reconstruct
//! the qualified key from the package the running routine belongs to and
//! prefer the mirror.
//!
//! This is the RESOLUTION fix ADR-0039 §4.1 deliberately excluded from slice
//! 1's `unit_lexicals` store. `our` variables are externally visible
//! (`@UFL3::arr` must stay readable and writable from outside the module), so
//! the qualified mirror — not a private per-compunit cell — is their canonical
//! store; the bug was never a missing store, only a resolution that never
//! looked at it.
//!
//! Containers need nothing beyond this. Their mutation is
//! write-through-the-shared-node (ADR-0013 / ADR-0039 §2), so once a read or a
//! write chokepoint resolves to the mirror's `Gc`, `push` / element-assign /
//! `:delete` land on the module's own container with no separate write-back.
//!
//! SCALARS need one thing more, and it is why they were a separate fix. A
//! scalar write *replaces* a value rather than mutating a shared node, so a
//! read-side preference alone leaves the write landing on the bare `env` key —
//! the loading scope's `my $s`. A plain `our $x` does have a single canonical
//! home for both halves (`OpCode::DeclareOurScalar`'s `ContainerRef` cell), so
//! the write chokepoints (`SetGlobal` and the read-modify-write ops, via
//! `store_scalar_by_name`) write THROUGH that cell and suppress their bare
//! stores. See `our_package_scalar_cell`.

use super::*;

impl Interpreter {
    /// The package-qualified `our_vars` key for a bare `@`/`%` container name,
    /// as seen from the routine that is running, or `None` when the name does
    /// not name an `our` container of a package in scope.
    ///
    /// Candidate packages mirror [`Interpreter::unit_lexical_slot`]'s order
    /// exactly — the running frame's `lexical_package` (the only one that
    /// survives a mixin), the method class, the frame package, then
    /// `current_package` — each walked up its `::` chain so a class declared
    /// inside a module resolves through its owner.
    ///
    /// A container the RUNNING FRAME declares itself shadows the package
    /// variable (`sub f { my @arr = ...; @arr.push(1) }` inside the very
    /// module that declares `our @arr`), so a name present in the frame's own
    /// `locals` is never redirected. That check is what keeps this a
    /// resolution *preference* rather than a hijack in the other direction.
    pub(crate) fn our_package_container_key(&self, name: &str) -> Option<String> {
        // Cheap gates first: this runs on every container read.
        if self.our_vars_is_empty() {
            return None;
        }
        if !(name.starts_with('@') || name.starts_with('%')) {
            return None;
        }
        self.our_package_var_key(name)
    }

    /// Sigil-agnostic core of [`Self::our_package_container_key`]: reconstruct
    /// the package-qualified `our_vars` key for a bare name as seen from the
    /// running routine. Callers own the sigil gate, because the two sigil
    /// families reach this from different chokepoints with different
    /// pre-filters (see [`Self::our_package_scalar_cell`]).
    fn our_package_var_key(&self, name: &str) -> Option<String> {
        // An explicitly-written `@Other::x` is already the package variable it
        // names; anonymous containers are never package variables.
        if name.contains("::") || name.contains("__ANON") {
            return None;
        }
        if self.running_frame_declares_local(name) {
            return None;
        }
        let cur = self.current_package();
        let frame = self.routine_stack().last();
        let candidates = [
            frame.and_then(|f| f.lexical_package).map(|s| s.as_str()),
            self.method_class_stack_top_str(),
            frame
                .map(|f| f.package.as_str())
                .filter(|pkg| !pkg.is_empty() && *pkg != "GLOBAL"),
            Some(cur.as_str()),
        ];
        for candidate in candidates.into_iter().flatten() {
            let mut pkg = candidate;
            loop {
                if pkg.is_empty() || pkg == "GLOBAL" || pkg.contains("::&") {
                    break;
                }
                // `package_qualified_candidate` applies the same twigil /
                // positional-capture exclusions the compiler's
                // `qualify_variable_name` does, so reads and writes reconstruct
                // exactly the key the declaration stored.
                if let Some(key) = Self::package_qualified_candidate(name, pkg)
                    && self.get_our_var(&key).is_some()
                {
                    return Some(key);
                }
                match pkg.rsplit_once("::") {
                    Some((parent, _)) => pkg = parent,
                    None => break,
                }
            }
        }
        None
    }

    /// The shared cell backing a bare `our` SCALAR name, as seen from the
    /// routine that is running, together with the `our_vars` key it lives
    /// under.
    ///
    /// Why a scalar needs more than the container redirect: a container is one
    /// `Gc` node published under several names, so preferring the mirror on a
    /// READ is enough — every mutation writes through the node. A scalar write
    /// *replaces* a value, so the write chokepoint must reach the same storage
    /// the read did or the two halves disagree. `our $x` already has exactly
    /// that storage: `OpCode::DeclareOurScalar` installs ONE `ContainerRef`
    /// cell into the declaring slot, `env[bare]`, `env[qualified]` and
    /// `our_vars[qualified]`, so writing through the cell updates every alias
    /// at once and can never desynchronize them.
    ///
    /// Requiring the entry to BE a cell is the gate, not an optimization. It
    /// is what makes this a redirect to a variable's canonical home rather
    /// than a name-shaped guess: only a plain `our $x` declaration creates
    /// one, so a bareword, a type object, an `our constant`, or an
    /// `our`-scoped sub that happens to share the reconstructed key is never
    /// mistaken for a package scalar. The shapes `use_our_cell` excludes
    /// (`our constant`, `:=`-bound, sigil-less, traited) keep their existing
    /// two-independent-stores behaviour untouched.
    fn our_package_scalar_cell(
        &self,
        name: &str,
    ) -> Option<(String, crate::gc::Gc<std::sync::Mutex<Value>>)> {
        // Cheap pre-gate: empty for any program with no package `our` scalar,
        // so an ordinary variable read pays one hash-set check. It also keeps
        // the `current_package()` lock read and the `locals` scan below off the
        // hot path for every unrelated name.
        if self.our_scalar_cell_names.is_empty() || !self.our_scalar_cell_names.contains(name) {
            return None;
        }
        let key = self.our_package_var_key(name)?;
        match self.get_our_var(&key).map(Value::view) {
            Some(ValueView::ContainerRef(cell)) => Some((key, cell.clone())),
            _ => None,
        }
    }

    /// Read companion of [`Self::our_package_scalar_cell`]: the cell's current
    /// value. Consulted BEFORE `env`, because the bare env key belongs to
    /// whatever scope loaded the module — a consumer's own `my $s` overwrites
    /// it (and its redeclaration guard even replaces the module's cell with
    /// `Nil`), so `env` is not merely stale for this name, it is a different
    /// variable.
    pub(crate) fn our_package_scalar(&self, name: &str) -> Option<Value> {
        let (_, cell) = self.our_package_scalar_cell(name)?;
        let val = cell.lock().unwrap().clone();
        Some(val)
    }

    /// Write companion of [`Self::our_package_scalar_cell`]: store `val` into
    /// the package scalar's canonical cell and report that the write is fully
    /// handled, so the caller SKIPS its bare-name `env` / `our_vars` /
    /// shared-var stores. Suppressing those is the point: the bare key is the
    /// loading scope's `my $s`, and writing it is what let a module's `our $s`
    /// assignment land on its consumer's lexical.
    ///
    /// Mirrors [`Interpreter::unit_scope_lexical_write`] (ADR-0039 slice 1)
    /// exactly, one store over: same "resolve to the cell, write through it,
    /// report handled" contract.
    pub(crate) fn our_package_scalar_write(&mut self, name: &str, val: &Value) -> bool {
        let Some((key, cell)) = self.our_package_scalar_cell(name) else {
            return false;
        };
        Self::cell_store_preserving_container_identity(&key, &cell, val);
        true
    }

    /// The by-name scalar write tail shared by the three read-modify-write ops
    /// (`++`, `--`, and the fused compound assignment `AtomicCompoundVar`):
    /// put `val` where the name's variable actually lives.
    ///
    /// These ops are a second by-name scalar write chokepoint alongside
    /// `SetGlobal`, and they leaked the same way: `$s ~= '+'` inside a module
    /// routine wrote the bare env key — the loading scope's `my $s`. When the
    /// name is a package `our` scalar the write goes through its canonical
    /// cell and the bare stores are skipped; otherwise the ordinary bare-name
    /// store applies, exactly as before.
    pub(crate) fn store_scalar_by_name(&mut self, name: &str, val: &Value) {
        if self.our_package_scalar_write(name, val) {
            return;
        }
        self.set_env_with_main_alias(name, val.clone());
        // A compound assign / inc-dec to a package-scope free variable (`our $X`
        // or a `package { my $X }` lexical) reached from inside a named sub uses
        // the bare name; mirror the value back into the canonical package store
        // so the mutation persists across calls (the env write above is only the
        // same-frame view). No-op for non-package-scope names.
        self.writeback_package_scope_var(name, val);
    }

    /// Read companion of [`Self::our_package_container_key`]: the mirror's
    /// current value, dereferenced. A container's `Gc` is shared with the
    /// stored mirror, so mutating what this returns in place mutates the
    /// package's own container.
    pub(crate) fn our_package_container(&self, name: &str) -> Option<Value> {
        let key = self.our_package_container_key(name)?;
        self.get_our_var(&key).cloned().map(Value::into_deref)
    }

    /// Write companion of [`Self::our_package_container_key`], for the
    /// container-mutation chokepoint [`Interpreter::env_root_descended_mut`].
    pub(crate) fn our_package_container_mut(&mut self, name: &str) -> Option<&mut Value> {
        let key = self.our_package_container_key(name)?;
        self.get_our_var_mut(&key)
    }

    /// Store `val` into the `our` mirror at `key`, writing THROUGH the
    /// container node the mirror already holds rather than swapping it for a
    /// fresh one.
    ///
    /// `our @a` publishes one container under three names at declaration time
    /// (`Dup; SetLocalDecl(slot); SetGlobal("@Pkg::a")`), all sharing one `Gc`.
    /// A plain replace here would orphan the module mainline's own slot and the
    /// `env["@Pkg::a"]` entry on the first `:delete`. Same reasoning as
    /// [`Interpreter::cell_store_preserving_container_identity`], one
    /// indirection shallower — the mirror is a plain `Value`, not a cell.
    pub(crate) fn our_mirror_store_preserving_identity(&mut self, key: &str, val: &Value) {
        let Some(slot) = self.get_our_var_mut(key) else {
            return;
        };
        let replacement = match (slot.view(), val.view()) {
            (ValueView::Hash(old_gc), ValueView::Hash(new_gc))
                if !crate::gc::Gc::ptr_eq(&old_gc, &new_gc) =>
            {
                Some(Self::hash_inplace_reassign(&old_gc, &new_gc))
            }
            (ValueView::Array(old_gc, _), ValueView::Array(new_gc, kind))
                if !crate::gc::Gc::ptr_eq(&old_gc, &new_gc) =>
            {
                Some(Self::array_inplace_reassign(&old_gc, &new_gc, kind))
            }
            _ => None,
        };
        // `hash_inplace_reassign` / `array_inplace_reassign` copy the new
        // contents INTO the old node and return a `Value` pointing at it, so
        // re-seating the slot on that result keeps the original `Gc`.
        let new_slot = replacement.unwrap_or_else(|| val.clone());
        if let Some(slot) = self.get_our_var_mut(key) {
            *slot = new_slot;
        }
    }

    /// Whether the bytecode frame currently executing declares `name` as one
    /// of its own locals — i.e. the bare name is a genuine lexical of this
    /// routine and must not be redirected to a package variable.
    ///
    /// Container locals keep their sigil in `CompiledCode::locals`
    /// (`locals: ["@arr", "%h", "s"]` for a `unit module` mainline), so the
    /// sigiled name is compared directly.
    fn running_frame_declares_local(&self, name: &str) -> bool {
        if self.current_code == 0 {
            return false;
        }
        // SAFETY: `current_code` is the address of the `CompiledCode` of the
        // bytecode frame currently executing in `exec_one` (set at the top of
        // every dispatch). It is an ancestor stack frame of this synchronous
        // call, and therefore alive. Same pattern as `builtins_atomic_shared`
        // and `builtins_dispatch_next`.
        let code = unsafe { &*(self.current_code as *const crate::opcode::CompiledCode) };
        if code.locals.iter().any(|n| n == name) {
            return true;
        }
        // A closure body does not list the enclosing lexicals it captures in
        // its own `locals`, but `compute_upvalues` — run ONLY for
        // anonymous-closure bodies — allocated an upvalue slot for each one.
        // A name with such a slot is therefore a captured lexical of an
        // enclosing scope, and it must win over a same-named `our` of the
        // package the closure happens to be written in:
        //
        //     module M {
        //         our $x = 'our';
        //         sub f { my $x = 'lex'; sub { $x ~= '!' }(); $x }   # 'lex!'
        //     }
        //
        // Because `compute_upvalues` never runs for a named routine's body,
        // this cannot mask the case the redirect exists for — a module
        // routine's bare reference to its own package variable.
        if !self.upvalues.is_empty()
            && code
                .upvalue_syms
                .iter()
                .any(|sym| sym.with_str(|n| n == name))
        {
            return true;
        }
        // `compute_upvalues` only rewrites *pure reads*, so a closure that
        // assigns its captured variable (`sub { $x ~= '!' }`) gets no upvalue
        // slot for it. The compile-time capture record covers that shape:
        // `free_var_parent_slots[i]` is `Some(slot)` exactly when the CREATING
        // frame declares free variable `i` as one of its own locals — i.e. the
        // name is a captured enclosing lexical. It is baked only for closure
        // bodies (`Compiler::add_closure_code_baked`), so a named routine's
        // body, which is where the redirect must fire, is untouched.
        code.free_var_syms
            .iter()
            .zip(code.free_var_parent_slots.iter())
            .any(|(sym, parent)| parent.is_some() && sym.with_str(|n| n == name))
    }
}
