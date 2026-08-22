//! Bare-name resolution for a package's `our`-declared `@`/`%` containers.
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
        code.locals.iter().any(|n| n == name)
    }
}
