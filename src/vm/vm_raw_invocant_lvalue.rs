//! The VM half of ADR-0067 slice 3a: box the invocant of an lvalue method call
//! into a container when — and only when — the callee binds parameter zero raw.
//!
//! `$a.snitch = 5` already compiles to
//! `GetLocal(0); ContainerizePair; WrapVarRef{name_idx, slot}; ...
//! CallFunc "__mutsu_assign_method_lvalue"`, so the invocant arrives tagged with
//! its source name. The missing step is the one a `return-rw` tail emits right
//! after such a tag: boxing the named slot into a shared cell.
//!
//! It cannot be emitted unconditionally by the compiler. Rawness is not
//! statically known — `$a.snitch`'s callee depends on `$a`'s runtime type and,
//! for the dynamic spellings, on a runtime method-name string — and boxing every
//! lvalue invocant would hand a `ContainerRef` to the ~40 `target.view()`
//! branches of `assign_method_lvalue_with_values` that match `Instance` /
//! `Array` / `Hash` directly. Nor can it be done inside that runtime function:
//! `capture_var_cell_inner` needs the frame's `&CompiledCode` to resolve a slot,
//! which the runtime entry does not have.
//!
//! So the box happens here, in the VM, where the frame's `code` and the invocant
//! value are both in hand, gated on the same declaration oracle the runtime
//! consumer reads (`Interpreter::method_returns_raw_invocant`).

use super::*;

impl Interpreter {
    /// Replace `args[0]` (the invocant of a `__mutsu_assign_method_lvalue` call)
    /// with the caller's container, when the callee binds its invocant raw.
    ///
    /// A no-op for every other callee, so the ordinary lvalue paths keep seeing
    /// exactly the value they see today.
    pub(super) fn box_raw_lvalue_invocant(&mut self, code: &CompiledCode, args: &mut [Value]) {
        // target, method name, method args, value -- the writeback-target name
        // is argument 4 and is what makes the element/attribute spellings work.
        if args.len() < 4 {
            return;
        }
        // The cheapest possible gate, first: this runs on EVERY `$obj.attr = v`,
        // and everything below it allocates (two `to_string_value()`s, a
        // `method_args` vector, an MRO walk plus a `MethodDef` clone). A raw
        // invocant is rare, so ask the registry's set-only flag and the native
        // table — both against a *borrowed* method name — before paying for any
        // of it. Measured: doing the extraction unconditionally costs ~13% on a
        // tight `$p.x = $i` loop.
        match args[1].as_str() {
            Some(name)
                if self.registry().any_raw_invocant_method
                    || crate::runtime::raw_invocant::native_method_returns_raw_invocant(name) => {}
            // A non-`Str` method name cannot happen from the compiler, but a
            // dynamic spelling could in principle; fall back to the full oracle
            // rather than silently declining.
            None => {}
            _ => {
                self.debug_verify_raw_invocant_filter(args);
                return;
            }
        }
        let target = args[0].clone();
        if target.is_container_ref() {
            return;
        }
        // The invocant's source name. `$a.m = v` tags it with `WrapVarRef`; the
        // element spellings (`@a[0].m = v`) instead stash the invocant in a
        // compiler temp whose name the parser passes as argument 4, and whose
        // value the copy-out tail reads back (`GetGlobal(tmp); ...;
        // IndexAssignExprNamed`). Either way the name is the location to box.
        let (source_name, inner, slot_hint) = match target.as_varref() {
            Some((name, value, _)) => (
                name.resolve().to_string(),
                value.clone(),
                target.varref_slot(),
            ),
            None => (String::new(), target.clone(), None),
        };
        let name = if source_name.is_empty() {
            match args.get(4).map(Value::to_string_value) {
                Some(n) if !n.is_empty() => n,
                _ => return,
            }
        } else {
            source_name
        };
        let method = args[1].to_string_value();
        let method_args = Self::lvalue_method_args(&args[2]);
        if !self.method_returns_raw_invocant(&inner, &method, &method_args) {
            return;
        }
        if let Some(cell) = self.capture_lvalue_invocant_cell(code, &name, inner, slot_hint) {
            args[0] = cell;
        }
    }

    /// Debug-only: re-derive the slow answer whenever the cheap pre-filter
    /// declined, and blow up if they disagree.
    ///
    /// The filter is a necessary condition maintained at *registration* time
    /// (`Registry::note_raw_invocant_methods`), so it can only go wrong if some
    /// path writes `method_entries`' `user_candidates` column without going
    /// through the documented mutators. This turns that into a deterministic
    /// failure of the debug `t/` suite instead of a feature that silently stops
    /// working.
    #[cfg(debug_assertions)]
    fn debug_verify_raw_invocant_filter(&mut self, args: &[Value]) {
        let method = args[1].to_string_value();
        let method_args: Vec<Value> = Self::lvalue_method_args(&args[2]);
        // Derive the invocant exactly as the live path does, or the resolve
        // would run against `VarRef` instead of the value it wraps.
        let inner = match args[0].as_varref() {
            Some((_, value, _)) => value.clone(),
            None => args[0].clone(),
        };
        assert!(
            !self.method_returns_raw_invocant(&inner, &method, &method_args),
            "the any_raw_invocant_method pre-filter declined, but the oracle says \
             .{method} takes a raw invocant -- a registration path wrote \
             user_candidates without going through Registry's mutators"
        );
    }

    #[cfg(not(debug_assertions))]
    fn debug_verify_raw_invocant_filter(&mut self, _args: &[Value]) {}

    /// The method's own argument list, as `__mutsu_assign_method_lvalue`
    /// packs it into argument 2.
    fn lvalue_method_args(packed: &Value) -> Vec<Value> {
        match packed.view() {
            ValueView::Array(items, ..) => items.to_vec(),
            ValueView::Nil => Vec::new(),
            _ => vec![packed.clone()],
        }
    }

    /// The container for the named lvalue invocant, or `None` when there is no
    /// storage location to hand out (in which case the assignment keeps its
    /// existing loud refusal rather than silently writing a disconnected cell).
    ///
    /// Four routes, in order. **Reusing an existing container always comes
    /// before minting one** — a name that already denotes a location must hand
    /// out *that* location, or the write lands in a disconnected cell and is
    /// silently lost. (Measured: `for @a -> $e is rw { $e.m = 3 }` binds `$e` to
    /// the element's own promoted cell, and minting a fresh one dropped the
    /// write.)
    ///
    /// 1. **A local of this frame** — `capture_var_cell` boxes the slot into a
    ///    shared cell and mirrors it into env, which is the same cell a
    ///    `return-rw` tail or a `\($a)` capture would produce. It already
    ///    returns an existing cell untouched when the slot holds one. This is
    ///    `$a.m = v`.
    /// 2. **A name whose env entry is already a container** — an `is rw` /
    ///    `<->` loop parameter aliasing the source element, a `:=`-bound name,
    ///    a captured-outer scalar that was boxed elsewhere. Hand out that cell.
    /// 3. **A `$`-scalar local that route 1 declined** because its value is
    ///    reference-shaped (an `Instance`). See the comment on the branch.
    /// 4. **A name that only lives in env** — the compiler temp the element
    ///    spellings route through (`__mutsu_tmp_assign_method_target_N`).
    ///    `capture_var_cell_inner` returns such a value unchanged (it has no
    ///    slot to box), so box it into a cell stored in env under that name.
    ///    The copy-out tail reads the name back through `GetGlobal`, which
    ///    dereferences the cell, so the write reaches `@a[0]` / `%h<a>`.
    ///
    /// Route 4 is deliberately restricted to *scalar-shaped* values, mirroring
    /// `capture_var_cell_inner`'s own `is_reference` guard: boxing an
    /// `Array`/`Hash`/`Instance` env entry would produce a cell that disagrees
    /// with the aggregate's own identity-shared storage.
    fn capture_lvalue_invocant_cell(
        &mut self,
        code: &CompiledCode,
        name: &str,
        inner: Value,
        slot_hint: Option<u32>,
    ) -> Option<Value> {
        let boxed = self.capture_var_cell(code, name, inner.clone(), slot_hint);
        if Self::is_lvalue_location(&boxed) {
            return Some(boxed);
        }
        let existing = self.env().get(name).cloned();
        if let Some(existing) = existing.as_ref().filter(|v| Self::is_lvalue_location(v)) {
            return Some(existing.clone());
        }
        // A `$`-scalar local holding a *reference* (an `Instance`): the general
        // capture paths deliberately refuse to re-containerize one, but a raw
        // invocant is the caller's Scalar container and raku replaces its whole
        // contents — `class C { method m(\S:) is raw { S } }; my $c = C.new;
        // $c.m = 5` leaves `$c` holding `5`. Scalar names only: `@a`/`%h`
        // locals keep their sigil in `code.locals`, and their own identity-
        // shared storage is not this cell's to replace.
        if Self::is_scalar_local_name(name)
            && let Some(idx) = Self::frame_local_slot(code, name, slot_hint)
        {
            let cell = self.locals[idx].clone().into_container_ref();
            self.locals[idx] = cell.clone();
            let sym = code.locals_sym.get(idx).copied();
            self.set_env_with_main_alias_sym(name, sym, cell.clone());
            return Some(cell);
        }
        existing?;
        if !Self::raw_invocant_boxable_in_env(&inner) {
            return None;
        }
        let cell = inner.into_container_ref();
        self.set_env_with_main_alias_sym(name, None, cell.clone());
        Some(cell)
    }

    /// Whether a value already IS a storage location, so it must be handed out
    /// rather than boxed into a fresh cell. Mirrors
    /// `capture_var_cell_inner`'s `is_lvalue_container_value`.
    fn is_lvalue_location(value: &Value) -> bool {
        value.is_container_ref() || matches!(value.view(), ValueView::HashEntryRef { .. })
    }

    /// Whether `name` (as it appears in `code.locals`) is a plain `$`-scalar
    /// lexical. Array, hash and code locals keep their sigil there, and a
    /// twigil / dynamic / attribute / compiler-synthesized name is not a plain
    /// lexical slot to re-containerize. Mirrors
    /// `Compiler::is_plain_lexical_name`, which gates the compile-side
    /// container-capture edge for the same reason.
    fn is_scalar_local_name(name: &str) -> bool {
        !name.is_empty()
            && !name.starts_with(['$', '@', '%', '&', '.', '!', '^', '*'])
            && name != "_"
            && !name.contains("::")
            && !name.starts_with("__mutsu_")
            && !name.starts_with("__ANON")
    }

    /// The frame slot `name` occupies, preferring the compile-time-resolved
    /// hint (shadow slots give several `code.locals` entries the same name, so
    /// a by-name search would pick the wrong one).
    fn frame_local_slot(code: &CompiledCode, name: &str, slot_hint: Option<u32>) -> Option<usize> {
        match slot_hint {
            Some(hint)
                if hint != u32::MAX
                    && code.locals.get(hint as usize).map(String::as_str) == Some(name) =>
            {
                Some(hint as usize)
            }
            Some(hint) if hint == u32::MAX => None,
            _ => code.locals.iter().rposition(|n| n == name),
        }
    }

    /// Whether an env-only name may be boxed into a fresh scalar container cell.
    /// Reference-shaped values carry their own shared storage and type objects
    /// are not locations, so neither is boxed — matching the guard
    /// `capture_var_cell_inner` applies to a frame local.
    fn raw_invocant_boxable_in_env(value: &Value) -> bool {
        !matches!(
            value.view(),
            ValueView::Array(..)
                | ValueView::Hash(..)
                | ValueView::Sub(..)
                | ValueView::Instance { .. }
                | ValueView::Proxy { .. }
                | ValueView::Package(_)
        )
    }
}
