//! The invocant is parameter zero, and a raw parameter binds the caller's
//! container (ADR-0067 slice 3a).
//!
//! `my $a = 42; $a.snitch = 5` writes `5` into `$a` in raku because `.snitch`
//! declares its invocant raw (`method snitch(\snitchee: ...)`) *and* hands it
//! straight back — so the assignment writes through the caller's own container.
//! The same holds for a user-written method: `method m(\S:) is raw { S }`.
//!
//! Raku requires **both** halves and mutsu must too:
//!
//! - the invocant parameter is raw (`\S:`, `$s is raw:`, `$s is rw:`), and
//! - the routine is rw-capable (`is rw` / `is raw` / spells `return-rw`) —
//!   [`Interpreter::method_is_rw_capable`], the ADR-0067 slice 2 oracle.
//!
//! Dropping either keeps the assignment a refusal: `method m(\S:) { S }` is
//! `Cannot modify an immutable Int` and `method m(Any:D $s:) is raw { $s }` is
//! `Cannot assign to a readonly variable or a value`.
//!
//! This module is the single *declaration* oracle both consumers read: the VM
//! gate that decides whether to box the lvalue invocant into a container
//! (`box_raw_lvalue_invocant`, `vm_call_func_ops.rs`) and the runtime write half
//! below. Keeping them on one function is what stops the pair from drifting —
//! a VM that boxes an invocant the runtime then refuses to consume would hand a
//! `ContainerRef` to the ~40 `Instance`/`Array`/`Hash` branches of
//! `assign_method_lvalue_with_values`, silently skipping all of them.

use super::*;

/// Native methods that Rakudo declares `is raw` on their invocant *and* which
/// hand that invocant straight back, so `$a.NAME = v` writes through `$a`.
///
/// A declaration table, deliberately, rather than a call-site name check: every
/// consumer reads this one row, so the family cannot drift apart the way
/// `.item`'s compiler-only erasure did (`scalar_container_alias_name`).
///
/// Deliberately *not* here, measured against raku v2026.07 on 2026-09-05:
///
/// - `.self` — `$a.self =:= $a` is `False` in raku and `$a.self = 5` is
///   refused. It is not in this family (ADR-0067 non-goals).
/// - `.list` — `$a.list =:= $a` is `False` and `$a.list.WHAT` is `(List)`.
///   `$a.list = 7` does reach `$a`, but through *list assignment* into a List
///   whose one element is the invocant's container, which is a different
///   mechanism from the raw-invocant lvalue return. Listing it here would make
///   `$a.list = 7` write the container directly and silently lose the list
///   semantics.
/// - `.item` — genuinely raw (`$a.item =:= $a` is `True`), but the compiler
///   erases `$a.item = 5` to a plain store (`scalar_container_alias_name`), so
///   this row would never be consulted for it. Erasure is sound only because
///   `.item` is pure; `.snitch` notes its invocant, which is why it cannot be
///   erased and needs the real route.
pub(crate) fn native_method_returns_raw_invocant(method: &str) -> bool {
    match method {
        // `method snitch(\snitchee: &snitcher = &note)` — 6.e only, so the
        // method simply is not there below that version (rakudo reports
        // `No such method`), and neither is the lvalue shape.
        "snitch" => crate::parser::current_language_version().starts_with("6.e"),
        _ => false,
    }
}

/// Whether a `ParamDef` declares a **raw invocant** — parameter zero bound to
/// the caller's container rather than to a copy.
///
/// Three spellings, all verified against raku v2026.07: the sigilless `\SELF:`
/// (which the parser records as `sigilless`), `$s is raw:`, and `$s is rw:`. A
/// plain `Any:D $s:` is *not* raw, which is the E2 regression control.
fn param_is_raw_invocant(pd: &crate::ast::ParamDef) -> bool {
    pd.is_invocant
        && (pd.sigilless
            || pd
                .traits
                .iter()
                .any(|t| t == "raw" || t == "rw" || t == "is raw" || t == "is rw"))
}

/// Whether `def` declares a raw invocant. A method with no explicit invocant
/// parameter has an *implicit* one, which is never raw.
///
/// Also read at *registration* time by `Registry::note_raw_invocant_methods`,
/// which raises the `any_raw_invocant_method` pre-filter — one predicate, both
/// consumers, so the filter cannot disagree with the oracle it guards.
pub(crate) fn method_def_has_raw_invocant(def: &crate::runtime::decl_types::MethodDef) -> bool {
    def.param_defs.iter().any(param_is_raw_invocant)
}

impl Interpreter {
    /// The ADR-0067 slice 3a declaration oracle: does `target.method(args)`
    /// resolve to a routine that binds its invocant raw *and* is rw-capable, so
    /// `target.method(args) = v` must write through the caller's container?
    ///
    /// A user-defined method always wins over the native table (an
    /// `augment`ed or user-declared `snitch` shadows the builtin), matching
    /// ordinary dispatch.
    /// The VM's gate short-circuits ahead of this on
    /// [`Registry::any_raw_invocant_method`] plus the native table, so a
    /// program that declares no raw invocant never reaches the resolve below.
    /// That filter is a *necessary condition* derived from the same
    /// `method_def_has_raw_invocant` predicate used here, so it cannot disagree
    /// with this answer; the `debug_assert` in `box_raw_lvalue_invocant` proves
    /// that every run of the debug `t/` suite.
    pub(crate) fn method_returns_raw_invocant(
        &mut self,
        target: &Value,
        method: &str,
        method_args: &[Value],
    ) -> bool {
        let class_name = Self::raw_invocant_class_name(target);
        if let Some(def) = self.resolve_method(&class_name, method, method_args) {
            return Self::method_is_rw_capable(&def) && method_def_has_raw_invocant(&def);
        }
        native_method_returns_raw_invocant(method)
    }

    /// The class name a method is resolved against for an lvalue invocant.
    /// Unlike `lvalue_invocant_class_name` (which only serves the
    /// Instance/type-object halves of ADR-0059) this answers for *any* value,
    /// because a raw invocant is exactly the case where the invocant is an
    /// ordinary `Int`/`Str`/... and the routine came from `augment class Any`.
    fn raw_invocant_class_name(target: &Value) -> String {
        match target.view() {
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            ValueView::Package(name) => name.resolve(),
            _ => crate::value::what_type_name(target),
        }
    }

    /// The write half: `$a.m(args) = value` where `m` binds its invocant raw
    /// and is rw-capable, and the VM has already boxed the invocant into a
    /// container (`box_raw_lvalue_invocant`).
    ///
    /// Runs the routine with the *container* as its invocant and writes through
    /// whatever container it hands back. That is the general rule, not a
    /// shortcut: a raw-invocant body is free to return some other location
    /// (`method m(\S:) is raw { $!other }`), and one that returns a plain value
    /// (`method m(\S:) is raw { 42 }`) must be refused exactly as raku refuses
    /// it — which is what falling through to the existing chain produces.
    ///
    /// Returns `Ok(None)` when the shape does not apply, leaving the caller's
    /// existing chain (and its diagnostics) untouched.
    pub(crate) fn try_raw_invocant_container_lvalue(
        &mut self,
        target: &Value,
        method: &str,
        method_args: &[Value],
        value: &Value,
    ) -> Result<Option<Value>, RuntimeError> {
        // The VM boxes the invocant only when this same oracle says yes, so a
        // non-container target here means the gate declined (an unnamed
        // invocant, a non-scalar location) and this shape is not applicable.
        if !target.is_container_ref() {
            return Ok(None);
        }
        let inner = target.deref_container();
        if !self.method_returns_raw_invocant(&inner, method, method_args) {
            return Ok(None);
        }
        let was_lvalue = self.in_lvalue_assignment;
        self.in_lvalue_assignment = true;
        // The pending argument-source names still describe the enclosing
        // `__mutsu_assign_method_lvalue` call, whose first "argument" is the
        // invocant, while `method_args` holds only the method's own arguments —
        // the two are off by one, so a raw (`\c`) parameter that re-reads its
        // argument by source name would bind the INVOCANT into the method's
        // first parameter. This call site supplies values, not source names.
        // Mirrors `try_rw_method_container_lvalue`.
        let saved_sources = self.take_pending_call_arg_sources();
        let result = self.call_method_with_values(target.clone(), method, method_args.to_vec());
        self.set_pending_call_arg_sources(saved_sources);
        self.in_lvalue_assignment = was_lvalue;
        let result = result?;
        match self.assign_lvalue_container(&result, value.clone()) {
            Some(assigned) => assigned.map(Some),
            None => Ok(None),
        }
    }
}
