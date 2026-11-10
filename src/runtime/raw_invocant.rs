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
use std::sync::atomic::{AtomicBool, Ordering};

/// Lock-free mirror of [`Registry::any_raw_invocant_method`], raised at the
/// same instant by the same writer ([`Registry::note_raw_invocant_methods`]).
///
/// Slice 3a's gate runs on `$obj.attr = v` and can afford
/// `self.registry().any_raw_invocant_method`; slice 3b's runs on **every**
/// `$var.method(...)`, where an `RwLock` read acquisition per call is not
/// affordable. Same soundness argument as `env.rs`'s `CLOSURE_META_KEY_SEEN`:
/// monotone, set-only, and an over-set only makes the (correct) resolve run.
/// Process-global rather than per-interpreter for the same reason that flag is
/// — a second interpreter in the same process merely inherits a conservative
/// `true`.
static ANY_RAW_INVOCANT_METHOD: AtomicBool = AtomicBool::new(false);

/// Raise the mirror. Called only from `Registry::note_raw_invocant_methods`, so
/// the two flags cannot drift apart.
pub(crate) fn note_any_raw_invocant_method() {
    ANY_RAW_INVOCANT_METHOD.store(true, Ordering::Relaxed);
}

/// Whether any registered method anywhere declares a raw invocant. The cheap
/// pre-gate of the slice 3b arrival path.
#[inline]
pub(crate) fn any_raw_invocant_method_possible() -> bool {
    ANY_RAW_INVOCANT_METHOD.load(Ordering::Relaxed)
}

/// Words of [`RAW_INVOCANT_NAME_BITS`]: 1024 x 64 = 65536 bits.
const RAW_NAME_WORDS: usize = 1024;

/// A bit set over the method names that declare a raw invocant, indexed by the
/// low 16 bits of the name's symbol id. Two names may share a bit (a
/// collision answers `true` for a name that declares none), never the other
/// way round, so the gate it feeds only ever errs towards running the correct
/// resolve. Lock-free for the same reason [`ANY_RAW_INVOCANT_METHOD`] is: the
/// gate runs on every `$var.method(...)`.
static RAW_INVOCANT_NAME_BITS: [std::sync::atomic::AtomicU64; RAW_NAME_WORDS] =
    [const { std::sync::atomic::AtomicU64::new(0) }; RAW_NAME_WORDS];

/// Record that a method named `name` declares a raw invocant. Called only from
/// `Registry::note_raw_invocant_methods`, beside [`note_any_raw_invocant_method`].
// Cost: O(1).
pub(crate) fn note_raw_invocant_method_name(name: Symbol) {
    let bit = name.raw_id() as usize & (RAW_NAME_WORDS * 64 - 1);
    RAW_INVOCANT_NAME_BITS[bit >> 6].fetch_or(1 << (bit & 63), Ordering::Relaxed);
}

/// Whether a method called `name` may declare a raw invocant: `false` proves
/// that no registered method of that name does, so a call of it never binds
/// its invocant raw. The per-call pre-gate of the slice 3b arrival path --
/// [`any_raw_invocant_method_possible`] says "somewhere", this says "under this
/// name", so a program that loads one module with a raw-invocant method does
/// not slow every other method call down.
// Cost: O(1).
#[inline]
pub(crate) fn raw_invocant_method_possible_for(name: Symbol) -> bool {
    if !any_raw_invocant_method_possible() {
        return false;
    }
    let bit = name.raw_id() as usize & (RAW_NAME_WORDS * 64 - 1);
    RAW_INVOCANT_NAME_BITS[bit >> 6].load(Ordering::Relaxed) & (1 << (bit & 63)) != 0
}

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
        "snitch" => crate::parser::unit_revisions::revision_e_methods_visible(),
        _ => false,
    }
}

/// Native lvalue methods that Rakudo declares with a raw invocant
/// (`method substr-rw(\SELF: ...) is rw`) but that hand back a Proxy into the
/// invocant rather than the invocant itself, so they are not rows of
/// [`native_method_returns_raw_invocant`]: the write is the method's own
/// (`Interpreter::assign_substr_rw`), applied to the boxed container.
///
/// Boxing the invocant is what lets `$c.s.substr-rw(0, 1) = "Z"` (an `is rw`
/// accessor) and `$x.substr-rw(...) = v` on an `is rw` parameter reach the
/// caller's container, which a by-name writeback cannot (#10790). `subbuf-rw`
/// is deliberately absent: it splices the Buf's storage node in place, which
/// every alias already sees.
pub(crate) fn native_method_writes_raw_invocant(method: &str) -> bool {
    method == "substr-rw"
}

/// The VM gates' native pre-filter: either native raw-invocant family.
pub(crate) fn native_method_boxes_lvalue_invocant(method: &str) -> bool {
    native_method_returns_raw_invocant(method) || native_method_writes_raw_invocant(method)
}

/// Whether a `ParamDef` declares a **raw invocant** — parameter zero bound to
/// the caller's container rather than to a copy.
///
/// Three spellings, all verified against raku v2026.07: the sigilless `\SELF:`
/// (which the parser records as `sigilless`), `$s is raw:`, and `$s is rw:`. A
/// plain `Any:D $s:` is *not* raw, which is the E2 regression control.
pub(crate) fn param_is_raw_invocant(pd: &crate::ast::ParamDef) -> bool {
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
        if self.mixin_role_method_defs_any(target, method, |def| {
            Self::method_is_rw_capable(def) && method_def_has_raw_invocant(def)
        }) == Some(true)
        {
            return true;
        }
        let class_name = Self::raw_invocant_class_name(target);
        if let Some(def) = self.resolve_method(&class_name, method, method_args, target) {
            return Self::method_is_rw_capable(&def) && method_def_has_raw_invocant(&def);
        }
        native_method_returns_raw_invocant(method)
    }

    /// Whether the VM should box the invocant of `target.method(args) = v`:
    /// [`Self::method_returns_raw_invocant`], or a native
    /// [`native_method_writes_raw_invocant`] method no user routine shadows.
    pub(crate) fn lvalue_call_boxes_invocant(
        &mut self,
        target: &Value,
        method: &str,
        method_args: &[Value],
    ) -> bool {
        if self.method_returns_raw_invocant(target, method, method_args) {
            return true;
        }
        native_method_writes_raw_invocant(method) && {
            let class_name = Self::raw_invocant_class_name(target);
            self.resolve_method(&class_name, method, method_args, target)
                .is_none()
        }
    }

    /// The ADR-0067 slice 3b **arrival** oracle: does `target.method(args)`
    /// resolve to a routine that binds its invocant raw, so mutation *through*
    /// the invocant inside the body must reach the caller's variable?
    ///
    /// This is deliberately a weaker question than
    /// [`Self::method_returns_raw_invocant`], and the difference was measured
    /// against raku v2026.07 rather than inherited from the ADR text, which
    /// asserted that both halves of the contract apply to slice 3b too:
    ///
    /// ```raku
    /// class C { method m(\S:)          { S = 7 } }; C.new.m   # raku: 7
    /// class C { method m($s is raw:)   { $s = 7 } }; C.new.m  # raku: 7
    /// class C { method m($s:)          { $s = 7 } }; C.new.m  # raku: dies
    /// ```
    ///
    /// No `is raw` on the *routine* anywhere — that trait governs the outbound
    /// direction (whether the call is an lvalue), which is slice 3a's question.
    /// Inbound, only the invocant *parameter*'s own rawness matters, so this
    /// oracle drops the `method_is_rw_capable` conjunct and keeps the
    /// [`method_def_has_raw_invocant`] one. Both oracles read that same
    /// predicate, so the two halves cannot disagree about what "raw invocant"
    /// means.
    ///
    /// There is deliberately no native fallback row: no native method mutates
    /// its invocant through parameter zero (`.snitch`, the one row of
    /// [`native_method_returns_raw_invocant`], only hands it back).
    pub(crate) fn method_binds_raw_invocant(
        &mut self,
        target: &Value,
        method: &str,
        method_args: &[Value],
    ) -> bool {
        if self.mixin_role_method_defs_any(target, method, method_def_has_raw_invocant)
            == Some(true)
        {
            return true;
        }
        let class_name = Self::raw_invocant_class_name(target);
        self.resolve_method(&class_name, method, method_args, target)
            .as_ref()
            .is_some_and(method_def_has_raw_invocant)
    }

    /// Whether `pred` holds for any candidate a `but`-mixed value's composed
    /// roles declare for `method` (the first role that declares it answers):
    /// a `Mixin` (`"abc" but R`) has no class of its own to resolve
    /// against, its method is a role's (the dispatch
    /// `dispatch_mixin_method_call` runs). `None` when `target` is no `Mixin`
    /// or no composed role declares the method. Borrowed: no `MethodDef` is cloned.
    // Cost: O(r + m), r = roles composed onto the value (a registry lookup each),
    // m = candidates of the first role declaring the method.
    fn mixin_role_method_defs_any(
        &self,
        target: &Value,
        method: &str,
        pred: impl Fn(&crate::runtime::decl_types::MethodDef) -> bool,
    ) -> Option<bool> {
        let ValueView::Mixin(_, mixins) = target.view() else {
            return None;
        };
        let lookup = method.strip_prefix('!').unwrap_or(method);
        // Borrowed: this runs on every call of a mixin's method (a CArray
        // `AT-POS`), and cloning the role's whole method table cost ~18k Ir
        // each time (#16215).
        mixins
            .keys()
            .filter_map(|k| k.strip_prefix("__mutsu_role__"))
            .find_map(|role_name| {
                self.with_mixin_role_def(mixins, role_name, |role| {
                    role.methods
                        .get(lookup)
                        .map(|defs| defs.iter().any(&pred))
                })
                .flatten()
            })
    }

    /// The class name a method is resolved against for an lvalue invocant.
    /// Unlike `lvalue_invocant_class_name` (which only serves the
    /// Instance/type-object halves of ADR-0059) this answers for *any* value,
    /// because a raw invocant is exactly the case where the invocant is an
    /// ordinary `Int`/`Str`/... and the routine came from `augment class Any`.
    // Cost: O(1) for an instance, type object or a memoized role-mixed instance;
    // otherwise `what_type_name`'s.
    pub(crate) fn raw_invocant_class_name(target: &Value) -> String {
        match target.view() {
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            ValueView::Package(name) => name.resolve(),
            ValueView::Mixin(inner, mixins) => {
                // A role-mixed class instance: the name is a function of its
                // override node (never mutated in place) and its class, and
                // building it renders every role (~20k Ir per call of a
                // CArray `AT-POS`, #16215). The entry owns a clone of the node
                // so its address stays this composition's.
                let ValueView::Instance { class_name, .. } = inner.view() else {
                    return crate::value::what_type_name(target);
                };
                type Memo = rustc_hash::FxHashMap<
                    (usize, Symbol),
                    (crate::gc::Gc<crate::value::MixinOverrides>, String),
                >;
                thread_local! {
                    static NAMES: std::cell::RefCell<Memo> = std::cell::RefCell::default();
                }
                let key = (&**mixins as *const crate::value::MixinOverrides as usize, class_name);
                if let Some(hit) = NAMES.with(|m| m.borrow().get(&key).map(|(_, n)| n.clone())) {
                    return hit;
                }
                let name = crate::value::what_type_name(target);
                NAMES.with(|m| {
                    let mut m = m.borrow_mut();
                    if m.len() >= 4096 {
                        m.clear();
                    }
                    m.insert(key, (mixins.clone(), name.clone()));
                });
                name
            }
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
        // A `but`-mixed value runs the role's method through the mixin
        // dispatch, which takes the container from the arrival channel (the
        // generic entry does not look through a boxed `Mixin`).
        let role_raw = matches!(inner.view(), ValueView::Mixin(..))
            && self.mixin_role_method_defs_any(&inner, method, method_def_has_raw_invocant)
                == Some(true);
        let result = if role_raw {
            let previous = self.pending_raw_invocant.replace(Box::new(
                crate::vm::vm_raw_invocant_arrival::PendingRawInvocant {
                    method: method.to_string(),
                    cell: target.clone(),
                    raw_parameter: true,
                    implicit_self: false,
                },
            ));
            let result = self.call_method_with_values(inner.clone(), method, method_args.to_vec());
            self.pending_raw_invocant = previous;
            result
        } else {
            self.call_method_with_values(target.clone(), method, method_args.to_vec())
        };
        self.set_pending_call_arg_sources(saved_sources);
        self.in_lvalue_assignment = was_lvalue;
        let result = result?;
        match self.assign_lvalue_container(&result, value.clone()) {
            Some(assigned) => assigned.map(Some),
            None => Ok(None),
        }
    }
}
