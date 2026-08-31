//! Declaration-metadata types for the class/role/method object model —
//! `ClassDef`/`RoleDef`/`RoleCandidateDef`/`SubsetDef`/`MethodDef` and the
//! dispatch-frame helpers (`MethodDispatchFrame`/`WrapDispatchFrame`,
//! `ProtoMethodCtx`, `SquishIteratorMeta`).
//!
//! Extracted verbatim from `runtime/mod.rs` (2026-07-21 hygiene re-slim).
//! Previously module-private structs/fields are widened to `pub(crate)` so the
//! sibling `class`/`dispatch`/`registration` modules keep their direct field
//! access; the whole set is re-exported from `runtime` via
//! `pub(crate) use self::decl_types::*`.

use super::ClassAttributeDef;
use crate::ast::{Expr, FunctionDef, ParamDef, Stmt};
use crate::value::Value;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

#[derive(Debug, Clone, Default)]
pub(crate) struct ClassDef {
    pub(crate) parents: Vec<String>,
    // (name, is_public, default, is_rw, is_required, sigil, where_constraint)
    pub(crate) attributes: Vec<ClassAttributeDef>,
    pub(crate) attribute_types: HashMap<String, String>, // attr_name -> type constraint
    pub(crate) attribute_smileys: HashMap<String, String>, // attr_name -> smiley ("D", "U", "_")
    pub(crate) attribute_built: HashMap<String, bool>,
    /// Attributes declared with `has $x` (no twigil) — the bare name is an alias
    /// for `$!x` inside class methods.
    pub(crate) alias_attributes: HashSet<String>,
    pub(crate) native_methods: HashSet<String>,
    pub(crate) mro: std::sync::Arc<[crate::symbol::Symbol]>,
    /// Attribute var names (e.g. "!foo") that have `handles *` wildcard delegation.
    pub(crate) wildcard_handles: Vec<String>,
    /// Class-level attributes declared with `our $.x` or `my $.x` (shared across instances).
    /// Maps attribute name to its current value.
    pub(crate) class_level_attrs: HashMap<String, Value>,
}

#[derive(Debug, Clone)]
pub(crate) struct RoleDef {
    pub(crate) attributes: Vec<ClassAttributeDef>,
    pub(crate) methods: HashMap<String, Vec<MethodDef>>,
    pub(crate) is_stub_role: bool,
    pub(crate) is_hidden: bool,
    /// Whether this role was declared with `is rw` or `also is rw`.
    /// Used during `register_role_decl` to compute effective is_rw for attributes.
    #[allow(dead_code)]
    pub(crate) is_rw: bool,
    /// Captured environment for evaluating attribute defaults in closures.
    pub(crate) captured_env: Option<HashMap<String, Value>>,
    /// Attribute var names (e.g. "!foo") that have `handles *` wildcard delegation.
    pub(crate) wildcard_handles: Vec<String>,
    /// Unique identifier for this role definition instance, used to distinguish
    /// different lexical roles with the same name.
    pub(crate) role_id: u64,
    /// Attribute conflicts detected during role-to-role composition.
    /// Each entry is (attr_name, declaring_role, conflicting_role).
    pub(crate) attribute_conflicts: Vec<(String, String, String)>,
    /// Attribute names declared directly in this role's body (not inherited
    /// via `does`). Used to disambiguate diamond composition (where the same
    /// attribute reaches via two paths from a shared ancestor) from a real
    /// attribute conflict.
    pub(crate) own_attribute_names: HashSet<String>,
    /// Precompiled per-statement mirror of the role body's non-declaration
    /// statements (ADR-0019 D8-1), copied from
    /// `CompiledRoleDeclPlan::deferred_body_ops` at registration. Every
    /// consumer site runs each op's precompiled `chunk` (falling back to its
    /// `raw` statement for the `TokenRule` carve-out) — the raw-`Stmt` vec
    /// this used to mirror (`deferred_body_stmts`) was dropped in D8-4 once
    /// D8-2 made every execution site read this field instead.
    pub(crate) deferred_body: Vec<crate::opcode::DeferredBodyOp>,
    /// Unknown lowercase trait names deferred for custom `trait_mod:<is>` dispatch.
    pub(crate) deferred_custom_traits: Vec<String>,
}

#[derive(Debug, Clone)]
pub(crate) struct RoleCandidateDef {
    pub(crate) type_params: Vec<String>,
    pub(crate) type_param_defs: Vec<ParamDef>,
    pub(crate) role_def: RoleDef,
    /// Parent classes/roles declared via `is` on this candidate.
    pub(crate) parents: Vec<String>,
    /// Language version (e.g. "6.c") captured at registration time.
    pub(crate) language_version: String,
}

#[derive(Debug, Clone)]
pub(crate) struct SubsetDef {
    pub(crate) base: String,
    pub(crate) predicate: Option<Expr>,
    pub(crate) version: String,
}

#[derive(Debug, Clone)]
pub(crate) struct MethodDef {
    /// Package containing the method declaration lexically. This can differ
    /// from the owning class for an explicitly qualified class declaration.
    pub(crate) lexical_package: String,
    pub(crate) params: Vec<String>,
    pub(crate) param_defs: Vec<ParamDef>,
    /// Method body AST. Wrapped in Arc to make MethodDef clones O(1) since
    /// the body is never mutated after construction and can be large.
    pub(crate) body: std::sync::Arc<Vec<Stmt>>,
    pub(crate) is_rw: bool,
    pub(crate) is_private: bool,
    pub(crate) is_multi: bool,
    pub(crate) is_my: bool,
    /// Role where this method was originally declared when composed into a class.
    pub(crate) role_origin: Option<String>,
    /// The deepest/original role where this method was first defined (for diamond detection).
    pub(crate) original_role: Option<String>,
    pub(crate) return_type: Option<String>,
    pub(crate) compiled_code: Option<std::sync::Arc<crate::opcode::CompiledCode>>,
    /// Compiled functions produced while compiling this method's body — e.g. a
    /// `sub` declared inside the method. Without this, a nested sub's compiled
    /// routine key resolves against an empty table at call time and every
    /// dispatch site substitutes `CompiledFns::default()`, so the plan-derived
    /// def can never register body-less (ADR-0019 C6e-3c).
    pub(crate) compiled_fns: Option<std::sync::Arc<crate::opcode::CompiledFns>>,
    /// Delegation info: (attribute_var_name, target_method_name).
    /// When set, the method forwards the call (with all args) to the named method
    /// on the object stored in the given attribute.
    pub(crate) delegation: Option<(String, String)>,
    /// `is default` trait — this candidate is preferred when multi dispatch ties.
    pub(crate) is_default: bool,
    /// `is DEPRECATED` message: None = not deprecated.
    pub(crate) deprecated_message: Option<String>,
    /// Whether this is a submethod (not inherited by subclasses).
    pub(crate) is_submethod: bool,
    /// Captured lexical environment for a method. This is populated for a method
    /// installed via `.^add_method` with a closure literal and for a class method
    /// declared inside a routine. In both cases, the plain body+compiled-code
    /// `MethodDef` would otherwise drop the defining scope. Method dispatch and
    /// candidate matching overlay these bindings so body reads and declaration-
    /// time parameter expressions resolve lexically.
    pub(crate) captured_env: Option<crate::env::Env>,
    /// Source file the method body was declared in (None = main script or a
    /// synthetic/native method with no real source). Flows into the pushed
    /// `RoutineFrame::def_file` at call time (`push_method_routine_with_location`),
    /// mirroring `FunctionDef::source_file` for subs and `SubData::source_file`
    /// for closures — without it, `executing_source_file()`'s frame walk always
    /// fell through past a method frame (which never carried a `def_file`) to
    /// the dynamically-scoped `?FILE`, misattributing `callframe(N).file` for
    /// any method defined in a `use`d module (see
    /// `news/2026-08/method-frame-def-file.md`).
    pub(crate) source_file: Option<String>,
    /// Role type-parameter bindings for THIS composed candidate (`T => Int`),
    /// stamped by `compose_role_into_class` when a parameterized role is
    /// composed. Injected into the body env at dispatch in preference to the
    /// per-class `Registry::class_role_param_bindings` map, which is
    /// last-write-wins when the same role is composed twice with different
    /// type args (`does R[Int] does R[Str]` — both candidates' bodies must
    /// see their OWN `T`, not whichever composition ran last). `None` for
    /// methods not composed from a parameterized role.
    pub(crate) role_param_bindings: Option<std::sync::Arc<Vec<(String, Value)>>>,
}

/// Invocant context for an active `proto method` `{*}` dispatch.
#[derive(Debug, Clone)]
pub(crate) struct ProtoMethodCtx {
    pub(crate) invocant: Value,
    /// The caller's `pending_call_arg_sources` as seen at the original call
    /// site, captured before the proto body runs. `{*}` restores it so the
    /// multi-candidate selection can still tell that an argument came from a
    /// writable variable — an `is rw` candidate is otherwise unmatchable
    /// through a `proto method f(|) {*}` (the proto's own signature declares no
    /// rw parameter, so `proto_rw_redispatch_args` rebuilds nothing).
    pub(crate) call_arg_sources: Option<Vec<Option<String>>>,
}

/// One entry of `multi_dispatch_stack`: (function_name, remaining_candidates,
/// original_args, first_candidate_rw_params, dispatch_token). See the field doc
/// on `Interpreter::multi_dispatch_stack`. The trailing `u64` is the ADR-0019
/// E9b-0 push-order token — see `MethodDispatchFrame::dispatch_token`.
pub(crate) type MultiDispatchEntry = (
    String,
    Vec<Arc<FunctionDef>>,
    Vec<Value>,
    Vec<(usize, String)>,
    u64,
);

/// One entry of `MethodDispatchFrame::remaining` (ADR-0019 E9b-1/E9b-2). A
/// `Candidate` is invoked directly as a resolved method; `Wrapper` lets a
/// method's own `.wrap()` chain fold into the same `remaining` list as prefix
/// entries instead of a separate `WrapDispatchFrame` — both variants are
/// built and consumed as of E9b-2 (`class_dispatch.rs`,
/// `vm_call_method_compiled.rs`'s wrap entry sites; the lazy mid-MRO splice
/// and advance legs in `builtins_dispatch_next.rs`).
#[derive(Debug, Clone)]
pub(crate) enum DeferralEntry {
    /// A wrapper code object; invoked with `[invocant, args...]` and shifted
    /// arg sources, mirroring today's `WrapDispatchFrame` wrapper leg.
    Wrapper(Value),
    /// A user method candidate; invoked directly as a resolved method (the
    /// existing method-frame advance leg, unchanged in substance).
    Candidate {
        owner: crate::symbol::Symbol,
        // Boxed: MethodDef is ~300 bytes, which would otherwise make every
        // DeferralEntry (including the small Wrapper(Value) variant) pay that
        // size (clippy::large_enum_variant).
        def: Box<MethodDef>,
        /// Whether this entry's own method-level wrap chain (if any) has
        /// already been spliced into `remaining` as `Wrapper` prefix entries
        /// — `true` for the winner's own entry (built at frame-construction
        /// time by the two wrap entry sites) and for a mid-MRO candidate
        /// after the lazy splice (`dispatch_next_candidate`); `false` for
        /// every other MRO-tail candidate, whose wrap chain (if any) is only
        /// checked when advancement actually reaches it.
        wraps_spliced: bool,
    },
}

/// One entry of `Interpreter::samewith_context_stack` (ADR-0019 E9c-1).
/// Replaces the former dual-stack shape (`samewith_context_stack:
/// Vec<(String, Option<Value>)>` alongside a separately pushed/popped
/// `samewith_call_args_stack: Vec<Vec<Value>>`), which relied on every push
/// site pushing both stacks in lockstep by CONVENTION — several raw push
/// sites pushed only the context, leaving `samewith_call_args_stack.last()`
/// free to pair with the wrong (stale, deeper) context entry. Folding `args`
/// into the same struct as `name`/`invocant` makes that desync structurally
/// impossible: a site with no original-args carrier to attach passes `args:
/// None` for its own entry instead of silently leaving a separate stack
/// short by one.
#[derive(Debug, Clone)]
pub(crate) struct SamewithContext {
    /// The enclosing multi sub / method / proto's dispatch name.
    pub(crate) name: String,
    /// The invocant for a method dispatch; `None` for a plain sub.
    pub(crate) invocant: Option<Value>,
    /// The original call args, when this push site has them to carry
    /// (`push_method_samewith_context`); `None` otherwise — e.g. a plain sub
    /// samewith context, or a captured `gather`-body re-push, never carried
    /// args here even before this consolidation.
    pub(crate) args: Option<Vec<Value>>,
}

#[derive(Debug, Clone)]
pub(crate) struct MethodDispatchFrame {
    pub(crate) receiver_class: String,
    pub(crate) invocant: Value,
    pub(crate) args: Vec<Value>,
    pub(crate) remaining: Vec<DeferralEntry>,
    /// The FIRST (winning) candidate's scalar `is rw`/`is raw` positional params
    /// as (positional_arg_index, sigil-less_param_name). Stays fixed across the
    /// MRO chain so a `nextsame`+rw redispatch can forward the rw param's current
    /// value and route the next candidate's writeback through it (§D capstone).
    pub(crate) rw_params: Vec<(usize, String)>,
    /// ADR-0019 E9b-0: monotonic push-order token shared with `WrapDispatchFrame`/
    /// `MultiDispatchEntry`. callsame/nextsame/lastcall/nextcallee compare tokens
    /// across all three deferral stacks and pick the highest (innermost) live frame.
    pub(crate) dispatch_token: u64,
    /// ADR-0019 E9b-1/E9b-2: call-site source variable names for a wrapped
    /// method's arguments, mirroring `WrapDispatchFrame::arg_sources`.
    /// `Some` only when this frame was built at a method-wrap entry site
    /// (`class_dispatch.rs`, `vm_call_method_compiled.rs`); every other
    /// builder sets `None`. Restored (and shifted for a `Wrapper` leg's
    /// `[invocant, ...args]` call shape) so an `is rw`/sigilless parameter
    /// anywhere in the wrap chain — including the wrapped original method
    /// itself — still binds to the TRUE call-site variable
    /// (`t/wrap-invocant-arg-source.t`).
    pub(crate) arg_sources: Option<Vec<Option<String>>>,
    /// ADR-0019 E9b-2: whether the code CURRENTLY executing under this frame
    /// (the thing whose `callsame`/`callwith` call is about to run through
    /// `dispatch_next_candidate`) is a `.wrap()` wrapper BLOCK rather than a
    /// real method body. A wrapper's own positional signature is
    /// `(invocant, ...args)` (its first param is bound to SELF), so
    /// `callwith`/`nextwith`'s override args, when called from inside a
    /// wrapper, include the invocant as element 0 — the same convention the
    /// pre-E9b-2 `WrapDispatchFrame.args` used unconditionally. A real
    /// method's signature never includes the invocant positionally, so
    /// override args from a method body are invocant-EXCLUSIVE. Only
    /// `push_wrapped_method_dispatch_frame` starts this `true` (the caller
    /// always invokes the outermost wrapper directly); every other builder
    /// leaves it `false` since none of them ever enter wrapper code. Updated
    /// by `dispatch_next_candidate` immediately before each advance so a
    /// NESTED `callwith` call reads the context it is actually running in.
    pub(crate) in_wrapper: bool,
}

/// Frame for navigating through a SUB wrapper chain during callsame/callwith.
///
/// ADR-0019 E9b-2: method wraps no longer use this frame — they fold into
/// `MethodDispatchFrame::remaining` as `DeferralEntry::Wrapper` prefix
/// entries instead, so `sub_id` is now always a real (non-zero) sub id. The
/// push helper (`Interpreter::push_wrap_dispatch_frame`) asserts this.
#[derive(Debug, Clone)]
pub(crate) struct WrapDispatchFrame {
    /// The sub id being wrapped (to prevent re-entrant wrap dispatch).
    /// Always non-zero as of E9b-2 (see the struct doc comment).
    pub(crate) sub_id: u64,
    /// Remaining callables: inner wrappers then original sub. Next to call is first.
    pub(crate) remaining: Vec<Value>,
    /// Original call arguments.
    pub(crate) args: Vec<Value>,
    /// Call-site source variable names for the wrapped sub's arguments.
    /// The outermost wrapper consumes the pending arg sources when
    /// its own signature binds, so `callsame` reaching the original would
    /// otherwise see none and reject an `is rw` parameter.
    pub(crate) arg_sources: Option<Vec<Option<String>>>,
    /// ADR-0019 E9b-0: see `MethodDispatchFrame::dispatch_token`.
    pub(crate) dispatch_token: u64,
}

#[derive(Debug, Clone)]
pub(crate) struct SquishIteratorMeta {
    pub(crate) source_items: Vec<Value>,
    pub(crate) as_func: Option<Value>,
    pub(crate) with_func: Option<Value>,
    pub(crate) revert_values: HashMap<String, Value>,
    pub(crate) revert_remove: Vec<String>,
}
