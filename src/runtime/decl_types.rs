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

#[derive(Clone, Default)]
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
    pub(crate) methods: HashMap<String, Vec<MethodDef>>, // name -> overloads
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
    /// Body statements deferred until composition time (for parameterized roles).
    /// These are non-method/non-attribute statements that may reference type parameters
    /// and must be re-executed for each class composition with concrete type bindings.
    pub(crate) deferred_body_stmts: Vec<Stmt>,
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
}

/// Invocant context for an active `proto method` `{*}` dispatch.
#[derive(Debug, Clone)]
pub(crate) struct ProtoMethodCtx {
    pub(crate) invocant: Value,
}

/// One entry of `multi_dispatch_stack`: (function_name, remaining_candidates,
/// original_args, first_candidate_rw_params). See the field doc on
/// `Interpreter::multi_dispatch_stack`.
pub(crate) type MultiDispatchEntry = (
    String,
    Vec<Arc<FunctionDef>>,
    Vec<Value>,
    Vec<(usize, String)>,
);

#[derive(Debug, Clone)]
pub(crate) struct MethodDispatchFrame {
    pub(crate) receiver_class: String,
    pub(crate) invocant: Value,
    pub(crate) args: Vec<Value>,
    pub(crate) remaining: Vec<(String, MethodDef)>,
    /// The FIRST (winning) candidate's scalar `is rw`/`is raw` positional params
    /// as (positional_arg_index, sigil-less_param_name). Stays fixed across the
    /// MRO chain so a `nextsame`+rw redispatch can forward the rw param's current
    /// value and route the next candidate's writeback through it (§D capstone).
    pub(crate) rw_params: Vec<(usize, String)>,
}

/// Frame for navigating through wrapper chain during callsame/callwith.
#[derive(Debug, Clone)]
pub(crate) struct WrapDispatchFrame {
    /// The sub id being wrapped (to prevent re-entrant wrap dispatch).
    pub(crate) sub_id: u64,
    /// Remaining callables: inner wrappers then original sub. Next to call is first.
    pub(crate) remaining: Vec<Value>,
    /// Original call arguments.
    pub(crate) args: Vec<Value>,
}

#[derive(Debug, Clone)]
pub(crate) struct SquishIteratorMeta {
    pub(crate) source_items: Vec<Value>,
    pub(crate) as_func: Option<Value>,
    pub(crate) with_func: Option<Value>,
    pub(crate) revert_values: HashMap<String, Value>,
    pub(crate) revert_remove: Vec<String>,
}
