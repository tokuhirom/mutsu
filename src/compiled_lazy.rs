//! Routine bodies that are decoded, and adapted to their `FunctionDef`, on first use
//! (ADR-12026 §2.2).
//!
//! A precompilation hit used to decode every routine of the module and clone each one
//! into its `FunctionDef` at registration, although a program calls a handful. Two
//! handles defer that work:
//!
//! - [`LazyFn`] is one slot of a [`CompiledFns`](crate::opcode::CompiledFns) table. A
//!   slot decoded from the cache holds the raw bytes of its body until something asks
//!   for the `CompiledFunction`.
//! - [`RoutineBody`] is `FunctionDef.compiled`: a [`LazyFn`] plus the signature
//!   facts of the def it is installed as. The first reader gets the adapted body.
//!
//! Both materialize at most once, so the `Arc<CompiledFunction>` a per-call-site inline
//! cache (ADR-0066) keys on is stable from the first read on. The facts registration
//! needs without a body travel beside the bytes (see [`LazyFn::has_nested_exports`]).

use crate::opcode::CompiledFunction;
use crate::symbol::Symbol;
use std::sync::{Arc, OnceLock};

/// The still-encoded body of a [`LazyFn`] and the symbol table it was encoded with.
struct RawBody {
    bytes: Box<[u8]>,
    symbols: Arc<[Symbol]>,
}

/// One routine of a [`CompiledFns`](crate::opcode::CompiledFns) table, decoded on first
/// use.
pub(crate) struct LazyFn {
    cell: OnceLock<Arc<CompiledFunction>>,
    raw: Option<RawBody>,
    /// Recorded beside the encoded body, so registration can skip the body of a
    /// routine that exports nothing nested. `None` for a routine that was never
    /// encoded: its answer is read off the body.
    nested_exports: Option<bool>,
}

impl std::fmt::Debug for LazyFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.cell.get() {
            Some(cf) => cf.fmt(f),
            None => f.write_str("LazyFn(<encoded>)"),
        }
    }
}

impl LazyFn {
    /// A slot whose body is already in hand.
    // Cost: O(1).
    pub(crate) fn ready(cf: Arc<CompiledFunction>) -> Self {
        let cell = OnceLock::new();
        let _ = cell.set(cf);
        Self {
            cell,
            raw: None,
            nested_exports: None,
        }
    }

    /// A shared slot around a body that is already in hand.
    // Cost: O(1).
    pub(crate) fn owned(cf: Arc<CompiledFunction>) -> Arc<Self> {
        Arc::new(Self::ready(cf))
    }

    /// A slot that decodes `bytes` (written by `encode_nested`) on first use.
    // Cost: O(1).
    pub(crate) fn encoded(bytes: Vec<u8>, symbols: Arc<[Symbol]>, nested_exports: bool) -> Self {
        Self {
            cell: OnceLock::new(),
            raw: Some(RawBody {
                bytes: bytes.into_boxed_slice(),
                symbols,
            }),
            nested_exports: Some(nested_exports),
        }
    }

    /// The body, decoding it on the first call.
    ///
    /// A cache entry is written by the binary that reads it, so a body that does not
    /// decode means the file was damaged after the whole entry was accepted.
    // Cost: O(n) on the first call, n = size of the body; O(1) after.
    pub(crate) fn get(&self) -> &Arc<CompiledFunction> {
        self.cell.get_or_init(|| {
            let raw = self
                .raw
                .as_ref()
                .expect("a LazyFn without a body is always ready");
            match crate::precomp_codec::decode_body::<CompiledFunction>(&raw.bytes, &raw.symbols) {
                Ok(cf) => Arc::new(cf),
                Err(e) => panic!(
                    "the precompilation cache holds a damaged routine body ({e}); \
                     remove the cache directory and run again"
                ),
            }
        })
    }

    /// Whether the body has been decoded.
    #[cfg(test)]
    pub(crate) fn is_decoded(&self) -> bool {
        self.cell.get().is_some()
    }

    /// Whether the body declares an `is export` routine nested in it
    /// ([`CompiledFunction::has_nested_export_plans`]), answered without decoding the
    /// body when the cache recorded it.
    // Cost: O(1) for a recorded slot; O(p) otherwise, p = sub declaration plans.
    pub(crate) fn has_nested_exports(&self) -> bool {
        match self.nested_exports {
            Some(known) => known,
            None => self.get().has_nested_export_plans(),
        }
    }

    /// Mutable access to the body of a slot nobody else shares.
    // Cost: O(n) when the body is decoded here, n = size of the body.
    pub(crate) fn body_mut(&mut self) -> &mut Arc<CompiledFunction> {
        self.get();
        self.nested_exports = None;
        self.raw = None;
        self.cell.get_mut().expect("decoded just above")
    }
}

/// The signature facts of the `FunctionDef` a plan-compiled body is installed as.
///
/// The compiler emits one `CompiledFunction` per declared signature, but
/// registration derives the authoritative signature (normalized `param_defs`, auto
/// `@_`/`%_`, empty-signature and rw/raw flags), so the installed bytecode takes its
/// signature-derived data from the def and re-precomputes everything keyed on it.
#[derive(Debug)]
pub(crate) struct AdaptInputs {
    pub(crate) params: Vec<String>,
    pub(crate) param_defs: Vec<crate::ast::ParamDef>,
    pub(crate) return_type: Option<String>,
    pub(crate) empty_sig: bool,
    pub(crate) is_rw: bool,
    pub(crate) is_raw: bool,
    pub(crate) source_file: Option<String>,
}

impl AdaptInputs {
    // Cost: O(p), p = size of the signature.
    pub(crate) fn of(def: &crate::ast::FunctionDef) -> Self {
        Self {
            params: def.params.clone(),
            param_defs: def.param_defs.clone(),
            return_type: def.return_type.clone(),
            empty_sig: def.empty_sig,
            is_rw: def.is_rw,
            is_raw: def.is_raw,
            source_file: def.source_file.clone(),
        }
    }

    // Cost: O(n), n = size of the signature plus the nested routine tables.
    fn adapt(&self, compiled: &CompiledFunction) -> Arc<CompiledFunction> {
        let mut adapted = compiled.clone();
        adapted.params.clone_from(&self.params);
        adapted.param_defs.clone_from(&self.param_defs);
        adapted.return_type.clone_from(&self.return_type);
        adapted.empty_sig = self.empty_sig;
        adapted.is_rw = self.is_rw;
        adapted.is_raw = self.is_raw;
        adapted.source_file.clone_from(&self.source_file);
        adapted.stamp_source_file(self.source_file.clone());
        // `captured_fatal_mode` (#9521) is already correct on `compiled` -- baked in
        // at compile time (`Compiler::fatal_pragma_active`) -- and the clone above
        // carries it over unchanged. It must NOT be re-captured here from the live
        // interpreter: a HOISTED registration (`hoist_sub_decls`) executes before the
        // declaring statement's own `use fatal;` (if any, earlier in the same scope)
        // has run, and would wrongly bake in `false`.
        adapted.precompute_param_local_slots();
        adapted.precompute_named_call_plan();
        adapted.precompute_param_name_syms();
        Arc::new(adapted)
    }
}

/// `FunctionDef.compiled`: the bytecode of a registered routine, adapted to the def
/// on first use.
#[derive(Debug)]
pub(crate) struct RoutineBody {
    source: Arc<LazyFn>,
    /// `None` installs the compiled body as it is (a proto's `{*}` body).
    adapt: Option<AdaptInputs>,
    adapted: OnceLock<Arc<CompiledFunction>>,
}

impl RoutineBody {
    /// A body installed as `source` compiled it.
    // Cost: O(1).
    pub(crate) fn plain(source: Arc<LazyFn>) -> Self {
        Self {
            source,
            adapt: None,
            adapted: OnceLock::new(),
        }
    }

    /// A body adapted to `inputs` when first read.
    // Cost: O(1).
    pub(crate) fn adapted_to(source: Arc<LazyFn>, inputs: AdaptInputs) -> Self {
        Self {
            source,
            adapt: Some(inputs),
            adapted: OnceLock::new(),
        }
    }

    /// The adapted body, producing it on the first call.
    // Cost: O(n) on the first call, n = size of the body's metadata; O(1) after.
    pub(crate) fn get(&self) -> &Arc<CompiledFunction> {
        match &self.adapt {
            None => self.source.get(),
            Some(inputs) => self.adapted.get_or_init(|| inputs.adapt(self.source.get())),
        }
    }
}
