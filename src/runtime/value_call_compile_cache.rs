//! Compile-once caches for code objects that carry only an AST body.
//!
//! A `Sub` value built from a declared routine without its bytecode (the
//! `nextcallee` method candidate, a `.^find_method` callable) is compiled on
//! the fly when it is called as a value. That compile is a pure function of
//! the body, the signature and the rw flag, so the result is memoized here,
//! keyed by the *identity* of those shared `Arc`s.
//!
//! The keys hold clones of the `Arc`s rather than their addresses: an address
//! alone could be reused by an unrelated allocation once the original value is
//! dropped, and would then hit a stale entry (the same reasoning as
//! `MapGrepCacheKey`). Both tables are bounded, because a dynamically built
//! body (an `EVAL` in a loop) is a fresh `Arc` per creation.

use super::*;
use crate::ast::{ParamDef, Stmt};
use crate::opcode::{CompiledCode, CompiledFns};
use crate::value::SubData;

/// More entries than this means the bodies are being created dynamically,
/// where no entry would be hit again anyway: the table is dropped and refilled.
const MAX_ENTRIES: usize = 1024;

/// Identity of a compile-on-the-fly `Sub`'s compiler inputs.
pub(crate) struct ValueCallCompileKey {
    body: Arc<Vec<Stmt>>,
    params: Arc<Vec<String>>,
    param_defs: Arc<Vec<ParamDef>>,
    is_rw: bool,
}

impl PartialEq for ValueCallCompileKey {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.body, &other.body)
            && Arc::ptr_eq(&self.params, &other.params)
            && Arc::ptr_eq(&self.param_defs, &other.param_defs)
            && self.is_rw == other.is_rw
    }
}

impl Eq for ValueCallCompileKey {}

impl std::hash::Hash for ValueCallCompileKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (Arc::as_ptr(&self.body) as usize).hash(state);
        (Arc::as_ptr(&self.params) as usize).hash(state);
        (Arc::as_ptr(&self.param_defs) as usize).hash(state);
        self.is_rw.hash(state);
    }
}

/// Identity of a method candidate handed out by `nextcallee`: its body and the
/// class it was declared in (which types the invocant parameter prepended to
/// the callable's signature).
pub(crate) struct NextcalleeSigKey {
    body: Arc<Vec<Stmt>>,
    owner: Symbol,
}

impl PartialEq for NextcalleeSigKey {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.body, &other.body) && self.owner == other.owner
    }
}

impl Eq for NextcalleeSigKey {}

impl std::hash::Hash for NextcalleeSigKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (Arc::as_ptr(&self.body) as usize).hash(state);
        self.owner.hash(state);
    }
}

/// The signature `nextcallee` gives a method candidate's callable, shared.
pub(crate) type NextcalleeSig = (Arc<Vec<String>>, Arc<Vec<ParamDef>>);

#[derive(Default)]
pub(crate) struct ValueCallCompileCache {
    compiled: HashMap<ValueCallCompileKey, (Arc<CompiledCode>, Arc<CompiledFns>)>,
    nextcallee_sigs: HashMap<NextcalleeSigKey, NextcalleeSig>,
}

impl Interpreter {
    /// The closure bytecode (and its nested-routine table) a compile-on-the-fly
    /// `Sub` runs, compiled on the first call and reused afterwards.
    // The keys hash and compare by `Arc` address only, so the interior
    // mutability clippy sees inside the AST (a doc slot's `OnceLock`) can
    // never move an entry.
    #[allow(clippy::mutable_key_type)]
    // Cost: O(1) amortized on a hit; O(body) for the compile on a miss.
    pub(crate) fn value_call_compiled_body(
        &mut self,
        data: &SubData,
    ) -> (Arc<CompiledCode>, Arc<CompiledFns>) {
        let key = ValueCallCompileKey {
            body: data.body.clone(),
            params: data.params.clone(),
            param_defs: data.param_defs.clone(),
            is_rw: data.is_rw || data.is_raw,
        };
        let cache = &mut self.caches.value_call_compile_cache.compiled;
        if let Some((cc, fns)) = cache.get(&key) {
            return (cc.clone(), fns.clone());
        }
        let mut compiler = crate::compiler::Compiler::new();
        // A routine closure body, so a `return` inside the routine works.
        let cc = Arc::new(compiler.compile_routine_closure_body(
            &data.params,
            &data.param_defs,
            &data.body,
            key.is_rw,
        ));
        let fns = Arc::new(compiler.take_compiled_functions());
        let cache = &mut self.caches.value_call_compile_cache.compiled;
        if cache.len() >= MAX_ENTRIES {
            cache.clear();
        }
        cache.insert(key, (cc.clone(), fns.clone()));
        (cc, fns)
    }

    /// The signature of the callable `nextcallee` returns for a method
    /// candidate: an invocant parameter typed by `owner` (unless the method
    /// declares its own) followed by the method's parameters. Built once per
    /// candidate so every callable handed out for it shares the same `Arc`s,
    /// which is what lets [`Self::value_call_compiled_body`] reuse its compile.
    // The keys hash and compare by `Arc` address only, so the interior
    // mutability clippy sees inside the AST (a doc slot's `OnceLock`) can
    // never move an entry.
    #[allow(clippy::mutable_key_type)]
    // Cost: O(1) amortized on a hit; O(params) on a miss.
    pub(crate) fn nextcallee_method_signature(
        &mut self,
        owner: Symbol,
        def: &MethodDef,
    ) -> NextcalleeSig {
        let key = NextcalleeSigKey {
            body: def.body.clone(),
            owner,
        };
        let cache = &mut self.caches.value_call_compile_cache.nextcallee_sigs;
        if let Some(sig) = cache.get(&key) {
            return sig.clone();
        }
        let has_explicit_invocant = def
            .param_defs
            .iter()
            .any(|pd| pd.is_invocant);
        let mut callable_param_defs = Vec::with_capacity(def.param_defs.len() + 1);
        if !has_explicit_invocant {
            callable_param_defs.push(Self::make_invocant_param(&owner.resolve()));
        }
        callable_param_defs.extend(def.param_defs.iter().cloned());
        let sig = (def.params.clone(), Arc::new(callable_param_defs));
        if cache.len() >= MAX_ENTRIES {
            cache.clear();
        }
        cache.insert(key, sig.clone());
        sig
    }
}
