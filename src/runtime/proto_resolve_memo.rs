//! The bare-name `proto` lookups of the call path, remembered per search
//! context.
//!
//! A call of a routine that has a `proto` asks two questions about it on every
//! call: whether a proto exists ([`Interpreter::has_proto`]) and, for a
//! trivial (`{*}`-only) proto, which definition it is
//! ([`Interpreter::resolve_proto_function_shared`]). Both walk the bare-name
//! search packages and build a qualified key per package. The by-name
//! `has_proto` cache in `vm_call_dispatch.rs` answered the first one only for
//! a name no `use` had imported: a family imported from a module (every
//! exported `multi` with an explicit `proto`, such as
//! `Array::Sorted::Util`'s `inserts`/`finds`/`insert`) is visible per
//! compunit (#11004), and that cache's key does not say which compunit asks,
//! so those calls ran the uncached walk each time — about 4.8k instructions
//! for `has_proto` and 4k for the proto resolution, several times per call
//! (#15786).
//!
//! The memo here answers both, keyed by everything the answer is a function
//! of:
//!
//! - the search context: the current package and the lexical package
//!   [`Interpreter::bare_name_packages_syms`] derives the search list from
//!   (the candidate scope's while a candidate is matched, else the frame's);
//! - the name;
//! - for a name with import records, the units that decide visibility
//!   ([`Interpreter::operator_candidate_visible`]): the current unit, the
//!   executing unit and the import table's generation.
//!
//! The registry it reads (the proto markers and the proto definitions) is
//! covered by the registry write generation, which moves on every
//! `registry_mut()` — the only path to a registry write — so an entry cannot
//! outlive a declaration, an import's registration or a restore.

use super::*;
use crate::symbol::Symbol;
use std::sync::Arc;

/// `(current package, lexical package, name, deciding units)`: see the
/// module docs. The units are `None` for a name with no import records.
type ProtoMemoKey = (Symbol, Option<Symbol>, Symbol, Option<(Symbol, Symbol, u64)>);

/// The two memo tables and the registry write generation they were filled
/// under.
#[derive(Default)]
pub(crate) struct ProtoResolveMemo {
    generation: u64,
    has_proto: rustc_hash::FxHashMap<ProtoMemoKey, bool>,
    proto_def: rustc_hash::FxHashMap<ProtoMemoKey, Option<Arc<FunctionDef>>>,
}

impl Interpreter {
    /// The memo key for the unqualified `name_sym` in the running code's
    /// search context, after dropping the tables if the registry moved.
    // Cost: O(d), d = routine-stack depth (the executing unit), only for a
    // name with import records; O(1) otherwise.
    fn proto_memo_key(&mut self, name_sym: Symbol) -> ProtoMemoKey {
        let generation = self.registry_write_generation();
        let memo = &mut self.caches.proto_resolve_memo;
        if memo.generation != generation {
            memo.has_proto.clear();
            memo.proto_def.clear();
            memo.generation = generation;
        }
        let lexical = self.candidate_scope_lexical_package().or_else(|| {
            self.routine_stack
                .last()
                .and_then(|frame| frame.lexical_package)
        });
        let units = self.operator_has_import_scope_sym(name_sym).then(|| {
            (
                self.current_unit,
                self.executing_unit_sym(),
                self.module.operator_import_gen,
            )
        });
        (self.current_package_sym(), lexical, name_sym, units)
    }

    /// [`Self::has_proto`] for an unqualified name, from the memo described
    /// in the module docs. A qualified name is answered uncached.
    // Cost: O(1) expected on a hit; `has_proto` on a miss.
    pub(crate) fn has_proto_memoized(&mut self, name: &str, name_sym: Symbol) -> bool {
        if crate::qualified::is_qualified(name_sym) {
            return self.has_proto(name);
        }
        let key = self.proto_memo_key(name_sym);
        if let Some(&hit) = self.caches.proto_resolve_memo.has_proto.get(&key) {
            return hit;
        }
        let answer = self.has_proto(name);
        self.caches.proto_resolve_memo.has_proto.insert(key, answer);
        answer
    }

    /// [`Self::resolve_proto_function_shared`] from the memo described in
    /// the module docs. A qualified name is answered uncached.
    // Cost: O(1) expected on a hit (plus one intern of `name`);
    // `resolve_proto_function_shared` on a miss.
    pub(crate) fn resolve_proto_function_memoized(
        &mut self,
        name: &str,
    ) -> Option<Arc<FunctionDef>> {
        let name_sym = Symbol::intern(name);
        if crate::qualified::is_qualified(name_sym) {
            return self.resolve_proto_function_shared(name);
        }
        let key = self.proto_memo_key(name_sym);
        if let Some(hit) = self.caches.proto_resolve_memo.proto_def.get(&key) {
            return hit.clone();
        }
        let answer = self.resolve_proto_function_shared(name);
        self.caches
            .proto_resolve_memo
            .proto_def
            .insert(key, answer.clone());
        answer
    }
}
