//! A memo for [`Interpreter::resolve_token_defs`].
//!
//! Resolving a grammar rule's candidates walks the current package's MRO and,
//! for every ancestor, builds the qualified key, interns it and scans the
//! proto variants of the name. A `Grammar.parse` asks for the start rule more
//! than once and every subrule call asks again, so a three-rule parse of `xy`
//! spent ~20k of its ~139k instructions repeating the walk.
//!
//! The answer is a function of the name, the package the lookup starts from,
//! the executing routine's package (consulted when the walk finds nothing),
//! and the declarations: the token tables and the MROs. A declaration moves
//! the registry write generation (`routines_mut`/`registry_mut`), and every
//! value that generation takes is unique to one registry content
//! (`registry_gen`), so the memo is keyed by the generation and dropped
//! whenever it moves. It is per thread, so it needs no lock.

use super::*;
use std::cell::RefCell;
use std::sync::Arc;

type Key = (Symbol, Symbol, Option<Symbol>);
type Answer = Option<Arc<[Arc<FunctionDef>]>>;

thread_local! {
    /// The generation the entries were resolved under, and the entries.
    static MEMO: RefCell<(u64, rustc_hash::FxHashMap<Key, Answer>)> =
        RefCell::new((0, rustc_hash::FxHashMap::default()));
}

/// More entries than any program's grammars name: past it the memo starts
/// over rather than grow without bound.
const MAX_ENTRIES: usize = 4096;

impl Interpreter {
    /// The candidates of the token, rule or regex `name` as seen from the
    /// current package (see [`Self::resolve_token_defs_uncached`]).
    // Cost: O(1) on a memo hit (one hash probe and a copy of the candidate
    // handles); a miss is the uncached walk.
    pub(crate) fn resolve_token_defs(&self, name: &str) -> Option<Vec<Arc<FunctionDef>>> {
        // A program that declared no token has none to find (the `&name`
        // resolution of every plain sub asks, #13049).
        if self.routines().token_defs.is_empty() {
            return None;
        }
        let generation = self.registry_write_generation();
        let key = (
            Symbol::intern(name),
            self.current_package_sym(),
            self.routine_stack_top().map(|frame| frame.package),
        );
        let hit = MEMO.with(|memo| {
            let memo = memo.borrow();
            if memo.0 == generation {
                memo.1.get(&key).cloned()
            } else {
                None
            }
        });
        if let Some(answer) = hit {
            return answer.map(|defs| defs.to_vec());
        }
        let defs = self.resolve_token_defs_uncached(name);
        // A walk that declared something (it never should) is not
        // remembered past its own generation.
        if self.registry_write_generation() == generation {
            let answer: Answer = defs.as_ref().map(|defs| Arc::from(defs.as_slice()));
            MEMO.with(|memo| {
                let mut memo = memo.borrow_mut();
                if memo.0 != generation || memo.1.len() >= MAX_ENTRIES {
                    memo.0 = generation;
                    memo.1.clear();
                }
                memo.1.insert(key, answer);
            });
        }
        defs
    }
}
