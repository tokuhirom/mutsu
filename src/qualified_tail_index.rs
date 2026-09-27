//! Process-wide index from a qualified name's *member name* to every interned
//! qualified name that ends in it (#9171).
//!
//! A package's symbols are not stored in a per-package table: they live as
//! flat qualified keys (`P::x`, `@P::a`, `&P::f`, `Outer::P::x`, `P::f/2`)
//! spread over the env, the `our` store and the routine registry. Reading one
//! key of a package stash (`P::<$x>`) therefore used to materialize the whole
//! stash by scanning all of them.
//!
//! Every such key is a [`Symbol`], and every `Symbol` is created by exactly one
//! function, `Symbol::intern_global`. Recording each qualified name here at
//! that choke point makes this index a guaranteed *superset* of the qualified
//! keys any store can hold, with no hook at the (many) insert sites — the same
//! argument that keeps the capture-shape registry in `symbol.rs` sound. A
//! consumer asks for the names ending in `x` and probes only those, then
//! re-checks each one exactly as the full scan would; a name that was interned
//! but never stored (or has since been removed) is simply a probe that misses.
//!
//! The index is keyed by the member's *bare* spelling: the text after a `::`
//! with any leading sigil dropped and anything from the first `/` on (a
//! routine registry key's signature suffix, `P::f/2`) cut off. Every split
//! point is recorded, so `Outer::P::x` is found both as a member `x` of
//! `Outer::P` and — through the suffix rule `stash_member_tail` applies — of
//! `P`. A spelling that still contains `::` names a sub-package, not a member,
//! and is not recorded.

use crate::symbol::Symbol;
use rustc_hash::FxHashMap;
use std::sync::{OnceLock, RwLock};

type TailMap = FxHashMap<&'static str, Vec<Symbol>>;

static TAIL_INDEX: OnceLock<RwLock<TailMap>> = OnceLock::new();

fn tail_index() -> &'static RwLock<TailMap> {
    TAIL_INDEX.get_or_init(|| RwLock::new(FxHashMap::default()))
}

fn strip_sigil(s: &str) -> &str {
    s.strip_prefix(['$', '@', '%', '&']).unwrap_or(s)
}

/// The bare member spelling a split-point `rest` is indexed under, or `None`
/// when `rest` names a sub-package (it is still qualified) or nothing.
fn bare_member(rest: &'static str) -> Option<&'static str> {
    let rest = strip_sigil(rest);
    let bare = rest.split('/').next().unwrap_or(rest);
    (!bare.is_empty() && !bare.contains("::")).then_some(bare)
}

/// Record a newly interned name. Called once per symbol, from the single place
/// a symbol id is assigned, so it must intern nothing.
// Cost: O(n), n = bytes of `text`; paid once per qualified symbol, ever.
pub(crate) fn record(sym: Symbol, text: &'static str) {
    if !text.contains("::") {
        return;
    }
    let body = strip_sigil(text);
    let mut index = tail_index().write().unwrap();
    let mut from = 0;
    while let Some(off) = body[from..].find("::") {
        from += off + 2;
        if let Some(bare) = bare_member(&body[from..]) {
            let names = index.entry(bare).or_default();
            if names.last() != Some(&sym) {
                names.push(sym);
            }
        }
    }
}

/// Every interned qualified name with a member spelled `bare` (see the module
/// docs for the spelling), in interning order.
///
/// Returns an owned list so no lock is held while the caller probes its
/// stores (a probe may intern, which takes the symbol-table write lock).
// Cost: O(k), k = interned qualified names ending in `bare` -- independent of the
// size of any env, package or registry.
pub(crate) fn names_ending_in(bare: &str) -> Vec<Symbol> {
    tail_index()
        .read()
        .unwrap()
        .get(bare)
        .cloned()
        .unwrap_or_default()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn indexes_every_split_point_and_form() {
        let names = [
            "TailIdxP::tailx",
            "@TailIdxP::tailx",
            "TailIdxOuter::TailIdxP::$tailx",
            "TailIdxP::tailx/2",
            "TailIdxP::Sub::other",
        ];
        let syms: Vec<Symbol> = names.iter().map(|n| Symbol::intern(n)).collect();
        let found = names_ending_in("tailx");
        for sym in &syms[..4] {
            assert!(found.contains(sym), "{} not indexed", sym.as_str());
        }
        assert!(!found.contains(&syms[4]));
        // A sub-package spelling is never a member.
        assert!(!names_ending_in("Sub::other").contains(&syms[4]));
        assert!(names_ending_in("other").contains(&syms[4]));
    }
}
