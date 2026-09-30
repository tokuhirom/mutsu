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

/// The companion index: a package spelling to every interned qualified name
/// that has a member under it (#9845). Keyed by each contiguous run of
/// package components, so `Outer::P::x` is found under `Outer`, `Outer::P`
/// and `P` -- the same suffix rule `stash_member_tail` applies.
///
/// Built lazily (#10228): almost no program reads a package stash, yet every
/// process interns thousands of qualified names at startup, so maintaining
/// this map from [`record`] cost ~12% of startup instructions for a lookup
/// that rarely happens. Instead [`names_under_package`] catches the map up
/// from the symbol table's append-only id sequence: `scanned` is the first id
/// not yet folded in. Because ids are never reused or remapped, the caught-up
/// map is exactly what eager recording would have built.
struct PackageIndex {
    map: TailMap,
    scanned: usize,
}

static PACKAGE_INDEX: OnceLock<RwLock<PackageIndex>> = OnceLock::new();

fn package_index() -> &'static RwLock<PackageIndex> {
    PACKAGE_INDEX.get_or_init(|| {
        RwLock::new(PackageIndex {
            map: FxHashMap::default(),
            scanned: 0,
        })
    })
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

/// Record `sym` under every package spelling `body` names a member of: each
/// contiguous run of its components that stops before the last one.
// Cost: O(c^2 + n), c = `::` components of `body`, n = its bytes.
fn record_packages(map: &mut TailMap, sym: Symbol, body: &'static str) {
    let mut start = 0;
    loop {
        let mut from = start;
        while let Some(off) = body[from..].find("::") {
            let end = from + off;
            let package = &body[start..end];
            if !package.is_empty() {
                let names = map.entry(package).or_default();
                if names.last() != Some(&sym) {
                    names.push(sym);
                }
            }
            from = end + 2;
        }
        match body[start..].find("::") {
            Some(off) => start += off + 2,
            None => break,
        }
    }
}

/// Every interned qualified name with a member under the package spelled
/// `package` (see [`PACKAGE_INDEX`]), in interning order. Owned for the same
/// reason as [`names_ending_in`].
///
/// The first call folds in every symbol interned so far; later calls fold in
/// only the ones interned since.
// Cost: O(k + m), k = interned qualified names under `package`, m = symbols
// interned since the previous call (amortized O(1) per symbol over the process).
pub(crate) fn names_under_package(package: &str) -> Vec<Symbol> {
    let index = package_index();
    {
        let idx = index.read().unwrap();
        if idx.scanned == crate::symbol::interned_count() {
            return idx.map.get(package).cloned().unwrap_or_default();
        }
    }
    let mut idx = index.write().unwrap();
    let PackageIndex { map, scanned } = &mut *idx;
    // Lock order: package index -> symbol table read. The intern path takes
    // the table write lock and never this one, so there is no cycle.
    *scanned = crate::symbol::for_each_interned_since(*scanned, |sym, text| {
        if text.contains("::") {
            record_packages(map, sym, strip_sigil(text));
        }
    });
    map.get(package).cloned().unwrap_or_default()
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

    #[test]
    fn indexes_every_package_run() {
        let sym = Symbol::intern("&PkgIdxOuter::PkgIdxP::pkgx/2");
        for package in ["PkgIdxOuter", "PkgIdxOuter::PkgIdxP", "PkgIdxP"] {
            assert!(names_under_package(package).contains(&sym), "{package}");
        }
        assert!(names_under_package("PkgIdxP::pkgx/2").is_empty());
        assert!(names_under_package("pkgx").is_empty());
    }

    #[test]
    fn package_index_catches_up_with_later_interns() {
        // The first read builds the index; a name interned afterwards must
        // still be found by the next read (#10228).
        let early = Symbol::intern("$PkgLateRoot::early");
        assert!(names_under_package("PkgLateRoot").contains(&early));
        let late = Symbol::intern("@PkgLateRoot::PkgLateSub::late");
        let found = names_under_package("PkgLateRoot");
        assert!(found.contains(&early) && found.contains(&late));
        assert_eq!(names_under_package("PkgLateSub"), vec![late]);
        // Each name is recorded once, however often the index catches up.
        assert_eq!(found.iter().filter(|s| **s == late).count(), 1);
    }
}
