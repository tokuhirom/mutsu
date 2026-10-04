//! Process-wide index from a qualified name's *member name* to every interned
//! qualified name that ends in it (#9171), plus the companion package and
//! routine-family indexes built the same way.
//!
//! A package's symbols are not stored in a per-package table: they live as
//! flat qualified keys (`P::x`, `@P::a`, `&P::f`, `Outer::P::x`, `P::f/2`)
//! spread over the env, the `our` store and the routine registry. Reading one
//! key of a package stash (`P::<$x>`) therefore used to materialize the whole
//! stash by scanning all of them.
//!
//! Every such key is a [`Symbol`], and every `Symbol` is created by exactly one
//! function, `Symbol::intern_global`, which assigns ids in an append-only
//! sequence. Folding every id of that sequence into the index (lazily, on the
//! first read after new ids appeared) makes it a guaranteed *superset* of the
//! qualified keys any store can hold, with no hook at the (many) insert sites
//! — the same argument that keeps the capture-shape registry in `symbol.rs`
//! sound. A
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
use std::sync::RwLock;

type TailMap = FxHashMap<&'static str, Vec<Symbol>>;

/// A map folded from the symbol table's append-only id sequence on demand.
///
/// Both indexes here are built lazily (#10228): almost no program reads a
/// package stash, yet every process interns thousands of qualified names at
/// startup, so maintaining either map eagerly from the intern choke point cost
/// startup instructions (~12% for the package index, ~10% for the tail index)
/// for a lookup that rarely happens. Instead a reader catches the map up:
/// `scanned` is the first id not yet folded in. Because ids are never reused
/// or remapped, the caught-up map is exactly what eager recording at
/// `Symbol::intern_global` would have built — the index is still a superset of
/// every qualified key any store can hold.
struct LazyIndex {
    map: TailMap,
    scanned: usize,
}

impl LazyIndex {
    const fn new() -> RwLock<Self> {
        RwLock::new(LazyIndex {
            map: FxHashMap::with_hasher(rustc_hash::FxBuildHasher),
            scanned: 0,
        })
    }
}

/// The member-name index: a member's bare spelling to every interned
/// qualified name that ends in it (#9171).
static TAIL_INDEX: RwLock<LazyIndex> = LazyIndex::new();

/// The companion index: a package spelling to every interned qualified name
/// that has a member under it (#9845). Keyed by each contiguous run of
/// package components, so `Outer::P::x` is found under `Outer`, `Outer::P`
/// and `P` -- the same suffix rule `stash_member_tail` applies.
static PACKAGE_INDEX: RwLock<LazyIndex> = LazyIndex::new();

/// The routine-family index: a spelling `F` to every interned qualified name
/// spelled `F/…` (#11761). A routine registry key is `F/<suffix>` for a multi
/// candidate of the family `F` = `Pkg::name` (`Pkg::name/2`,
/// `Pkg::name/1:Int`, `Pkg::name/2__m1`), so this lists a family's candidate
/// keys without scanning the registry. Keyed at every `/` of the name, so an
/// operator whose own name holds a `/` (`Pkg::infix:</>/2`) is found under
/// its family exactly as a `starts_with("F/")` test would find it.
///
/// Nearly every family has one or two names, so instead of a `Vec` per
/// spelling (an allocation per name folded in) the names form one chain per
/// spelling through a shared node list.
struct FamilyIndex {
    /// Spelling -> 1 + the index in `nodes` of its most recent name.
    heads: FxHashMap<&'static str, u32>,
    /// A name and 1 + the index of the spelling's previous one (0: none).
    nodes: Vec<(Symbol, u32)>,
    /// Position in the symbol table's slash-name list folded in so far.
    scanned: usize,
}

static FAMILY_INDEX: RwLock<FamilyIndex> = RwLock::new(FamilyIndex {
    heads: FxHashMap::with_hasher(rustc_hash::FxBuildHasher),
    nodes: Vec::new(),
    scanned: 0,
});

/// `index.map[key]` after folding every symbol interned since the previous
/// read into it with `fold`.
// Cost: O(k + m), k = names recorded under `key`, m = symbols interned since
// the previous read (amortized O(1) per symbol over the process).
fn caught_up_lookup(
    index: &RwLock<LazyIndex>,
    key: &str,
    fold: fn(&mut TailMap, Symbol, &'static str),
) -> Vec<Symbol> {
    {
        let idx = index.read().unwrap();
        if idx.scanned == crate::symbol::interned_count() {
            return idx.map.get(key).cloned().unwrap_or_default();
        }
    }
    let mut idx = index.write().unwrap();
    let LazyIndex { map, scanned } = &mut *idx;
    // Lock order: index -> symbol table read. The intern path takes the
    // table write lock and never this one, so there is no cycle.
    *scanned = crate::symbol::for_each_interned_since(*scanned, |sym, text| {
        if crate::str_scan::has_double_colon(text) {
            fold(map, sym, strip_sigil(text));
        }
    });
    map.get(key).cloned().unwrap_or_default()
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

/// Record `sym` under the bare member spelling of every split point of `body`.
// Cost: O(n), n = bytes of `body`.
fn record_members(map: &mut TailMap, sym: Symbol, body: &'static str) {
    let mut from = 0;
    while let Some(off) = body[from..].find("::") {
        from += off + 2;
        if let Some(bare) = bare_member(&body[from..]) {
            let names = map.entry(bare).or_default();
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

impl FamilyIndex {
    /// Record `sym` under every prefix of `text` that a `/` follows, the
    /// first of them at byte `slash`, with a leading sigil dropped. Each
    /// prefix is a different spelling, so no chain receives `sym` twice.
    // Cost: O(n), n = bytes of `text` after `slash`.
    fn record(&mut self, sym: Symbol, text: &'static str, slash: usize) {
        let body = strip_sigil(text);
        let skipped = text.len() - body.len();
        let mut pos = slash - skipped.min(slash);
        loop {
            if pos > 0 && body.as_bytes()[pos] == b'/' {
                let node = self.nodes.len() as u32 + 1;
                let prev = self.heads.insert(&body[..pos], node).unwrap_or(0);
                self.nodes.push((sym, prev));
            }
            match body[pos + 1..].find('/') {
                Some(off) => pos += 1 + off,
                None => break,
            }
        }
    }

    /// The names recorded under `family`, most recently interned first.
    // Cost: O(k), k = names recorded under `family`.
    fn names(&self, family: &str) -> Vec<Symbol> {
        let mut names = Vec::new();
        let mut node = self.heads.get(family).copied().unwrap_or(0);
        while node != 0 {
            let (sym, prev) = self.nodes[node as usize - 1];
            names.push(sym);
            node = prev;
        }
        names
    }
}

/// Every interned qualified name spelled `family/…` (see [`FAMILY_INDEX`]):
/// a superset of the registry keys of `family`'s multi candidates. Owned for
/// the same reason as [`names_ending_in`].
// Cost: O(k + m), k = interned names spelled `family/…`, m = symbols interned
// since the previous call (amortized O(1) per symbol over the process).
pub(crate) fn names_in_family(family: &str) -> Vec<Symbol> {
    // `caught_up_lookup`, with the catch-up narrowed to the qualified symbols
    // holding a `/`, which the symbol table lists apart: any other name is in
    // no family, so it need not be visited at all.
    {
        let idx = FAMILY_INDEX.read().unwrap();
        if idx.scanned == crate::symbol::interned_slash_count() {
            return idx.names(family);
        }
    }
    let mut idx = FAMILY_INDEX.write().unwrap();
    let idx = &mut *idx;
    // Most names hold one `/`: size for one spelling each up front, so the
    // first catch-up (every routine key interned before it) does not grow
    // the map through each power of two.
    let incoming = crate::symbol::interned_slash_count().saturating_sub(idx.scanned);
    idx.heads.reserve(incoming);
    idx.nodes.reserve(incoming);
    // Lock order: index -> symbol table read, as in `caught_up_lookup`.
    idx.scanned = crate::symbol::for_each_slash_interned_since(idx.scanned, |sym, text, slash| {
        idx.record(sym, text, slash);
    });
    idx.names(family)
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
    caught_up_lookup(&PACKAGE_INDEX, package, record_packages)
}

/// Every interned qualified name with a member spelled `bare` (see the module
/// docs for the spelling), in interning order.
///
/// Returns an owned list so no lock is held while the caller probes its
/// stores (a probe may intern, which takes the symbol-table write lock).
// Cost: O(k + m), k = interned qualified names ending in `bare`, m = symbols
// interned since the previous call (amortized O(1) per symbol) -- independent
// of the size of any env, package or registry.
pub(crate) fn names_ending_in(bare: &str) -> Vec<Symbol> {
    caught_up_lookup(&TAIL_INDEX, bare, record_members)
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
    fn tail_index_catches_up_with_later_interns() {
        // The member index is folded in lazily too: a name interned after a
        // read must still be found by the next one, exactly once.
        let early = Symbol::intern("TailLateP::tail_late_m");
        assert!(names_ending_in("tail_late_m").contains(&early));
        let late = Symbol::intern("&TailLateQ::tail_late_m/1");
        let found = names_ending_in("tail_late_m");
        assert!(found.contains(&early) && found.contains(&late));
        assert_eq!(found.iter().filter(|s| **s == late).count(), 1);
        // An unqualified spelling is never a member of anything.
        let bare = Symbol::intern("tail_late_m");
        assert!(!names_ending_in("tail_late_m").contains(&bare));
    }

    #[test]
    fn family_index_lists_every_slash_prefix() {
        let first = Symbol::intern("FamIdxP::famx/2");
        let typed = Symbol::intern("FamIdxP::famx/1:Int");
        let op = Symbol::intern("FamIdxP::infix:</>/2");
        let found = names_in_family("FamIdxP::famx");
        assert!(found.contains(&first) && found.contains(&typed));
        assert_eq!(names_in_family("FamIdxP::infix:<"), vec![op]);
        assert_eq!(names_in_family("FamIdxP::infix:</>"), vec![op]);
        // A name interned after a read is found by the next one, once.
        let late = Symbol::intern("FamIdxP::famx/3");
        let found = names_in_family("FamIdxP::famx");
        assert_eq!(found.iter().filter(|s| **s == late).count(), 1);
        assert!(names_in_family("FamIdxP::fam").is_empty());
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
