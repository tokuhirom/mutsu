//! Package-qualified names, classified and built once per pair.
//!
//! # Why this module exists
//!
//! A package-qualified name is a *derived* name: `Pkg::thing` is a function of
//! the package and the thing, both of which the caller already holds. The
//! obvious way to get one is `format!("{pkg}::{name}")`, and the obvious way to
//! ask whether a name is qualified at all is `name.contains("::")`. Both do at
//! run time what the source text decided once.
//!
//! That is the same finding [`crate::runtime::meta_ns`] records for
//! `__mutsu_*` metadata keys, in a different namespace, and it is measurably
//! larger. Profiling one `JSON::Fast` decode (issue
//! [#8898](https://github.com/tokuhirom/mutsu/issues/8898)):
//!
//! | | inclusive share of one decode |
//! |---|---:|
//! | `resolve_type_in_current_package` | 5.11% (0.48% of the program in `format!` alone) |
//! | `running_module_bareword` | 1.54% (+0.77% `format!`) |
//! | `set_our_var` | 1.23% |
//! | `resolve_type_name_for_owner` | 1.16% (+0.45% `format!`) |
//!
//! `<core::str::pattern::StrSearcher>::new` — the `"::"` searches — was
//! constructed 1,390,603 times in a ten-decode run, and its top callers are
//! exactly those functions. `Symbol::intern` is 4.34% inclusive, largely
//! re-interning names that were just built.
//!
//! # What it does
//!
//! Symbols are global and append-only: an id, once assigned to a string, is
//! never reused or remapped. So every question below has one answer for the
//! life of the process, and every one of them is memoized per thread:
//!
//! - [`qualified`] builds `Pkg::name` once per `(package, name)` pair.
//! - [`package_parent`] strips one trailing `::` segment, once per package.
//! - [`package_ancestors`] walks that chain without allocating, which is the
//!   shape every "try this name in each enclosing package" loop wants.
//! - [`is_qualified`] and [`is_global_package`] classify a name once.
//!
//! # The gate
//!
//! `scripts/check-name-scans.sh` is a shrinking ratchet over the call sites
//! that still do this by hand, modelled on `scripts/check-magic-keys.sh` and
//! for the same reason: the memoized helpers existing is not enough, because
//! nothing stops the next site being written. `src/parser/` and
//! `src/compiler/` are exempt — deciding what a name *is* from its text is
//! precisely their job, and doing it there is the point.

use crate::symbol::{Symbol, wk};

/// `<pkg>::<name>` as an interned `Symbol`, built once per pair.
///
/// Use [`Symbol::as_str`] on the result for the `&str`-keyed APIs; it hands
/// back the interner's own `&'static str` and allocates nothing.
pub(crate) fn qualified(pkg: Symbol, name: Symbol) -> Symbol {
    thread_local! {
        static PAIRS: std::cell::RefCell<rustc_hash::FxHashMap<(Symbol, Symbol), Symbol>> =
            std::cell::RefCell::new(rustc_hash::FxHashMap::default());
    }
    let pair = (pkg, name);
    if let Some(sym) = PAIRS.with(|c| c.borrow().get(&pair).copied()) {
        return sym;
    }
    let sym = Symbol::intern(&format!("{}::{}", pkg.as_str(), name.as_str()));
    PAIRS.with(|c| {
        c.borrow_mut().insert(pair, sym);
    });
    sym
}

/// [`qualified`] for a caller holding only `&str`s. Interns both operands
/// before the memo lookup, so it costs two string hashes per call — the wrong
/// entry point for a hot loop, right where the caller genuinely has no symbol.
pub(crate) fn qualified_strs(pkg: &str, name: &str) -> Symbol {
    qualified(Symbol::intern(pkg), Symbol::intern(name))
}

/// `pkg` with its last `::` segment removed, or `None` when it has only one.
///
/// This is `rsplit_once("::")` decided once per package rather than per walk
/// step: a type lookup walks the whole enclosing chain, and every lookup for
/// the same package walks the same chain.
pub(crate) fn package_parent(pkg: Symbol) -> Option<Symbol> {
    thread_local! {
        static PARENTS: std::cell::RefCell<rustc_hash::FxHashMap<Symbol, Option<Symbol>>> =
            std::cell::RefCell::new(rustc_hash::FxHashMap::default());
    }
    if let Some(parent) = PARENTS.with(|c| c.borrow().get(&pkg).copied()) {
        return parent;
    }
    let parent = pkg
        .as_str()
        .rsplit_once("::")
        .map(|(head, _)| Symbol::intern(head));
    PARENTS.with(|c| {
        c.borrow_mut().insert(pkg, parent);
    });
    parent
}

/// `pkg` and each enclosing package in turn: `Foo::Bar::Baz`, `Foo::Bar`,
/// `Foo`. Every step is a memo hit, and the walk allocates nothing.
///
/// The empty package yields nothing, so a loop over this is already the
/// `while !pkg.is_empty()` the hand-written walks open with.
pub(crate) fn package_ancestors(pkg: Symbol) -> PackageAncestors {
    PackageAncestors { next: Some(pkg) }
}

/// The iterator [`package_ancestors`] answers.
pub(crate) struct PackageAncestors {
    next: Option<Symbol>,
}

impl Iterator for PackageAncestors {
    type Item = Symbol;

    fn next(&mut self) -> Option<Symbol> {
        let cur = self.next?;
        if cur.as_str().is_empty() {
            self.next = None;
            return None;
        }
        self.next = package_parent(cur);
        Some(cur)
    }
}

/// Whether `name` carries a `::` qualifier, decided once per symbol.
///
/// Backed by a flag table indexed by symbol id rather than a map: ids are
/// dense and assigned in order, so the table is a `Vec` push in the worst case
/// and a bounds-checked byte read otherwise.
pub(crate) fn is_qualified(name: Symbol) -> bool {
    thread_local! {
        // 0 = not yet classified, 1 = unqualified, 2 = qualified.
        static FLAGS: std::cell::RefCell<Vec<u8>> = const { std::cell::RefCell::new(Vec::new()) };
    }
    let idx = name.id() as usize;
    if let Some(flag) = FLAGS.with(|c| c.borrow().get(idx).copied())
        && flag != 0
    {
        return flag == 2;
    }
    let qualified = name.as_str().contains("::");
    FLAGS.with(|c| {
        let mut flags = c.borrow_mut();
        if flags.len() <= idx {
            flags.resize(idx + 1, 0);
        }
        flags[idx] = if qualified { 2 } else { 1 };
    });
    qualified
}

/// Whether `pkg` names no package at all — unset, or the default top-level
/// `GLOBAL`.
///
/// Two id compares. The spelling this replaces — `pkg.is_empty() || pkg ==
/// "GLOBAL"` against a `current_package()` clone — took the interpreter's
/// package `RwLock` and put the name on the heap to compare it against two
/// literals.
#[inline]
pub(crate) fn is_global_package(pkg: Symbol) -> bool {
    pkg == wk::empty_package() || pkg == wk::global_package()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_pair_is_built_once_and_spelled_the_way_format_spelled_it() {
        let pkg = Symbol::intern("Foo::Bar");
        let name = Symbol::intern("Baz");
        let first = qualified(pkg, name);
        assert_eq!(first.as_str(), "Foo::Bar::Baz");
        // The memo hands back the identical symbol, not an equal rebuild.
        assert_eq!(first.id(), qualified(pkg, name).id());
        assert_eq!(first.id(), qualified_strs("Foo::Bar", "Baz").id());
    }

    #[test]
    fn ancestors_walk_outwards_and_stop() {
        let walk: Vec<&str> = package_ancestors(Symbol::intern("A::B::C"))
            .map(|s| s.as_str())
            .collect();
        assert_eq!(walk, ["A::B::C", "A::B", "A"]);
        assert_eq!(package_ancestors(Symbol::intern("A")).count(), 1);
        assert_eq!(package_ancestors(wk::empty_package()).count(), 0);
    }

    #[test]
    fn classification_matches_the_string_form_it_replaces() {
        for name in ["A::B", "A", "", "::", "A::B::C", "a-b"] {
            let sym = Symbol::intern(name);
            assert_eq!(is_qualified(sym), name.contains("::"), "{name:?}");
            // Second call takes the memo, and must answer the same.
            assert_eq!(is_qualified(sym), name.contains("::"), "{name:?} memoized");
        }
    }

    #[test]
    fn only_the_two_no_package_spellings_are_global() {
        assert!(is_global_package(Symbol::intern("")));
        assert!(is_global_package(Symbol::intern("GLOBAL")));
        assert!(!is_global_package(Symbol::intern("GLOBALish")));
        assert!(!is_global_package(Symbol::intern("Foo")));
    }
}
