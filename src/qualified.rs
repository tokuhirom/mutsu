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
//! - [`is_qualified`], [`is_routine_scoped_package`] and [`is_global_package`]
//!   classify a name once.
//! - [`unqualified_part`] strips a qualifier back off, once per name.
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

/// Everything a symbol's TEXT decides, classified on first ask and kept in a
/// flag table indexed by symbol id.
///
/// A table rather than a map because ids are dense and assigned in order: a
/// `Vec` push in the worst case, a bounds-checked byte read otherwise.
mod flags {
    use crate::symbol::Symbol;

    pub(super) const CLASSIFIED: u8 = 1 << 0;
    /// The name carries a `::` qualifier.
    pub(super) const QUALIFIED: u8 = 1 << 1;
    /// The "package" is a routine-scope mangled name (`Pkg::&sub/arity`),
    /// which is not a package name at all and must not be walked as one.
    pub(super) const ROUTINE_SCOPED: u8 = 1 << 2;

    pub(super) fn of(sym: Symbol) -> u8 {
        thread_local! {
            static TABLE: std::cell::RefCell<Vec<u8>> = const { std::cell::RefCell::new(Vec::new()) };
        }
        let idx = sym.id() as usize;
        if let Some(f) = TABLE.with(|c| c.borrow().get(idx).copied())
            && f & CLASSIFIED != 0
        {
            return f;
        }
        let text = sym.as_str();
        let mut f = CLASSIFIED;
        if text.contains("::") {
            f |= QUALIFIED;
        }
        if crate::runtime::utils::has_routine_scope_marker(text) {
            f |= ROUTINE_SCOPED;
        }
        TABLE.with(|c| {
            let mut table = c.borrow_mut();
            if table.len() <= idx {
                table.resize(idx + 1, 0);
            }
            table[idx] = f;
        });
        f
    }
}

/// Whether `name` carries a `::` qualifier, decided once per symbol.
pub(crate) fn is_qualified(name: Symbol) -> bool {
    flags::of(name) & flags::QUALIFIED != 0
}

/// Whether `pkg` is a routine-scope mangled package name (`Pkg::&sub/arity`,
/// used for nested subs) rather than a real package, decided once per symbol.
///
/// Such a name must not be walked as a package chain, which is why every site
/// that reads `current_package` for a chain walk asks this too.
pub(crate) fn is_routine_scoped_package(pkg: Symbol) -> bool {
    flags::of(pkg) & flags::ROUTINE_SCOPED != 0
}

/// `key` with its package qualifier removed, keeping any sigil:
/// `$Foo::Bar::x` -> `$x`, `x` -> `x`. Built once per symbol.
///
/// The `our` store's unqualified-name index derives one of these for every
/// variable it records, and the hand-written form allocated a fresh `String`
/// to do it — 354,298 of the 1,390,603 `StrSearcher` constructions in a
/// ten-decode `JSON::Fast` profile came from that one derivation.
pub(crate) fn unqualified_part(key: Symbol) -> Symbol {
    thread_local! {
        static BARE: std::cell::RefCell<rustc_hash::FxHashMap<Symbol, Symbol>> =
            std::cell::RefCell::new(rustc_hash::FxHashMap::default());
    }
    if let Some(sym) = BARE.with(|c| c.borrow().get(&key).copied()) {
        return sym;
    }
    let text = key.as_str();
    let (sigil, rest) = match text.as_bytes().first() {
        Some(b'$' | b'@' | b'%' | b'&') => text.split_at(1),
        _ => ("", text),
    };
    let bare = rest.rsplit("::").next().unwrap_or(rest);
    // The common case is an already-unqualified, sigil-less name, where the
    // answer is the key itself and there is nothing to intern.
    let sym = if sigil.is_empty() && bare.len() == text.len() {
        key
    } else {
        Symbol::intern(&format!("{sigil}{bare}"))
    };
    BARE.with(|c| {
        c.borrow_mut().insert(key, sym);
    });
    sym
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
        // ...including when the pair is reached from freshly interned copies.
        assert_eq!(
            first.id(),
            qualified(Symbol::intern("Foo::Bar"), Symbol::intern("Baz")).id()
        );
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
    fn a_routine_scope_mangled_package_is_not_a_package() {
        for pkg in ["Foo::&bar/2", "::&x", "Foo::Bar", "Foo", "", "&bar"] {
            let sym = Symbol::intern(pkg);
            let want = crate::runtime::utils::has_routine_scope_marker(pkg);
            assert_eq!(is_routine_scoped_package(sym), want, "{pkg:?}");
            // Second call takes the memo, and must answer the same.
            assert_eq!(is_routine_scoped_package(sym), want, "{pkg:?} memoized");
            // The two classifications share one table entry and must not
            // overwrite each other.
            assert_eq!(is_qualified(sym), pkg.contains("::"), "{pkg:?} qualified");
        }
    }

    #[test]
    fn the_unqualified_part_matches_the_string_form_it_replaces() {
        fn by_hand(key: &str) -> String {
            let (sigil, rest) = match key.as_bytes().first() {
                Some(b'$' | b'@' | b'%' | b'&') => key.split_at(1),
                _ => ("", key),
            };
            let bare = rest.rsplit("::").next().unwrap_or(rest);
            format!("{sigil}{bare}")
        }
        for key in [
            "$Foo::Bar::x",
            "x",
            "@Foo::a",
            "%h",
            "&Foo::f",
            "",
            "Foo::",
            "::x",
        ] {
            let sym = Symbol::intern(key);
            assert_eq!(unqualified_part(sym).as_str(), by_hand(key), "{key:?}");
            // Second call takes the memo, and must answer the same.
            assert_eq!(
                unqualified_part(sym).as_str(),
                by_hand(key),
                "{key:?} memoized"
            );
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
