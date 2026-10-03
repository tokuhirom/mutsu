//! Package-qualified *variable* names: the sigiled counterparts of
//! [`super::qualified`], memoized the same way.
//!
//! A variable key keeps its sigil in front of the package (`$P::x`, `@P::a`),
//! so it cannot be built by joining the package and the name with `::`; the
//! hand-written form re-split the sigil off and `format!`ted the key on every
//! free-variable read. Both directions are a function of the symbols alone,
//! so each is decided once per symbol (or pair).

use crate::symbol::Symbol;

/// `name` qualified by `pkg` with its sigil kept in front:
/// `($x, P)` -> `$P::x`, `(@a, P)` -> `@P::a`, `(x, P)` -> `P::x`.
/// Built once per pair.
// Cost: O(1) amortized (one memo probe; the first call per pair interns).
pub(crate) fn qualified_var(pkg: Symbol, name: Symbol) -> Symbol {
    thread_local! {
        static PAIRS: std::cell::RefCell<rustc_hash::FxHashMap<(Symbol, Symbol), Symbol>> =
            std::cell::RefCell::new(rustc_hash::FxHashMap::default());
    }
    let pair = (pkg, name);
    if let Some(sym) = PAIRS.with(|c| c.borrow().get(&pair).copied()) {
        return sym;
    }
    let text = name.as_str();
    let sym = match text.as_bytes().first() {
        Some(b'$' | b'@' | b'%' | b'&') => {
            let (sigil, rest) = text.split_at(1);
            Symbol::intern(&format!("{sigil}{}::{rest}", pkg.as_str()))
        }
        _ => super::qualified(pkg, name),
    };
    PAIRS.with(|c| {
        c.borrow_mut().insert(pair, sym);
    });
    sym
}

/// A qualified variable name split at its last `::`. Every part borrows the
/// interner's own `&'static str`, so a split allocates nothing.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct QualifiedVar {
    /// The leading sigil (`$`, `@`, `%` or `&`), or `""` for a sigil-less key.
    pub(crate) sigil: &'static str,
    /// Everything between the sigil and the last `::` (may be empty: `$::x`).
    pub(crate) pkg: &'static str,
    /// The last segment.
    pub(crate) bare: &'static str,
}

/// `name` split into sigil, package and last segment, or `None` when it has
/// no `::` qualifier: `@Foo::Bar::a` -> (`@`, `Foo::Bar`, `a`). Decided once
/// per symbol.
// Cost: O(1) amortized (one memo probe; the first call per symbol scans it).
pub(crate) fn split_qualified_var(name: Symbol) -> Option<QualifiedVar> {
    thread_local! {
        static SPLITS: std::cell::RefCell<rustc_hash::FxHashMap<Symbol, Option<QualifiedVar>>> =
            std::cell::RefCell::new(rustc_hash::FxHashMap::default());
    }
    if !super::is_qualified(name) {
        return None;
    }
    if let Some(split) = SPLITS.with(|c| c.borrow().get(&name).copied()) {
        return split;
    }
    let text: &'static str = name.as_str();
    let (sigil, rest) = match text.as_bytes().first() {
        Some(b'$' | b'@' | b'%' | b'&') => text.split_at(1),
        _ => ("", text),
    };
    let split = rest
        .rsplit_once("::")
        .map(|(pkg, bare)| QualifiedVar { sigil, pkg, bare });
    SPLITS.with(|c| {
        c.borrow_mut().insert(name, split);
    });
    split
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn qualified_var_keeps_the_sigil_in_front() {
        let pkg = Symbol::intern("Foo::Bar");
        for (name, want) in [
            ("$x", "$Foo::Bar::x"),
            ("@a", "@Foo::Bar::a"),
            ("%h", "%Foo::Bar::h"),
            ("&f", "&Foo::Bar::f"),
            ("x", "Foo::Bar::x"),
        ] {
            let got = qualified_var(pkg, Symbol::intern(name));
            assert_eq!(got.as_str(), want, "{name}");
            // The memo hands back the identical symbol.
            assert_eq!(got.id(), qualified_var(pkg, Symbol::intern(name)).id());
        }
    }

    #[test]
    fn split_is_the_inverse_at_the_last_qualifier() {
        let split = |s: &str| split_qualified_var(Symbol::intern(s));
        assert_eq!(
            split("@Foo::Bar::a"),
            Some(QualifiedVar {
                sigil: "@",
                pkg: "Foo::Bar",
                bare: "a"
            })
        );
        assert_eq!(
            split("P::x"),
            Some(QualifiedVar {
                sigil: "",
                pkg: "P",
                bare: "x"
            })
        );
        assert_eq!(
            split("$::x"),
            Some(QualifiedVar {
                sigil: "$",
                pkg: "",
                bare: "x"
            })
        );
        assert_eq!(split("$x"), None);
        assert_eq!(split("x"), None);
        // Second call takes the memo, and must answer the same.
        assert_eq!(split("x"), None);
        assert_eq!(split("P::x").map(|q| q.bare), Some("x"));
    }
}
