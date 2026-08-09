//! Stable receiver-type identity for method dispatch (ADR-0019 Phase E, box E1).
//!
//! See `docs/adr/0019-compiled-declarations-and-unified-method-dispatch.md` (Phase E
//! section) and the detailed design `todo/deep/adr0019-e1-typeid-receiver-owner.md`.
//!
//! `TypeId` is a newtype over [`Symbol`], not a dense index: dense ids were rejected
//! because the registry COW-forks per thread (a registry-owned id space would diverge
//! across forks while `Symbol` cannot), and `MethodEntryKey::owner` is already a
//! `Symbol`, so no key migration is needed. The newtype's value is the *invariant*: a
//! `TypeId` may only be produced by the E1 classifier or the E1 builtin-type catalog,
//! so holding one proves the name went through owner canonicalization (aliases folded,
//! Instance/Package resolved to the class symbol) rather than being an arbitrary
//! interned string.
//!
//! E1a (current slice) only *constructs* `TypeId`s for shadow comparison against the
//! existing string-based owner decisions — see `crate::runtime::receiver_class`. No
//! dispatch site is authoritative on this type yet (that is E1b).

use crate::symbol::Symbol;
use std::sync::OnceLock;

/// A canonicalized receiver-type identity: interned like a [`Symbol`] (so equality is
/// an O(1) integer compare, never a string compare), but only ever produced by the E1
/// classifier/catalog so that holding one is proof the name is dispatch-canonical.
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub(crate) struct TypeId(Symbol);

impl TypeId {
    /// Intern `name` and wrap it as a `TypeId`. Callers must only pass names that have
    /// already been through owner canonicalization (a catalog row's `name`, a
    /// registry-MRO `Symbol`, or a `WellKnownTypes` constant) — this constructor does
    /// not itself canonicalize anything.
    pub(crate) fn intern(name: &str) -> TypeId {
        TypeId(Symbol::intern(name))
    }

    /// Wrap an already-interned, already-canonical `Symbol` (e.g. one element of a
    /// registry `ClassDef::mro`) as a `TypeId`.
    pub(crate) fn from_symbol(sym: Symbol) -> TypeId {
        TypeId(sym)
    }

    /// The underlying interned symbol. Unused by E1a's shadow probes (which only
    /// compare names); kept for E2's `MethodEntryKey { owner: Symbol, .. }` lookups,
    /// which will want the raw symbol rather than a re-resolved string.
    #[allow(dead_code)]
    pub(crate) fn symbol(self) -> Symbol {
        self.0
    }

    /// Borrow the type name without allocating.
    pub(crate) fn as_str(self) -> &'static str {
        self.0.as_str()
    }
}

impl std::fmt::Display for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0.as_str())
    }
}

/// Lazily-interned `TypeId`s for the types the E1 classifier/catalog consult often
/// enough that a string compare (even a `Symbol`-cached one) would show up on a
/// profile. Every field is a one-time intern; comparisons against them are then a u32
/// equality check. Interning is idempotent and process-global (see [`Symbol::intern`]),
/// so this is safe to initialize once and share across threads.
// `mu`/`any`/`array`/`list` are read by the E1a classifier today
// (`crate::runtime::receiver_class`); the rest are provided per the design doc's
// declared shape for the dispatch-site cutovers that consult them next (E1b/E4), so
// they are allowed to sit unread for one slice rather than being re-added later.
#[allow(dead_code)]
pub(crate) struct WellKnownTypes {
    pub(crate) mu: TypeId,
    pub(crate) any: TypeId,
    pub(crate) cool: TypeId,
    pub(crate) array: TypeId,
    pub(crate) list: TypeId,
    pub(crate) hash: TypeId,
    pub(crate) map: TypeId,
    pub(crate) str_: TypeId,
    pub(crate) int: TypeId,
    pub(crate) num: TypeId,
    pub(crate) bool_: TypeId,
    pub(crate) code: TypeId,
    pub(crate) callable: TypeId,
}

/// Return the process-wide [`WellKnownTypes`], initializing it on first use.
pub(crate) fn well_known_types() -> &'static WellKnownTypes {
    static WK: OnceLock<WellKnownTypes> = OnceLock::new();
    WK.get_or_init(|| WellKnownTypes {
        mu: TypeId::intern("Mu"),
        any: TypeId::intern("Any"),
        cool: TypeId::intern("Cool"),
        array: TypeId::intern("Array"),
        list: TypeId::intern("List"),
        hash: TypeId::intern("Hash"),
        map: TypeId::intern("Map"),
        str_: TypeId::intern("Str"),
        int: TypeId::intern("Int"),
        num: TypeId::intern("Num"),
        bool_: TypeId::intern("Bool"),
        code: TypeId::intern("Code"),
        callable: TypeId::intern("Callable"),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn type_id_equality_is_symbol_equality() {
        let a = TypeId::intern("SomeTypeIdTestType");
        let b = TypeId::intern("SomeTypeIdTestType");
        assert_eq!(a, b);
        let c = TypeId::intern("SomeOtherTypeIdTestType");
        assert_ne!(a, c);
    }

    #[test]
    fn well_known_types_are_stable_across_calls() {
        let a = well_known_types();
        let b = well_known_types();
        assert_eq!(a.any, b.any);
        assert_eq!(a.array.as_str(), "Array");
        assert_eq!(a.mu.as_str(), "Mu");
    }

    #[test]
    fn display_shows_the_name() {
        let t = TypeId::intern("DisplayTestType");
        assert_eq!(format!("{t}"), "DisplayTestType");
    }
}
