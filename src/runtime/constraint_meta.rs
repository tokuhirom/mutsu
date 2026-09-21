//! The `Value` a type constraint is stored as in a variable's
//! `__mutsu_type::<name>` / `__mutsu_hash_key_type::<name>` env metadata.
//!
//! # Why this exists
//!
//! Three sites register a type constraint, and all three run *per execution*:
//! the typed-declaration op ([`crate::vm`]'s `SetVarType` family), the
//! routine-scoped declaration writer, and the parameter binder. Each of them
//! ended in `Value::str(<the constraint's text>.to_string())` — a `String` copy
//! plus the `Arc` that wraps it, so two `malloc`s and two `free`s per typed
//! declaration and per typed parameter bind, for text that the *compiler*
//! decided and that never changes again.
//!
//! The set of constraint spellings a program contains is fixed at compile time
//! and tiny (`int`, `str`, `Str`, `Any`, ... — a few dozen in the largest
//! module), while the number of registrations is unbounded: `JSON::Fast`'s
//! parse routines re-register the same handful on every token. So the value is
//! built once per *spelling* and every later registration is an `Arc` refcount
//! bump.
//!
//! This is the same memoization [`crate::runtime::meta_ns::MetaNs`] applies to
//! the metadata *key*; this module does it for the metadata *value*.
//!
//! # Why it is sound
//!
//! `Value::Str` is an immutable `Arc<String>` — a scalar variant, not one of
//! the cycle-collectable `Gc` container kinds — so a shared copy cannot be
//! mutated through, cannot participate in a cycle, and needs no GC root. The
//! `Symbol -> text` mapping is append-only and fixed for the life of the
//! process, so a cached entry can never go stale.

use crate::symbol::Symbol;
use crate::value::Value;

thread_local! {
    /// `constraint spelling -> its `Value::Str``. Per-thread rather than
    /// global so the lookup needs no lock; a spawned thread simply rebuilds
    /// the handful of entries it uses.
    static CONSTRAINT_VALUES: std::cell::RefCell<rustc_hash::FxHashMap<Symbol, Value>> =
        std::cell::RefCell::new(rustc_hash::FxHashMap::default());
}

/// The shared `Value::Str` for the constraint spelled `constraint`.
///
/// Callers store it straight into the env: it is the value every typed-lexical
/// probe (`var_type_constraint_value_sym` and friends) reads back.
pub(crate) fn constraint_meta_value(constraint: Symbol) -> Value {
    if let Some(v) = CONSTRAINT_VALUES.with(|c| c.borrow().get(&constraint).cloned()) {
        return v;
    }
    let value = Value::str(constraint.as_str().to_string());
    CONSTRAINT_VALUES.with(|c| {
        c.borrow_mut().insert(constraint, value.clone());
    });
    value
}

/// [`constraint_meta_value`] for a caller holding the constraint as text.
///
/// Interning hashes the bytes once; the alternative it replaces allocated,
/// copied and freed them on every registration.
pub(crate) fn constraint_meta_value_str(constraint: &str) -> Value {
    constraint_meta_value(Symbol::intern(constraint))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::ValueView;

    /// The point of the module: re-registering a constraint reuses the same
    /// allocation instead of making a new one. Two `Value::str` calls with the
    /// same text would give two distinct `Arc`s.
    #[test]
    fn the_same_spelling_hands_back_the_same_allocation() {
        let a = constraint_meta_value_str("Int");
        let b = constraint_meta_value_str("Int");
        let (ValueView::Str(sa), ValueView::Str(sb)) = (a.view(), b.view()) else {
            panic!("a constraint's metadata value is a Str");
        };
        assert!(
            std::ptr::eq(sa.as_str().as_ptr(), sb.as_str().as_ptr()),
            "the second registration must not allocate a fresh copy"
        );
        assert_eq!(sa.as_str(), "Int");
    }

    #[test]
    fn different_spellings_stay_distinct() {
        let int = constraint_meta_value_str("int");
        let str_ = constraint_meta_value_str("str");
        let (ValueView::Str(i), ValueView::Str(s)) = (int.view(), str_.view()) else {
            panic!("a constraint's metadata value is a Str");
        };
        assert_eq!(i.as_str(), "int");
        assert_eq!(s.as_str(), "str");
    }
}
