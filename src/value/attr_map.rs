//! `AttrMap` — the instance-attribute map (ADR-0006 §2.4).
//!
//! Instance attributes used to be a `HashMap<String, Value>`: every lookup
//! hashed the attribute name with SipHash and then `memcmp`'d it, every
//! construction heap-allocated one `String` per attribute, and every
//! `to_map()`/`commit_attrs()` snapshot cloned all of those `String`s again.
//! On `benchmarks/bench-class.raku` that showed up as 5.2% `__memcmp_avx2` plus
//! a large share of the ~20% spent in the allocator.
//!
//! The keys are now [`Symbol`]s: hashing is a `u32` through `FxHashMap`, key
//! comparison is an integer compare, and cloning a key is a `Copy`.
//!
//! To keep the blast radius of the migration sane, `AttrMap` is a newtype whose
//! inherent `get`/`insert`/`contains_key`/`remove`/`entry` accept anything that
//! implements [`AttrKey`] — a `Symbol` (the hot paths, which already hold one)
//! *or* a `&str`/`String` (the many cold construction sites: exception attrs,
//! native-type constructors, introspection). A `&str` key is interned on the
//! spot, so those sites keep their `"literal"` spelling and pay one intern
//! lookup; a `Symbol` key skips interning entirely.

use super::Value;
use crate::symbol::Symbol;
use rustc_hash::FxHashMap;
use std::collections::HashMap;
use std::collections::hash_map::Entry;

/// Anything usable as an attribute key. `Symbol` is the native (hot) form; the
/// string forms intern on the fly for cold call sites.
pub(crate) trait AttrKey {
    /// The key as a `Symbol`, interning it if necessary.
    fn into_symbol(self) -> Symbol;
    /// The key as a `Symbol` *if it is already interned*. Lookups use this so a
    /// miss on a never-interned name does not pollute the symbol table.
    fn lookup_symbol(&self) -> Option<Symbol>;
}

impl AttrKey for Symbol {
    #[inline]
    fn into_symbol(self) -> Symbol {
        self
    }
    #[inline]
    fn lookup_symbol(&self) -> Option<Symbol> {
        Some(*self)
    }
}

impl AttrKey for &Symbol {
    #[inline]
    fn into_symbol(self) -> Symbol {
        *self
    }
    #[inline]
    fn lookup_symbol(&self) -> Option<Symbol> {
        Some(**self)
    }
}

impl AttrKey for &str {
    #[inline]
    fn into_symbol(self) -> Symbol {
        Symbol::intern(self)
    }
    #[inline]
    fn lookup_symbol(&self) -> Option<Symbol> {
        Symbol::lookup(self)
    }
}

impl AttrKey for &String {
    #[inline]
    fn into_symbol(self) -> Symbol {
        Symbol::intern(self)
    }
    #[inline]
    fn lookup_symbol(&self) -> Option<Symbol> {
        Symbol::lookup(self)
    }
}

impl AttrKey for String {
    #[inline]
    fn into_symbol(self) -> Symbol {
        Symbol::intern(&self)
    }
    #[inline]
    fn lookup_symbol(&self) -> Option<Symbol> {
        Symbol::lookup(self)
    }
}

/// If `name` is an attribute-twigil variable name — scalar (`!x`/`.x`), array
/// (`@!x`/`@.x`) or hash (`%!x`/`%.x`) — return `(bare attribute name,
/// is_private)`. Excludes the bare `!`/`.` special vars and internal names. The
/// attribute cell stores attributes under the bare name, so all six twigil forms
/// of an attribute resolve to the same cell slot.
///
/// Lives here (rather than on `Interpreter`) so the compiled-code local-slot
/// attribute cache (`CompiledCode::local_attr_key`) can pre-resolve it at
/// compile time and hand the VM a ready `Symbol`.
pub(crate) fn attr_twigil_base(name: &str) -> Option<(&str, bool)> {
    // Optional `@`/`%` sigil, then the `!` (private) / `.` (public) twigil.
    let rest = name
        .strip_prefix('@')
        .or_else(|| name.strip_prefix('%'))
        .unwrap_or(name);
    let (bare, is_private) = if let Some(b) = rest.strip_prefix('!') {
        (b, true)
    } else if let Some(b) = rest.strip_prefix('.') {
        (b, false)
    } else {
        return None;
    };
    // Attribute names are ordinary identifiers (start alpha/underscore). This
    // filters out `!=`, the bare `!`/`.` special vars, and `__mutsu_` keys.
    match bare.chars().next() {
        Some(c) if c.is_alphabetic() || c == '_' => Some((bare, is_private)),
        _ => None,
    }
}

/// The attribute map of an instance: `Symbol -> Value`, hashed with `FxHash`.
#[derive(Debug, Clone, Default, PartialEq)]
pub(crate) struct AttrMap(FxHashMap<Symbol, Value>);

impl AttrMap {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub(crate) fn get<K: AttrKey>(&self, key: K) -> Option<&Value> {
        let sym = key.lookup_symbol()?;
        self.0.get(&sym)
    }

    #[inline]
    pub(crate) fn get_mut<K: AttrKey>(&mut self, key: K) -> Option<&mut Value> {
        let sym = key.lookup_symbol()?;
        self.0.get_mut(&sym)
    }

    #[inline]
    pub(crate) fn contains_key<K: AttrKey>(&self, key: K) -> bool {
        match key.lookup_symbol() {
            Some(sym) => self.0.contains_key(&sym),
            None => false,
        }
    }

    #[inline]
    pub(crate) fn insert<K: AttrKey>(&mut self, key: K, value: Value) -> Option<Value> {
        self.0.insert(key.into_symbol(), value)
    }

    #[inline]
    pub(crate) fn remove<K: AttrKey>(&mut self, key: K) -> Option<Value> {
        let sym = key.lookup_symbol()?;
        self.0.remove(&sym)
    }

    #[inline]
    pub(crate) fn entry<K: AttrKey>(&mut self, key: K) -> Entry<'_, Symbol, Value> {
        self.0.entry(key.into_symbol())
    }

    #[inline]
    pub(crate) fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub(crate) fn clear(&mut self) {
        self.0.clear();
    }

    #[inline]
    pub(crate) fn iter(&self) -> std::collections::hash_map::Iter<'_, Symbol, Value> {
        self.0.iter()
    }

    #[inline]
    pub(crate) fn keys(&self) -> std::collections::hash_map::Keys<'_, Symbol, Value> {
        self.0.keys()
    }

    #[inline]
    pub(crate) fn values(&self) -> std::collections::hash_map::Values<'_, Symbol, Value> {
        self.0.values()
    }

    #[inline]
    pub(crate) fn values_mut(
        &mut self,
    ) -> std::collections::hash_map::ValuesMut<'_, Symbol, Value> {
        self.0.values_mut()
    }

    /// Assign into the slot at `key`, writing *through* an existing
    /// `ContainerRef` cell (a `:=`-bound attribute) instead of replacing the
    /// entry — the attribute-map analogue of `Value::hash_insert_through`, which
    /// does the same for the `Value::Hash` element map.
    pub(crate) fn insert_through<K: AttrKey + Copy>(&mut self, key: K, val: Value) {
        match self.get_mut(key) {
            Some(slot) => Value::assign_element_slot(slot, val),
            None => {
                self.insert(key, val);
            }
        }
    }
}

impl Extend<(Symbol, Value)> for AttrMap {
    fn extend<T: IntoIterator<Item = (Symbol, Value)>>(&mut self, iter: T) {
        self.0.extend(iter);
    }
}

impl FromIterator<(Symbol, Value)> for AttrMap {
    fn from_iter<T: IntoIterator<Item = (Symbol, Value)>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl FromIterator<(String, Value)> for AttrMap {
    fn from_iter<T: IntoIterator<Item = (String, Value)>>(iter: T) -> Self {
        Self(
            iter.into_iter()
                .map(|(k, v)| (Symbol::intern(&k), v))
                .collect(),
        )
    }
}

impl<'a> FromIterator<(&'a str, Value)> for AttrMap {
    fn from_iter<T: IntoIterator<Item = (&'a str, Value)>>(iter: T) -> Self {
        Self(
            iter.into_iter()
                .map(|(k, v)| (Symbol::intern(k), v))
                .collect(),
        )
    }
}

impl IntoIterator for AttrMap {
    type Item = (Symbol, Value);
    type IntoIter = std::collections::hash_map::IntoIter<Symbol, Value>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> IntoIterator for &'a AttrMap {
    type Item = (&'a Symbol, &'a Value);
    type IntoIter = std::collections::hash_map::Iter<'a, Symbol, Value>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl From<HashMap<String, Value>> for AttrMap {
    fn from(map: HashMap<String, Value>) -> Self {
        map.into_iter().collect()
    }
}

impl From<&AttrMap> for HashMap<String, Value> {
    fn from(map: &AttrMap) -> Self {
        map.iter().map(|(k, v)| (k.resolve(), v.clone())).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn str_and_symbol_keys_agree() {
        let mut m = AttrMap::new();
        m.insert("alpha", Value::int(1));
        assert!(m.contains_key(Symbol::intern("alpha")));
        assert_eq!(m.get("alpha"), m.get(Symbol::intern("alpha")));
        assert_eq!(m.iter().count(), 1);
    }

    #[test]
    fn missing_str_key_does_not_intern() {
        let m = AttrMap::new();
        // A never-interned name must miss without creating a symbol for it.
        assert!(m.get("attr_map_never_interned_name_xyz").is_none());
        assert!(Symbol::lookup("attr_map_never_interned_name_xyz").is_none());
    }

    #[test]
    fn from_string_hashmap_roundtrips() {
        let mut src: HashMap<String, Value> = HashMap::new();
        src.insert("beta".to_string(), Value::int(7));
        let m: AttrMap = src.into();
        assert_eq!(m.get("beta"), Some(&Value::int(7)));
    }
}
