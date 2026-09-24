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
use crate::value::ValueMap;
use rustc_hash::FxHashMap;
use std::collections::HashMap;
use std::sync::Arc;

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
    // Optional `@`/`%`/`&` sigil, then the `!` (private) / `.` (public) twigil.
    let rest = name
        .strip_prefix('@')
        .or_else(|| name.strip_prefix('%'))
        .or_else(|| name.strip_prefix('&'))
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

/// Return the variable sigil carried by an attribute twigil. A missing
/// container sigil denotes the scalar form (`$!x`/`$.x`); `@` and `%` retain
/// their container kind. This is separate from [`attr_twigil_base`] because
/// most callers only need privacy, while the instance store must distinguish
/// colliding scalar and container attributes with the same bare name.
pub(crate) fn attr_twigil_sigil(name: &str) -> Option<char> {
    let sigil = match name.as_bytes() {
        [b'@', ..] => '@',
        [b'%', ..] => '%',
        [b'&', ..] => '&',
        _ => '$',
    };
    attr_twigil_base(name).map(|_| sigil)
}

/// Attribute under which `Str.WHICH` keeps its invocant (see
/// [`AttrMap::objat_which`]).
pub(crate) const OBJAT_STR_PAYLOAD: &str = "__str_which";

/// The declared-attribute layout of one composed class (ADR-0121 D2): the
/// storage key of every attribute its instances carry, in MRO order (parents
/// first), and the key -> slot index. Built once per class from the
/// constructor plan and shared by every instance constructed from it.
///
/// A layout is immutable. A class whose shape changes after instances exist
/// (`augment`, a late role composition) gets a NEW layout with a new
/// [`ClassLayout::id`]; the instances already built keep the old one, which
/// stays correct for them.
#[derive(Debug)]
pub(crate) struct ClassLayout {
    id: u32,
    keys: Box<[Symbol]>,
    index: FxHashMap<Symbol, u32>,
    /// Bare names some key of this layout qualifies (`Owner\0bare`,
    /// `Owner\0<sigil>bare`): a private attribute declared in both a parent
    /// and a child, or a sigil-colliding one. Which key an access to such a
    /// name picks depends on the running method's owner, so a per-site cache
    /// keyed by layout alone must not remember it.
    qualified_bares: rustc_hash::FxHashSet<Symbol>,
}

impl ClassLayout {
    /// A layout over `keys`, in order. A key listed twice keeps its first
    /// slot.
    pub(crate) fn new(keys: impl IntoIterator<Item = Symbol>) -> Self {
        static NEXT_ID: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(1);
        let mut ordered = Vec::new();
        let mut index = FxHashMap::default();
        let mut qualified_bares = rustc_hash::FxHashSet::default();
        for key in keys {
            if let std::collections::hash_map::Entry::Vacant(e) = index.entry(key) {
                e.insert(ordered.len() as u32);
                ordered.push(key);
                if let Some((_, rest)) = key.as_str().split_once('\0') {
                    let bare = rest.strip_prefix(['$', '@', '%', '&']).unwrap_or(rest);
                    qualified_bares.insert(Symbol::intern(bare));
                }
            }
        }
        Self {
            id: NEXT_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
            keys: ordered.into_boxed_slice(),
            index,
            qualified_bares,
        }
    }

    /// The slot a per-site cache may remember for an access to the attribute
    /// `bare` that resolved to storage key `key`, or `None` when the choice of
    /// `key` could differ for another access through the same site on this
    /// layout.
    ///
    /// The key resolution tries, in order, the owner-qualified private keys
    /// (private access only), `bare`, and the sigil-prefixed key. A cached slot
    /// is only sound if every candidate ranked above `key` can never be a
    /// declared slot of this layout: a declared slot that is merely absent now
    /// could be filled later and would then win. Candidates in the undeclared
    /// overflow are the caller's to rule out, per access (see
    /// [`AttrMap::has_undeclared`]).
    // Cost: O(1).
    pub(crate) fn site_cacheable_slot(
        &self,
        key: Symbol,
        bare: Symbol,
        is_private: bool,
    ) -> Option<usize> {
        let slot = self.slot_of(key)?;
        if is_private && self.qualified_bares.contains(&bare) {
            return None;
        }
        if key != bare && self.slot_of(bare).is_some() {
            return None;
        }
        (slot <= u32::MAX as usize).then_some(slot)
    }

    /// The storage key of declared slot `slot`.
    #[inline]
    pub(crate) fn key_at(&self, slot: usize) -> Symbol {
        self.keys[slot]
    }

    /// This layout's identity: distinct for every layout ever built, so a
    /// cache keyed by it can never confuse two class shapes.
    #[inline]
    pub(crate) fn id(&self) -> u32 {
        self.id
    }

    /// The slot of `key`, if the layout declares it.
    // Cost: O(1), one hash probe of an interned id.
    #[inline]
    pub(crate) fn slot_of(&self, key: Symbol) -> Option<usize> {
        self.index.get(&key).map(|&i| i as usize)
    }

    /// Number of declared slots.
    #[inline]
    pub(crate) fn len(&self) -> usize {
        self.keys.len()
    }
}

/// The attribute map of an instance: `Symbol -> Value`.
///
/// An instance built from a class's constructor plan carries that class's
/// [`ClassLayout`]: its declared attributes live in `slots` (one per layout
/// key, `None` while the attribute is absent), and anything else -- an
/// attribute of a builtin base the registry does not know, an internal
/// marker -- lives in `extra`. A map with no layout keeps everything in
/// `extra`, which is exactly the old representation.
///
/// Every operation stays key-based, so callers are unaffected by where a key
/// lives. What changes observably is iteration order: slots in declaration
/// order first, then `extra`. (The hash order it replaces was arbitrary.)
#[derive(Debug, Clone, Default)]
pub(crate) struct AttrMap {
    layout: Option<Arc<ClassLayout>>,
    slots: Vec<Option<Value>>,
    extra: FxHashMap<Symbol, Value>,
}

impl PartialEq for AttrMap {
    /// Map equality: the same keys with equal values, wherever they live.
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len() && self.iter().all(|(k, v)| other.get(*k) == Some(v))
    }
}

/// A view of one key of an [`AttrMap`], the `hash_map::Entry` subset callers
/// use: a declared slot, or an entry of the undeclared overflow.
pub(crate) enum AttrEntry<'a> {
    Slot(&'a mut Option<Value>),
    Extra(std::collections::hash_map::Entry<'a, Symbol, Value>),
}

impl<'a> AttrEntry<'a> {
    pub(crate) fn or_insert(self, value: Value) -> &'a mut Value {
        match self {
            AttrEntry::Slot(slot) => slot.get_or_insert(value),
            AttrEntry::Extra(e) => e.or_insert(value),
        }
    }

    pub(crate) fn or_insert_with(self, f: impl FnOnce() -> Value) -> &'a mut Value {
        match self {
            AttrEntry::Slot(slot) => slot.get_or_insert_with(f),
            AttrEntry::Extra(e) => e.or_insert_with(f),
        }
    }
}

impl AttrMap {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    /// An empty map over `layout`: every declared slot present but absent.
    pub(crate) fn with_layout(layout: Arc<ClassLayout>) -> Self {
        Self {
            slots: vec![None; layout.len()],
            layout: Some(layout),
            extra: FxHashMap::default(),
        }
    }

    /// The layout this map's declared attributes are laid out by, if any.
    #[inline]
    pub(crate) fn layout(&self) -> Option<&Arc<ClassLayout>> {
        self.layout.as_ref()
    }

    /// Whether any attribute lives outside the layout's declared slots.
    #[inline]
    pub(crate) fn has_undeclared(&self) -> bool {
        !self.extra.is_empty()
    }

    /// The value in declared slot `slot`, `None` when absent. A caller that
    /// resolved `slot` against [`Self::layout`] reads it with no hashing.
    // Cost: O(1).
    #[inline]
    pub(crate) fn slot(&self, slot: usize) -> Option<&Value> {
        self.slots.get(slot).and_then(Option::as_ref)
    }

    /// Mutable form of [`Self::slot`].
    #[inline]
    pub(crate) fn slot_mut(&mut self, slot: usize) -> Option<&mut Value> {
        self.slots.get_mut(slot).and_then(Option::as_mut)
    }

    #[inline]
    fn slot_index(&self, sym: Symbol) -> Option<usize> {
        self.layout.as_ref().and_then(|l| l.slot_of(sym))
    }

    #[inline]
    pub(crate) fn get<K: AttrKey>(&self, key: K) -> Option<&Value> {
        let sym = key.lookup_symbol()?;
        match self.slot_index(sym) {
            Some(slot) => self.slots[slot].as_ref(),
            None => self.extra.get(&sym),
        }
    }

    /// The identity text of an `ObjAt` / `ValueObjAt` instance. `Str.WHICH`
    /// stores its invocant under [`OBJAT_STR_PAYLOAD`] (shared, O(1)) instead of
    /// a pre-rendered `Str|...` key, so the text is assembled here, only when
    /// something actually reads it; every other ObjAt carries a `WHICH` Str.
    // Cost: O(n), n = chars of the identity text.
    pub(crate) fn objat_which(&self) -> Option<String> {
        if let Some(payload) = self.get(OBJAT_STR_PAYLOAD) {
            return Some(format!("Str|{}", payload.string_value_cow()));
        }
        self.get("WHICH").map(|v| v.to_string_value())
    }

    #[inline]
    pub(crate) fn get_mut<K: AttrKey>(&mut self, key: K) -> Option<&mut Value> {
        let sym = key.lookup_symbol()?;
        match self.slot_index(sym) {
            Some(slot) => self.slots[slot].as_mut(),
            None => self.extra.get_mut(&sym),
        }
    }

    #[inline]
    pub(crate) fn contains_key<K: AttrKey>(&self, key: K) -> bool {
        self.get(key).is_some()
    }

    #[inline]
    pub(crate) fn insert<K: AttrKey>(&mut self, key: K, value: Value) -> Option<Value> {
        let sym = key.into_symbol();
        match self.slot_index(sym) {
            Some(slot) => self.slots[slot].replace(value),
            None => self.extra.insert(sym, value),
        }
    }

    #[inline]
    pub(crate) fn remove<K: AttrKey>(&mut self, key: K) -> Option<Value> {
        let sym = key.lookup_symbol()?;
        match self.slot_index(sym) {
            Some(slot) => self.slots[slot].take(),
            None => self.extra.remove(&sym),
        }
    }

    #[inline]
    pub(crate) fn entry<K: AttrKey>(&mut self, key: K) -> AttrEntry<'_> {
        let key = key.into_symbol();
        match self.slot_index(key) {
            Some(slot) => AttrEntry::Slot(&mut self.slots[slot]),
            None => AttrEntry::Extra(self.extra.entry(key)),
        }
    }

    /// Number of attributes present.
    // Cost: O(s), s = declared slots.
    pub(crate) fn len(&self) -> usize {
        self.slots.iter().filter(|v| v.is_some()).count() + self.extra.len()
    }

    #[inline]
    pub(crate) fn is_empty(&self) -> bool {
        self.extra.is_empty() && self.slots.iter().all(Option::is_none)
    }

    pub(crate) fn clear(&mut self) {
        self.slots.iter_mut().for_each(|v| *v = None);
        self.extra.clear();
    }

    /// The present attributes: declared slots in layout order, then the
    /// undeclared ones.
    pub(crate) fn iter(&self) -> impl Iterator<Item = (&Symbol, &Value)> + '_ {
        let keys: &[Symbol] = self.layout.as_ref().map_or(&[], |l| &l.keys);
        keys.iter()
            .zip(self.slots.iter())
            .filter_map(|(k, v)| v.as_ref().map(|v| (k, v)))
            .chain(self.extra.iter())
    }

    #[inline]
    pub(crate) fn keys(&self) -> impl Iterator<Item = &Symbol> + '_ {
        self.iter().map(|(k, _)| k)
    }

    #[inline]
    pub(crate) fn values(&self) -> impl Iterator<Item = &Value> + '_ {
        self.iter().map(|(_, v)| v)
    }

    pub(crate) fn values_mut(&mut self) -> impl Iterator<Item = &mut Value> + '_ {
        self.slots
            .iter_mut()
            .filter_map(Option::as_mut)
            .chain(self.extra.values_mut())
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

    /// The boxed-word before-image of this map; see [`AttrBits`].
    pub(crate) fn bits_image(&self) -> AttrBits {
        AttrBits(self.iter().map(|(k, v)| (*k, v.nanbox_bits())).collect())
    }
}

/// The boxed-word image of an [`AttrMap`] — the before-image a delta commit
/// diffs against (see [`super::InstanceAttrs::commit_attrs_delta`]).
///
/// Why not just keep a cloned `AttrMap` as the before-image: cloning the map
/// clones every `Value`, which for the pointer variants means a refcount bump
/// per attribute on a path that already pays one full clone (the working copy
/// handed to the native method). The *only* question the diff asks of the
/// before-image is "is the value still the same boxed word", and that is one
/// `u64` per key — no `Value` clones, one allocation.
///
/// Safe against address reuse (an ABA on a pointer variant) because the
/// working copy the handler holds keeps every snapshotted value alive until the
/// commit: the addresses in here cannot be recycled in between.
#[derive(Default)]
pub(crate) struct AttrBits(FxHashMap<Symbol, u64>);

impl AttrBits {
    /// The boxed word `key` held when the image was taken, or `None` if the key
    /// was absent. Equal bits mean the entry was never touched.
    #[inline]
    pub(crate) fn bits(&self, key: Symbol) -> Option<u64> {
        self.0.get(&key).copied()
    }

    #[inline]
    pub(crate) fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub(crate) fn keys(&self) -> std::collections::hash_map::Keys<'_, Symbol, u64> {
        self.0.keys()
    }
}

impl Extend<(Symbol, Value)> for AttrMap {
    fn extend<T: IntoIterator<Item = (Symbol, Value)>>(&mut self, iter: T) {
        for (k, v) in iter {
            self.insert(k, v);
        }
    }
}

impl FromIterator<(Symbol, Value)> for AttrMap {
    fn from_iter<T: IntoIterator<Item = (Symbol, Value)>>(iter: T) -> Self {
        Self {
            extra: iter.into_iter().collect(),
            ..Self::default()
        }
    }
}

impl FromIterator<(String, Value)> for AttrMap {
    fn from_iter<T: IntoIterator<Item = (String, Value)>>(iter: T) -> Self {
        iter.into_iter()
            .map(|(k, v)| (Symbol::intern(&k), v))
            .collect()
    }
}

impl<'a> FromIterator<(&'a str, Value)> for AttrMap {
    fn from_iter<T: IntoIterator<Item = (&'a str, Value)>>(iter: T) -> Self {
        iter.into_iter()
            .map(|(k, v)| (Symbol::intern(k), v))
            .collect()
    }
}

/// Keeps a declared slot's `(key, value)` when the slot is present.
type PresentSlot = fn((Symbol, Option<Value>)) -> Option<(Symbol, Value)>;

impl IntoIterator for AttrMap {
    type Item = (Symbol, Value);
    type IntoIter = std::iter::Chain<
        std::iter::FilterMap<
            std::iter::Zip<std::vec::IntoIter<Symbol>, std::vec::IntoIter<Option<Value>>>,
            PresentSlot,
        >,
        std::collections::hash_map::IntoIter<Symbol, Value>,
    >;
    fn into_iter(self) -> Self::IntoIter {
        let keys: Vec<Symbol> = self
            .layout
            .as_ref()
            .map_or_else(Vec::new, |l| l.keys.to_vec());
        let present: PresentSlot = |(k, v)| v.map(|v| (k, v));
        keys.into_iter()
            .zip(self.slots)
            .filter_map(present)
            .chain(self.extra)
    }
}

impl<'a> IntoIterator for &'a AttrMap {
    type Item = (&'a Symbol, &'a Value);
    type IntoIter = Box<dyn Iterator<Item = (&'a Symbol, &'a Value)> + 'a>;
    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.iter())
    }
}

/// Hasher-agnostic: the cold construction sites build their attribute map with
/// whichever `String`-keyed map is handy (std's, [`ValueMap`]'s seeded fast
/// hasher, `FxHashMap`), and all of them intern into the same `Symbol` keys here.
impl<S: std::hash::BuildHasher> From<HashMap<String, Value, S>> for AttrMap {
    fn from(map: HashMap<String, Value, S>) -> Self {
        map.into_iter().collect()
    }
}

impl From<&AttrMap> for ValueMap {
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
        let mut src: ValueMap = ValueMap::default();
        src.insert("beta".to_string(), Value::int(7));
        let m: AttrMap = src.into();
        assert_eq!(m.get("beta"), Some(&Value::int(7)));
    }
}
