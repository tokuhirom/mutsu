use super::*;

impl ArrayKind {
    /// True for `Array`, `ItemArray`, `Shaped`, and `Lazy` (the `[...]` constructor or shaped declaration).
    pub fn is_real_array(self) -> bool {
        matches!(
            self,
            ArrayKind::Array | ArrayKind::ItemArray | ArrayKind::Shaped | ArrayKind::Lazy
        )
    }

    /// True for lazy arrays backed by an infinite source.
    pub fn is_lazy(self) -> bool {
        matches!(self, ArrayKind::Lazy)
    }

    /// True for `ItemList` and `ItemArray` (Scalar-wrapped).
    pub fn is_itemized(self) -> bool {
        matches!(self, ArrayKind::ItemList | ArrayKind::ItemArray)
    }

    /// Wrap in a Scalar container (`.item`).
    pub fn itemize(self) -> Self {
        match self {
            ArrayKind::List => ArrayKind::ItemList,
            ArrayKind::Array => ArrayKind::ItemArray,
            other => other,
        }
    }

    /// Remove Scalar wrapper (decontainerize).
    /// Strips itemization only: `ItemArray → Array`, `ItemList → List`.
    /// Non-itemized kinds are unchanged: `Array → Array`, `List → List`.
    pub fn decontainerize(self) -> Self {
        match self {
            ArrayKind::ItemArray => ArrayKind::Array,
            ArrayKind::ItemList => ArrayKind::List,
            other => other,
        }
    }
}

impl HashData {
    pub fn new(map: HashMap<String, Value>) -> Self {
        HashData {
            map,
            value_type: None,
            key_type: None,
            declared_type: None,
            original_keys: None,
            default: None,
            itemized: false,
        }
    }

    /// Whether any container metadata is attached (type or object-hash keys).
    pub fn has_meta(&self) -> bool {
        self.has_type_meta() || self.original_keys.is_some()
    }

    /// Whether container *type* metadata (element/key/declared type) is attached.
    /// This is the authoritative replacement for the `hash_type_metadata` side
    /// table: a freshly-built hash literal has `false` here, so it can never
    /// inherit a stale typed-hash entry via Arc-pointer reuse.
    pub fn has_type_meta(&self) -> bool {
        self.value_type.is_some() || self.key_type.is_some() || self.declared_type.is_some()
    }

    /// Clear all container *type* metadata (used when re-tagging in place).
    pub fn clear_type_meta(&mut self) {
        self.value_type = None;
        self.key_type = None;
        self.declared_type = None;
    }

    /// Whether reading this hash's entries should yield typed (original) keys
    /// rather than plain `Str` keys. Only object hashes (`my %h{KeyType}`,
    /// marked by `key_type`) and hashes coerced from a Set/Bag/Mix (tagged with
    /// the `__mutsu_setty_origin` marker) preserve typed keys. A *plain* hash
    /// always reports `Str` keys even if construction speculatively recorded a
    /// typed key (e.g. the `Int` 1 from `my %h = 1..6`), because Raku hash keys
    /// are always stringified.
    pub fn has_typed_keys(&self) -> bool {
        self.original_keys.as_ref().is_some_and(|orig| {
            !orig.is_empty()
                && (self.key_type.is_some() || orig.contains_key("__mutsu_setty_origin"))
        })
    }

    /// Get the original (typed) key Value for a stored string key. For object
    /// hashes (`my %h{Int}`) the stored key is a `.WHICH` string (e.g.
    /// `"Int|1"`); this returns the real key object (`Int(1)`). Plain hashes
    /// report a `Str`. Mirrors `BagData::typed_key`. Honors [`Self::has_typed_keys`]:
    /// a plain hash's speculative `original_keys` is ignored.
    pub fn typed_key(&self, str_key: &str) -> Value {
        if self.has_typed_keys()
            && let Some(ref orig) = self.original_keys
            && let Some(v) = orig.get(str_key)
        {
            return v.clone();
        }
        Value::Str(Arc::new(str_key.to_string()))
    }

    /// Build a `Pair` for a hash entry, using the original typed key for object
    /// hashes. Plain string keys yield `Value::Pair`; non-string typed keys
    /// (object hashes) yield `Value::ValuePair`. Behaviour-neutral for plain
    /// hashes (returns `Value::Pair(str_key, value)` as before).
    /// Build a `Pair` for a hash entry `(str_key, value)`, honoring object-hash
    /// typed keys. The value is decontainerized: a hash element is stored as a
    /// `ContainerRef` cell, but the pair must carry the inner value (matching a
    /// `%h<k>` read and `.values`) — otherwise iterating a hash as pairs leaks
    /// the cell and `+`/`.elems` on the pair value misbehave (the cell counts as
    /// a single scalar item). See t/bind-hash-value-pairs.t.
    pub fn typed_pair(&self, str_key: &str, value: Value) -> Value {
        let value = value.deref_container();
        match self.typed_key(str_key) {
            Value::Str(s) => Value::Pair((*s).clone(), Box::new(value)),
            other => Value::ValuePair(Box::new(other), Box::new(value)),
        }
    }
}

impl std::ops::Deref for HashData {
    type Target = HashMap<String, Value>;
    fn deref(&self) -> &HashMap<String, Value> {
        &self.map
    }
}

impl std::ops::DerefMut for HashData {
    fn deref_mut(&mut self) -> &mut HashMap<String, Value> {
        &mut self.map
    }
}

impl From<HashMap<String, Value>> for HashData {
    fn from(map: HashMap<String, Value>) -> Self {
        HashData::new(map)
    }
}

/// Hash equality ignores container metadata — only the key/value map matters
/// (preserves the prior `Arc<HashMap>` PartialEq semantics).
impl PartialEq for HashData {
    fn eq(&self, other: &Self) -> bool {
        self.map == other.map
    }
}

impl ArrayData {
    pub fn new(items: Vec<Value>) -> Self {
        ArrayData {
            items,
            value_type: None,
            key_type: None,
            declared_type: None,
            default: None,
            shape: None,
        }
    }

    /// Whether container *type* metadata (element/key/declared type) is attached.
    pub fn has_type_meta(&self) -> bool {
        self.value_type.is_some() || self.key_type.is_some() || self.declared_type.is_some()
    }
}

impl std::ops::Deref for ArrayData {
    type Target = Vec<Value>;
    fn deref(&self) -> &Vec<Value> {
        &self.items
    }
}

impl std::ops::DerefMut for ArrayData {
    fn deref_mut(&mut self) -> &mut Vec<Value> {
        &mut self.items
    }
}

impl From<Vec<Value>> for ArrayData {
    fn from(items: Vec<Value>) -> Self {
        ArrayData::new(items)
    }
}

/// Array equality ignores container metadata — only the elements matter
/// (preserves the prior `Arc<Vec<Value>>` PartialEq semantics).
impl PartialEq for ArrayData {
    fn eq(&self, other: &Self) -> bool {
        self.items == other.items
    }
}

impl FromIterator<Value> for ArrayData {
    fn from_iter<I: IntoIterator<Item = Value>>(iter: I) -> Self {
        ArrayData::new(iter.into_iter().collect())
    }
}

impl<'a> IntoIterator for &'a ArrayData {
    type Item = &'a Value;
    type IntoIter = std::slice::Iter<'a, Value>;
    fn into_iter(self) -> Self::IntoIter {
        self.items.iter()
    }
}
