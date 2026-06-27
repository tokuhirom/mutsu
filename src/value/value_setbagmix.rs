use super::*;

impl BagData {
    pub fn new(counts: HashMap<String, NumBigInt>) -> Self {
        BagData {
            counts,
            original_keys: None,
            value_type: None,
            key_type: None,
            declared_type: None,
        }
    }

    pub fn with_original_keys(
        counts: HashMap<String, NumBigInt>,
        original_keys: HashMap<String, Value>,
    ) -> Self {
        BagData {
            counts,
            original_keys: Some(original_keys),
            value_type: None,
            key_type: None,
            declared_type: None,
        }
    }

    /// Whether container *type* metadata (element/key/declared type) is attached.
    pub fn has_type_meta(&self) -> bool {
        self.value_type.is_some() || self.key_type.is_some() || self.declared_type.is_some()
    }

    /// Get the original Value for a key, falling back to Str.
    pub fn typed_key(&self, str_key: &str) -> Value {
        if let Some(ref orig) = self.original_keys
            && let Some(v) = orig.get(str_key)
        {
            return v.clone();
        }
        Value::Str(Arc::new(str_key.to_string()))
    }
}

impl Deref for BagData {
    type Target = HashMap<String, NumBigInt>;
    fn deref(&self) -> &Self::Target {
        &self.counts
    }
}

impl DerefMut for BagData {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.counts
    }
}

impl PartialEq for BagData {
    fn eq(&self, other: &Self) -> bool {
        self.counts == other.counts
    }
}

impl SetData {
    pub fn new(elements: HashSet<String>) -> Self {
        SetData {
            elements,
            original_keys: None,
            value_type: None,
            key_type: None,
            declared_type: None,
        }
    }

    pub fn with_original_keys(
        elements: HashSet<String>,
        original_keys: HashMap<String, Value>,
    ) -> Self {
        SetData {
            elements,
            original_keys: Some(original_keys),
            value_type: None,
            key_type: None,
            declared_type: None,
        }
    }

    /// Whether container *type* metadata (element/key/declared type) is attached.
    pub fn has_type_meta(&self) -> bool {
        self.value_type.is_some() || self.key_type.is_some() || self.declared_type.is_some()
    }

    /// Get the original Value for a key, falling back to Str.
    pub fn typed_key(&self, str_key: &str) -> Value {
        if let Some(ref orig) = self.original_keys
            && let Some(v) = orig.get(str_key)
        {
            return v.clone();
        }
        Value::Str(Arc::new(str_key.to_string()))
    }
}

impl Deref for SetData {
    type Target = HashSet<String>;
    fn deref(&self) -> &Self::Target {
        &self.elements
    }
}

impl DerefMut for SetData {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.elements
    }
}

impl PartialEq for SetData {
    fn eq(&self, other: &Self) -> bool {
        self.elements == other.elements
    }
}

impl MixData {
    pub fn new(weights: HashMap<String, f64>) -> Self {
        MixData {
            weights,
            original_keys: None,
            value_type: None,
            key_type: None,
            declared_type: None,
        }
    }

    pub fn with_original_keys(
        weights: HashMap<String, f64>,
        original_keys: HashMap<String, Value>,
    ) -> Self {
        MixData {
            weights,
            original_keys: Some(original_keys),
            value_type: None,
            key_type: None,
            declared_type: None,
        }
    }

    /// Whether container *type* metadata (element/key/declared type) is attached.
    pub fn has_type_meta(&self) -> bool {
        self.value_type.is_some() || self.key_type.is_some() || self.declared_type.is_some()
    }

    /// Get the original Value for a key, falling back to Str.
    pub fn typed_key(&self, str_key: &str) -> Value {
        if let Some(ref orig) = self.original_keys
            && let Some(v) = orig.get(str_key)
        {
            return v.clone();
        }
        Value::Str(Arc::new(str_key.to_string()))
    }
}

impl Deref for MixData {
    type Target = HashMap<String, f64>;
    fn deref(&self) -> &Self::Target {
        &self.weights
    }
}

impl DerefMut for MixData {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.weights
    }
}

impl PartialEq for MixData {
    fn eq(&self, other: &Self) -> bool {
        self.weights == other.weights
    }
}
