use super::*;

impl Value {
    /// Check if this Failure instance is handled.
    /// Returns true if handled, false otherwise.
    /// Panics if called on a non-Failure.
    pub(crate) fn is_failure_handled(&self) -> bool {
        if let Value::Instance {
            class_name,
            attributes,
            id,
            ..
        } = self
            && class_name.resolve() == "Failure"
        {
            // Check the global registry first (shared across clones)
            if let Some(handled) = super::is_failure_handled(*id) {
                return handled;
            }
            // Fall back to the attribute
            attributes
                .as_map()
                .get("handled")
                .is_some_and(|v| v.truthy())
        } else {
            false
        }
    }

    /// Mark this Failure as handled in the global registry.
    pub(crate) fn mark_failure_handled(&self) {
        if let Value::Instance { id, .. } = self {
            super::mark_failure_handled(*id);
        }
    }

    pub(crate) fn truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Int(i) => *i != 0,
            Value::BigInt(n) => !n.is_zero(),
            Value::Num(f) => *f != 0.0 || f.is_nan(),
            Value::Str(s) => !s.is_empty(),
            Value::Range(_, _) => true,
            Value::RangeExcl(_, _) => true,
            Value::RangeExclStart(_, _) => true,
            Value::RangeExclBoth(_, _) => true,
            Value::GenericRange { .. } => true,
            Value::Array(items, ..) => !items.is_empty(),
            Value::Hash(items) => !items.is_empty(),
            Value::Rat(n, _) => *n != 0,
            Value::FatRat(n, _) => !n.is_zero(),
            Value::BigRat(n, _) => !n.is_zero(),
            Value::Complex(r, i) => *r != 0.0 || *i != 0.0,
            Value::Set(s, _) => !s.is_empty(),
            Value::Bag(b, _) => !b.is_empty(),
            Value::Mix(m, _) => !m.is_empty(),
            Value::Pair(_, _) | Value::ValuePair(_, _) => true,
            Value::Enum { value, .. } => match value {
                EnumValue::Int(i) => *i != 0,
                EnumValue::Str(s) => !s.is_empty(),
                EnumValue::Generic(v) => v.as_ref().truthy(),
            },
            Value::CompUnitDepSpec { .. } => true,
            Value::Package(_) | Value::ParametricRole { .. } => false,
            Value::Routine { .. } => true,
            Value::Sub(_) | Value::WeakSub(_) => true,
            Value::Instance {
                class_name,
                attributes,
                ..
            } => {
                if class_name == "Proc" {
                    // Proc is truthy when exitcode == 0
                    return match attributes.as_map().get("exitcode") {
                        Some(Value::Int(code)) => *code == 0,
                        _ => false,
                    };
                }
                if class_name == "Failure" {
                    return false;
                }
                // Buf/Blob: truthy when non-empty
                let cn = class_name.resolve();
                if cn == "Buf"
                    || cn == "Blob"
                    || cn.starts_with("Buf[")
                    || cn.starts_with("Blob[")
                    || cn.starts_with("buf")
                    || cn.starts_with("blob")
                {
                    return match attributes.as_map().get("bytes") {
                        Some(Value::Array(items, ..)) => !items.is_empty(),
                        _ => false,
                    };
                }
                true
            }
            Value::Junction { kind, values } => match kind {
                JunctionKind::Any => values.iter().any(|v| v.truthy()),
                JunctionKind::All => values.iter().all(|v| v.truthy()),
                JunctionKind::One => values.iter().filter(|v| v.truthy()).count() == 1,
                JunctionKind::None => values.iter().all(|v| !v.truthy()),
            },
            Value::Slip(items) => !items.is_empty(),
            Value::Seq(items) | Value::HyperSeq(items) | Value::RaceSeq(items) => !items.is_empty(),
            Value::LazyList(_) => true,
            Value::Promise(p) => p.is_resolved(),
            Value::Channel(_) => true,
            Value::Regex(_) | Value::RegexWithAdverbs { .. } => true,
            Value::Version { .. } => true,
            Value::Nil => false,
            Value::Whatever => true,
            Value::HyperWhatever => true,
            Value::Capture { positional, named } => !positional.is_empty() || !named.is_empty(),
            Value::Uni(u) => !u.text.is_empty(),
            Value::Mixin(inner, mixins) => {
                if let Some(bool_val) = mixins.get("Bool") {
                    bool_val.truthy()
                } else {
                    inner.truthy()
                }
            }
            Value::Proxy { .. } => true,
            Value::CustomType { .. } => false,
            Value::CustomTypeInstance(_) => true,
            Value::Scalar(inner) => inner.truthy(),
            Value::ContainerRef(_) => self.with_deref(Value::truthy),
            Value::LazyThunk(thunk_data) => {
                let cache = thunk_data.cache.lock().unwrap();
                if let Some(ref cached) = *cache {
                    cached.truthy()
                } else {
                    // Unforced thunk is truthy (it exists)
                    true
                }
            }
            Value::LazyIoLines { .. } => true,
            // An unmaterialized deferred bind token is undefined (and hence
            // falsy) until written through — see `value_is_defined`.
            Value::HashEntryRef { .. } => false,
        }
    }
}
