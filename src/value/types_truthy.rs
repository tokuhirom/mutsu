use super::*;

impl Value {
    /// Check if this Failure instance is handled.
    /// Returns true if handled, false otherwise.
    /// Panics if called on a non-Failure.
    pub(crate) fn is_failure_handled(&self) -> bool {
        if let Value(ValueRepr::Instance {
            class_name,
            attributes,
            id,
            ..
        }) = self
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
        if let Value(ValueRepr::Instance { id, .. }) = self {
            super::mark_failure_handled(*id);
        }
    }

    pub(crate) fn truthy(&self) -> bool {
        match self {
            Value(ValueRepr::Bool(b)) => *b,
            Value(ValueRepr::Int(i)) => *i != 0,
            Value(ValueRepr::BigInt(n)) => !n.is_zero(),
            Value(ValueRepr::Num(f)) => *f != 0.0 || f.is_nan(),
            Value(ValueRepr::Str(s)) => !s.is_empty(),
            Value(ValueRepr::Range(_, _)) => true,
            Value(ValueRepr::RangeExcl(_, _)) => true,
            Value(ValueRepr::RangeExclStart(_, _)) => true,
            Value(ValueRepr::RangeExclBoth(_, _)) => true,
            Value(ValueRepr::GenericRange { .. }) => true,
            Value(ValueRepr::Array(items, ..)) => !items.is_empty(),
            Value(ValueRepr::Hash(items, _)) => !items.is_empty(),
            Value(ValueRepr::Rat(n, _)) => *n != 0,
            Value(ValueRepr::FatRat(n, _)) => !n.is_zero(),
            Value(ValueRepr::BigRat(n, _)) => !n.is_zero(),
            Value(ValueRepr::Complex(r, i)) => *r != 0.0 || *i != 0.0,
            Value(ValueRepr::Set(s, _)) => !s.is_empty(),
            Value(ValueRepr::Bag(b, _)) => !b.is_empty(),
            Value(ValueRepr::Mix(m, _)) => !m.is_empty(),
            Value(ValueRepr::Pair(_, _)) | Value(ValueRepr::ValuePair(_, _)) => true,
            Value(ValueRepr::Enum { value, .. }) => match value {
                EnumValue::Int(i) => *i != 0,
                EnumValue::Str(s) => !s.is_empty(),
                EnumValue::Generic(v) => v.as_ref().truthy(),
            },
            Value(ValueRepr::CompUnitDepSpec { .. }) => true,
            Value(ValueRepr::Package(_)) | Value(ValueRepr::ParametricRole { .. }) => false,
            Value(ValueRepr::Routine { .. }) => true,
            Value(ValueRepr::Sub(_)) | Value(ValueRepr::WeakSub(_)) => true,
            Value(ValueRepr::Instance {
                class_name,
                attributes,
                ..
            }) => {
                if class_name == "Proc" {
                    // Proc is truthy when exitcode == 0
                    return match attributes.as_map().get("exitcode") {
                        Some(Value(ValueRepr::Int(code))) => *code == 0,
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
                        Some(Value(ValueRepr::Array(items, ..))) => !items.is_empty(),
                        _ => false,
                    };
                }
                true
            }
            Value(ValueRepr::Junction { kind, values }) => match kind {
                JunctionKind::Any => values.iter().any(|v| v.truthy()),
                JunctionKind::All => values.iter().all(|v| v.truthy()),
                JunctionKind::One => values.iter().filter(|v| v.truthy()).count() == 1,
                JunctionKind::None => values.iter().all(|v| !v.truthy()),
            },
            Value(ValueRepr::Slip(items)) => !items.is_empty(),
            Value(ValueRepr::Seq(items))
            | Value(ValueRepr::HyperSeq(items))
            | Value(ValueRepr::RaceSeq(items)) => !items.is_empty(),
            Value(ValueRepr::LazyList(_)) => true,
            Value(ValueRepr::Promise(p)) => p.is_resolved(),
            Value(ValueRepr::Channel(_)) => true,
            Value(ValueRepr::Regex(_)) | Value(ValueRepr::RegexWithAdverbs { .. }) => true,
            Value(ValueRepr::Version { .. }) => true,
            Value(ValueRepr::Nil) => false,
            Value(ValueRepr::Whatever) => true,
            Value(ValueRepr::HyperWhatever) => true,
            Value(ValueRepr::Capture { positional, named }) => {
                !positional.is_empty() || !named.is_empty()
            }
            Value(ValueRepr::Uni(u)) => !u.text.is_empty(),
            Value(ValueRepr::Mixin(inner, mixins)) => {
                if let Some(bool_val) = mixins.get("Bool") {
                    bool_val.truthy()
                } else {
                    inner.truthy()
                }
            }
            Value(ValueRepr::Proxy { .. }) => true,
            Value(ValueRepr::CustomType { .. }) => false,
            Value(ValueRepr::CustomTypeInstance(_)) => true,
            Value(ValueRepr::Scalar(inner)) => inner.truthy(),
            Value(ValueRepr::ContainerRef(_)) => self.with_deref(Value::truthy),
            Value(ValueRepr::LazyThunk(thunk_data)) => {
                let cache = thunk_data.cache.lock().unwrap();
                if let Some(ref cached) = *cache {
                    cached.truthy()
                } else {
                    // Unforced thunk is truthy (it exists)
                    true
                }
            }
            Value(ValueRepr::LazyIoLines { .. }) => true,
            // An unmaterialized deferred bind token is undefined (and hence
            // falsy) until written through — see `value_is_defined`.
            Value(ValueRepr::HashEntryRef { .. }) => false,
        }
    }
}
