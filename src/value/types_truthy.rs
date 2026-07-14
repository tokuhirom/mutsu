use super::*;

impl Value {
    /// Check if this Failure instance is handled.
    /// Returns true if handled, false otherwise.
    /// Panics if called on a non-Failure.
    pub(crate) fn is_failure_handled(&self) -> bool {
        if let ValueView::Instance {
            class_name,
            attributes,
            id,
            ..
        } = self.view()
            && class_name.resolve() == "Failure"
        {
            // Check the global registry first (shared across clones)
            if let Some(handled) = super::is_failure_handled(id) {
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
        if let ValueView::Instance { id, .. } = self.view() {
            super::mark_failure_handled(id);
        }
    }

    pub(crate) fn truthy(&self) -> bool {
        match self.view() {
            // A `VarRef` is a transient binder wrapper: it is as true as the
            // variable's value.
            ValueView::VarRef { value, .. } => value.truthy(),
            ValueView::Bool(b) => b,
            ValueView::Int(i) => i != 0,
            ValueView::BigInt(n) => !n.is_zero(),
            ValueView::Num(f) => f != 0.0 || f.is_nan(),
            ValueView::Str(s) => !s.is_empty(),
            ValueView::Range(_, _) => true,
            ValueView::RangeExcl(_, _) => true,
            ValueView::RangeExclStart(_, _) => true,
            ValueView::RangeExclBoth(_, _) => true,
            ValueView::GenericRange { .. } => true,
            ValueView::Array(items, ..) => !items.is_empty(),
            ValueView::Hash(items) => !items.is_empty(),
            ValueView::Rat(n, _) => n != 0,
            ValueView::FatRat(n, _) => n != 0,
            ValueView::BigRat(n, _) => !n.is_zero(),
            ValueView::Complex(r, i) => r != 0.0 || i != 0.0,
            ValueView::Set(s, _) => !s.is_empty(),
            ValueView::Bag(b, _) => !b.is_empty(),
            ValueView::Mix(m, _) => !m.is_empty(),
            ValueView::Pair(_, _) | ValueView::ValuePair(_, _) => true,
            ValueView::Enum { value, .. } => match value {
                EnumValue::Int(i) => *i != 0,
                EnumValue::Str(s) => !s.is_empty(),
                EnumValue::Generic(v) => v.as_ref().truthy(),
            },
            ValueView::CompUnitDepSpec { .. } => true,
            ValueView::Package(_) | ValueView::ParametricRole { .. } => false,
            ValueView::Routine { .. } => true,
            ValueView::Sub(_) | ValueView::WeakSub(_) => true,
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } => {
                if class_name == "Proc" {
                    // Proc is truthy when exitcode == 0
                    return match attributes.as_map().get("exitcode").map(Value::view) {
                        Some(ValueView::Int(code)) => code == 0,
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
                    return match attributes.as_map().get("bytes").map(Value::view) {
                        Some(ValueView::Array(items, ..)) => !items.is_empty(),
                        _ => false,
                    };
                }
                true
            }
            ValueView::Junction { kind, values } => match kind {
                JunctionKind::Any => values.iter().any(|v| v.truthy()),
                JunctionKind::All => values.iter().all(|v| v.truthy()),
                JunctionKind::One => values.iter().filter(|v| v.truthy()).count() == 1,
                JunctionKind::None => values.iter().all(|v| !v.truthy()),
            },
            ValueView::Slip(items) => !items.is_empty(),
            ValueView::Seq(items) | ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => {
                !items.is_empty()
            }
            ValueView::LazyList(_) => true,
            ValueView::Promise(p) => p.is_resolved(),
            ValueView::Channel(_) => true,
            ValueView::Regex(_) | ValueView::RegexWithAdverbs { .. } => true,
            ValueView::Version { .. } => true,
            ValueView::Nil => false,
            ValueView::Whatever => true,
            ValueView::HyperWhatever => true,
            ValueView::Capture { positional, named } => !positional.is_empty() || !named.is_empty(),
            ValueView::Uni(u) => !u.text.is_empty(),
            ValueView::Mixin(inner, mixins) => {
                if let Some(bool_val) = mixins.get("Bool") {
                    bool_val.truthy()
                } else {
                    inner.truthy()
                }
            }
            ValueView::Proxy { .. } => true,
            ValueView::CustomType { .. } => false,
            ValueView::CustomTypeInstance(_) => true,
            ValueView::Scalar(inner) => inner.truthy(),
            ValueView::ContainerRef(_) => self.with_deref(Value::truthy),
            ValueView::LazyThunk(thunk_data) => {
                let cache = thunk_data.cache.lock().unwrap();
                if let Some(ref cached) = *cache {
                    cached.truthy()
                } else {
                    // Unforced thunk is truthy (it exists)
                    true
                }
            }
            ValueView::LazyIoLines { .. } => true,
            // An unmaterialized deferred bind token is undefined (and hence
            // falsy) until written through — see `value_is_defined`.
            ValueView::HashEntryRef { .. } => false,
        }
    }
}
