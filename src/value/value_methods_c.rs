use super::*;

impl Value {
    /// Create a Match object with positional captures and the shared subject.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn make_match_object_full(
        from: i64,
        to: i64,
        positional: &[String],
        named: &HashMap<String, Vec<String>>,
        named_subcaps: &HashMap<String, Vec<std::sync::Arc<crate::runtime::CapNode>>>,
        positional_subcaps: &[Option<std::sync::Arc<crate::runtime::CapNode>>],
        positional_quantified: &[Option<Vec<crate::runtime::QuantifiedCaptureEntry>>],
        positional_nil: &[bool],
        target: crate::runtime::MatchTarget,
    ) -> Self {
        Self::make_match_object_full_q(
            from,
            to,
            positional,
            named,
            named_subcaps,
            positional_subcaps,
            positional_quantified,
            positional_nil,
            target,
            &HashSet::new(),
        )
    }

    /// Like make_match_object_full but with named_quantified tracking.
    ///
    /// ADR-0016 P5: this no longer builds an eager `Instance` tree. It
    /// synthesizes a top-level `CapNode` from the exploded capture axes and
    /// returns a lazy `Match` value (`ValueRepr::Match`); the Instance-shaped
    /// attribute map materializes on first observation, one level at a time
    /// (see `value::match_lazy`). The synthesized top node deliberately
    /// carries no `sym`/`action_name`/`ast`/`regex_vars`/`capture_alias_map`
    /// — the pre-P5 builder never surfaced those on the top-level Match.
    ///
    /// ADR-0016 P3: matched text is not passed or stored; `.Str` derives
    /// from the span through `target`.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn make_match_object_full_q(
        from: i64,
        to: i64,
        positional: &[String],
        named: &HashMap<String, Vec<String>>,
        named_subcaps: &HashMap<String, Vec<std::sync::Arc<crate::runtime::CapNode>>>,
        positional_subcaps: &[Option<std::sync::Arc<crate::runtime::CapNode>>],
        positional_quantified: &[Option<Vec<crate::runtime::QuantifiedCaptureEntry>>],
        positional_nil: &[bool],
        target: crate::runtime::MatchTarget,
        named_quantified: &HashSet<String>,
    ) -> Self {
        let has_children = !named.is_empty()
            || !named_subcaps.is_empty()
            || !named_quantified.is_empty()
            || !positional.is_empty()
            || !positional_subcaps.is_empty()
            || !positional_quantified.is_empty()
            || !positional_nil.is_empty();
        let children = has_children.then(|| {
            Box::new(crate::runtime::CapChildren {
                named: named.clone(),
                named_subcaps: named_subcaps.clone(),
                named_quantified: named_quantified.clone(),
                capture_alias_map: HashMap::new(),
                positional: positional.to_vec(),
                positional_subcaps: positional_subcaps.to_vec(),
                positional_quantified: positional_quantified.to_vec(),
                positional_nil: positional_nil.to_vec(),
                code_blocks: Vec::new(),
                regex_vars: HashMap::new(),
            })
        });
        let cap = crate::runtime::CapNode {
            from: from.max(0) as usize,
            to: to.max(0) as usize,
            sym: None,
            action_name: None,
            ast: None,
            children,
        };
        Value::lazy_match(std::sync::Arc::new(cap), target)
    }

    pub(crate) fn version_strip_trailing_zeros(parts: &[VersionPart]) -> Vec<VersionPart> {
        let mut v: Vec<VersionPart> = parts.to_vec();
        while matches!(v.last(), Some(VersionPart::Num(0))) {
            v.pop();
        }
        if v.is_empty() {
            vec![VersionPart::Num(0)]
        } else {
            v
        }
    }

    pub(crate) fn is_range(&self) -> bool {
        matches!(
            self.view(),
            ValueView::Range(_, _)
                | ValueView::RangeExcl(_, _)
                | ValueView::RangeExclStart(_, _)
                | ValueView::RangeExclBoth(_, _)
                | ValueView::GenericRange { .. }
        )
    }

    /// Check if this value is a numeric type (Int, Num, Rat, FatRat, BigInt).
    /// Returns the inner items if this value is an Array, Seq, or Slip.
    /// Decoded inside the seam so the returned slice borrows from `self`'s
    /// payload, not from a temporary view guard.
    pub(crate) fn as_list_items(&self) -> Option<&[Value]> {
        self.0.as_list_slice(false)
    }

    /// Like [`Self::as_list_items`], but also accepts the parallel Seq
    /// variants (`HyperSeq` / `RaceSeq`). Used by list-comparison helpers.
    pub(crate) fn as_list_items_with_hyper(&self) -> Option<&[Value]> {
        self.0.as_list_slice(true)
    }

    pub(crate) fn is_numeric(&self) -> bool {
        matches!(
            self.view(),
            ValueView::Int(_)
                | ValueView::BigInt(_)
                | ValueView::Num(_)
                | ValueView::Rat(_, _)
                | ValueView::FatRat(_, _)
                | ValueView::BigRat(_, _)
                | ValueView::Whatever
        )
    }

    /// Convert a numeric value to f64.
    pub(crate) fn to_f64(&self) -> f64 {
        match self.view() {
            // Phase 2 element container: numify the inner value transparently
            // if a `:=`-bound element's cell leaks into a numeric context.
            ValueView::ContainerRef(cell) => cell.lock().unwrap().to_f64(),
            ValueView::Int(i) => i as f64,
            ValueView::BigInt(n) => n.to_f64().unwrap_or(0.0),
            ValueView::Num(f) => f,
            ValueView::Rat(n, d) => {
                if d != 0 {
                    n as f64 / d as f64
                } else if n == 0 {
                    f64::NAN
                } else if n > 0 {
                    f64::INFINITY
                } else {
                    f64::NEG_INFINITY
                }
            }
            ValueView::FatRat(n, d) => {
                if d != 0 {
                    n as f64 / d as f64
                } else if n == 0 {
                    f64::NAN
                } else if n > 0 {
                    f64::INFINITY
                } else {
                    f64::NEG_INFINITY
                }
            }
            ValueView::BigRat(n, d) => {
                if !d.is_zero() {
                    n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0)
                } else if n.is_zero() {
                    f64::NAN
                } else if n.is_positive() {
                    f64::INFINITY
                } else {
                    f64::NEG_INFINITY
                }
            }
            ValueView::Bool(b) => {
                if b {
                    1.0
                } else {
                    0.0
                }
            }
            ValueView::Whatever => f64::INFINITY,
            ValueView::Str(s) => s.trim().parse::<f64>().unwrap_or(0.0),
            ValueView::Array(items, ..) => items.len() as f64,
            ValueView::Hash(map) => map.len() as f64,
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Instant" || class_name == "Duration" => attributes
                .as_map()
                .get("value")
                .map(|v| v.to_f64())
                .unwrap_or(0.0),
            // A subclass of native Int (e.g. `class Foo is Int`) carries its
            // integer payload in the reserved `__mutsu_int_value` attribute.
            ValueView::Instance { attributes, .. }
                if attributes.contains_key("__mutsu_int_value") =>
            {
                attributes
                    .as_map()
                    .get("__mutsu_int_value")
                    .map(|v| v.to_f64())
                    .unwrap_or(0.0)
            }
            // Match coerces to Numeric via its matched string
            ValueView::Instance { class_name, .. } if class_name == "Match" => self
                .match_str_value()
                .map(|v| v.to_string_value().trim().parse::<f64>().unwrap_or(0.0))
                .unwrap_or(0.0),
            // A numeric allomorph (IntStr/NumStr/RatStr) numifies to its inner value.
            ValueView::Mixin(inner, _) => inner.to_f64(),
            _ => 0.0,
        }
    }

    /// Convert a Value to a num_bigint::BigInt for arbitrary-precision arithmetic.
    pub(crate) fn to_bigint(&self) -> NumBigInt {
        match self.view() {
            ValueView::Int(i) => NumBigInt::from(i),
            ValueView::BigInt(n) => (**n).clone(),
            ValueView::Num(f) => NumBigInt::from(f as i64),
            ValueView::Rat(n, d) => {
                if d != 0 {
                    NumBigInt::from(n / d)
                } else {
                    NumBigInt::from(0)
                }
            }
            ValueView::BigRat(n, d) => {
                if !d.is_zero() {
                    n / d
                } else {
                    NumBigInt::from(0)
                }
            }
            ValueView::Str(s) => s
                .parse::<NumBigInt>()
                .unwrap_or_else(|_| NumBigInt::from(0)),
            // A subclass of native Int (e.g. `class Foo is Int`) carries its
            // integer payload in the reserved `__mutsu_int_value` attribute.
            ValueView::Instance { attributes, .. } => attributes
                .as_map()
                .get("__mutsu_int_value")
                .map(|v| v.to_bigint())
                .unwrap_or_else(|| NumBigInt::from(0)),
            ValueView::Mixin(inner, _) => inner.to_bigint(),
            _ => NumBigInt::from(0),
        }
    }

    /// Create a Value from a BigInt, normalizing to Int(i64) when possible.
    pub(crate) fn from_bigint(n: NumBigInt) -> Value {
        if let Some(i) = n.to_i64() {
            Value::Int(i)
        } else {
            Value::bigint(n)
        }
    }
}
