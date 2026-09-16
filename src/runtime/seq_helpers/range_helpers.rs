use super::super::*;

impl Interpreter {
    /// Get raw bounds of a range as f64 (NOT adjusted for exclusivity).
    pub(crate) fn range_raw_bounds_f64(v: &Value) -> (f64, f64) {
        match v.view() {
            ValueView::Range(a, b) => (a as f64, b as f64),
            ValueView::RangeExcl(a, b) => (a as f64, b as f64),
            ValueView::RangeExclStart(a, b) => (a as f64, b as f64),
            ValueView::RangeExclBoth(a, b) => (a as f64, b as f64),
            ValueView::GenericRange { start, end, .. } => {
                let s = match start.as_ref().view() {
                    ValueView::Whatever | ValueView::HyperWhatever => f64::NEG_INFINITY,
                    _ => start.to_f64(),
                };
                let e = match end.as_ref().view() {
                    ValueView::Whatever | ValueView::HyperWhatever => f64::INFINITY,
                    _ => end.to_f64(),
                };
                (s, e)
            }
            _ => (0.0, 0.0),
        }
    }

    /// Get exclusivity flags for a range: (start_val, end_val, excl_start, excl_end).
    pub(crate) fn range_exclusivity(v: &Value) -> (f64, f64, bool, bool) {
        match v.view() {
            ValueView::Range(a, b) => (a as f64, b as f64, false, false),
            ValueView::RangeExcl(a, b) => (a as f64, b as f64, false, true),
            ValueView::RangeExclStart(a, b) => (a as f64, b as f64, true, false),
            ValueView::RangeExclBoth(a, b) => (a as f64, b as f64, true, true),
            ValueView::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                let s = match start.as_ref().view() {
                    ValueView::Whatever | ValueView::HyperWhatever => f64::NEG_INFINITY,
                    _ => start.to_f64(),
                };
                let e = match end.as_ref().view() {
                    ValueView::Whatever | ValueView::HyperWhatever => f64::INFINITY,
                    _ => end.to_f64(),
                };
                (s, e, excl_start, excl_end)
            }
            _ => (0.0, 0.0, false, false),
        }
    }

    /// Get raw Value endpoints of a GenericRange (for Whatever detection).
    pub(in crate::runtime) fn range_raw_endpoints(v: &Value) -> (Value, Value) {
        match v.view() {
            ValueView::GenericRange { start, end, .. } => {
                (start.as_ref().clone(), end.as_ref().clone())
            }
            _ => (Value::NIL, Value::NIL),
        }
    }

    /// Check if a range has string endpoints.
    pub(crate) fn range_has_string_endpoints(v: &Value) -> bool {
        match v.view() {
            ValueView::GenericRange { start, end, .. } => {
                matches!(start.as_ref().view(), ValueView::Str(_))
                    || matches!(end.as_ref().view(), ValueView::Str(_))
            }
            _ => false,
        }
    }

    /// Get raw string bounds of a range.
    pub(crate) fn range_raw_string_bounds(v: &Value) -> (String, String) {
        match v.view() {
            ValueView::GenericRange { start, end, .. } => {
                (start.to_string_value(), end.to_string_value())
            }
            ValueView::Range(a, b) => (a.to_string(), b.to_string()),
            ValueView::RangeExcl(a, b) => (a.to_string(), b.to_string()),
            ValueView::RangeExclStart(a, b) => (a.to_string(), b.to_string()),
            ValueView::RangeExclBoth(a, b) => (a.to_string(), b.to_string()),
            _ => (String::new(), String::new()),
        }
    }

    /// Convert a `GenericRange` endpoint to an exact `BigInt`, when it is one
    /// (`Int` or `BigInt`). `None` for anything else — a `Whatever`/`Inf`
    /// end, a fractional `Num`/`Rat`, a `Str` endpoint, etc. — so callers fall
    /// back to the approximate `f64` computation for those.
    fn range_endpoint_as_bigint(v: &Value) -> Option<num_bigint::BigInt> {
        match v.view() {
            ValueView::Int(i) => Some(num_bigint::BigInt::from(i)),
            ValueView::BigInt(n) => Some((**n).clone()),
            _ => None,
        }
    }

    /// Compute element count of a range as f64.
    ///
    /// A `GenericRange` first tries an exact `BigInt` subtraction of its
    /// endpoints before falling back to `f64`: converting each endpoint to
    /// `f64` independently (the old approach) rounds both to the *same*
    /// float once their magnitude exceeds `f64`'s 52-bit mantissa (e.g. two
    /// `BigInt`s around `2**70`, 10 apart), collapsing a genuinely non-zero
    /// difference to zero (see #8591).
    pub(crate) fn range_elems_f64(v: &Value) -> f64 {
        match v.view() {
            ValueView::Range(a, b) => {
                if b == i64::MAX || a == i64::MIN {
                    f64::INFINITY
                } else {
                    (b - a + 1) as f64
                }
            }
            ValueView::RangeExcl(a, b) => {
                if b == i64::MAX || a == i64::MIN {
                    f64::INFINITY
                } else {
                    (b - a) as f64
                }
            }
            ValueView::RangeExclStart(a, b) => {
                if b == i64::MAX || a == i64::MIN {
                    f64::INFINITY
                } else {
                    (b - a) as f64
                }
            }
            ValueView::RangeExclBoth(a, b) => {
                if b == i64::MAX || a == i64::MIN {
                    f64::INFINITY
                } else {
                    (b - a - 1) as f64
                }
            }
            ValueView::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                let adj = if excl_start { 1.0 } else { 0.0 } + if excl_end { 1.0 } else { 0.0 };
                if let (Some(s), Some(e)) = (
                    Self::range_endpoint_as_bigint(start.as_ref()),
                    Self::range_endpoint_as_bigint(end.as_ref()),
                ) {
                    let count = e - s + 1;
                    return num_traits::ToPrimitive::to_f64(&count).unwrap_or(0.0) - adj;
                }
                let s = start.to_f64();
                let e = end.to_f64();
                let count = e - s + 1.0;
                count - adj
            }
            _ => 0.0,
        }
    }

    /// Check if a value is contained within a range.
    pub(crate) fn value_in_range(val: &Value, range: &Value) -> bool {
        let (r_min, r_max) = Self::range_raw_bounds_f64(range);
        let (_, _, r_es, r_ee) = Self::range_exclusivity(range);

        // For string ranges, compare strings
        if Self::range_has_string_endpoints(range) {
            let v_str = val.to_string_value();
            let (r_start, r_end) = Self::range_raw_endpoints(range);
            let start_is_whatever = matches!(
                r_start.view(),
                ValueView::Whatever | ValueView::HyperWhatever
            );
            let end_is_whatever =
                matches!(r_end.view(), ValueView::Whatever | ValueView::HyperWhatever);
            let (r_min_s, r_max_s) = Self::range_raw_string_bounds(range);
            let min_ok = if start_is_whatever {
                true
            } else if r_es {
                v_str > r_min_s
            } else {
                v_str >= r_min_s
            };
            let max_ok = if end_is_whatever {
                true
            } else if r_ee {
                v_str < r_max_s
            } else {
                v_str <= r_max_s
            };
            return min_ok && max_ok;
        }

        // Handle Complex values: if imaginary part is negligible, use real part
        if let ValueView::Complex(re, im) = val.view() {
            // Raku converts Complex to Real only if imaginary part is zero
            // (or very close to zero due to floating point)
            if im == 0.0 || im.abs() < f64::EPSILON {
                let v = re;
                let min_ok = if r_es { v > r_min } else { v >= r_min };
                let max_ok = if r_ee { v < r_max } else { v <= r_max };
                return min_ok && max_ok;
            } else {
                return false;
            }
        }

        let v = val.to_f64();
        let min_ok = if r_es { v > r_min } else { v >= r_min };
        let max_ok = if r_ee { v < r_max } else { v <= r_max };
        min_ok && max_ok
    }
}
