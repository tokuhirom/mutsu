use super::super::*;

impl Interpreter {
    /// Get raw bounds of a range as f64 (NOT adjusted for exclusivity).
    pub(crate) fn range_raw_bounds_f64(v: &Value) -> (f64, f64) {
        match v {
            Value::Range(a, b) => (*a as f64, *b as f64),
            Value::RangeExcl(a, b) => (*a as f64, *b as f64),
            Value::RangeExclStart(a, b) => (*a as f64, *b as f64),
            Value::RangeExclBoth(a, b) => (*a as f64, *b as f64),
            Value::GenericRange { start, end, .. } => {
                let s = match start.as_ref() {
                    Value::Whatever | Value::HyperWhatever => f64::NEG_INFINITY,
                    v => v.to_f64(),
                };
                let e = match end.as_ref() {
                    Value::Whatever | Value::HyperWhatever => f64::INFINITY,
                    v => v.to_f64(),
                };
                (s, e)
            }
            _ => (0.0, 0.0),
        }
    }

    /// Get exclusivity flags for a range: (start_val, end_val, excl_start, excl_end).
    pub(crate) fn range_exclusivity(v: &Value) -> (f64, f64, bool, bool) {
        match v {
            Value::Range(a, b) => (*a as f64, *b as f64, false, false),
            Value::RangeExcl(a, b) => (*a as f64, *b as f64, false, true),
            Value::RangeExclStart(a, b) => (*a as f64, *b as f64, true, false),
            Value::RangeExclBoth(a, b) => (*a as f64, *b as f64, true, true),
            Value::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                let s = match start.as_ref() {
                    Value::Whatever | Value::HyperWhatever => f64::NEG_INFINITY,
                    v => v.to_f64(),
                };
                let e = match end.as_ref() {
                    Value::Whatever | Value::HyperWhatever => f64::INFINITY,
                    v => v.to_f64(),
                };
                (s, e, *excl_start, *excl_end)
            }
            _ => (0.0, 0.0, false, false),
        }
    }

    /// Get raw Value endpoints of a GenericRange (for Whatever detection).
    pub(in crate::runtime) fn range_raw_endpoints(v: &Value) -> (Value, Value) {
        match v {
            Value::GenericRange { start, end, .. } => {
                (start.as_ref().clone(), end.as_ref().clone())
            }
            _ => (Value::Nil, Value::Nil),
        }
    }

    /// Check if a range has string endpoints.
    pub(crate) fn range_has_string_endpoints(v: &Value) -> bool {
        match v {
            Value::GenericRange { start, end, .. } => {
                matches!(**start, Value::Str(_)) || matches!(**end, Value::Str(_))
            }
            _ => false,
        }
    }

    /// Get raw string bounds of a range.
    pub(crate) fn range_raw_string_bounds(v: &Value) -> (String, String) {
        match v {
            Value::GenericRange { start, end, .. } => {
                (start.to_string_value(), end.to_string_value())
            }
            Value::Range(a, b) => (a.to_string(), b.to_string()),
            Value::RangeExcl(a, b) => (a.to_string(), b.to_string()),
            Value::RangeExclStart(a, b) => (a.to_string(), b.to_string()),
            Value::RangeExclBoth(a, b) => (a.to_string(), b.to_string()),
            _ => (String::new(), String::new()),
        }
    }

    /// Compute element count of a range as f64.
    pub(crate) fn range_elems_f64(v: &Value) -> f64 {
        match v {
            Value::Range(a, b) => {
                if *b == i64::MAX || *a == i64::MIN {
                    f64::INFINITY
                } else {
                    (*b - *a + 1) as f64
                }
            }
            Value::RangeExcl(a, b) => {
                if *b == i64::MAX || *a == i64::MIN {
                    f64::INFINITY
                } else {
                    (*b - *a) as f64
                }
            }
            Value::RangeExclStart(a, b) => {
                if *b == i64::MAX || *a == i64::MIN {
                    f64::INFINITY
                } else {
                    (*b - *a) as f64
                }
            }
            Value::RangeExclBoth(a, b) => {
                if *b == i64::MAX || *a == i64::MIN {
                    f64::INFINITY
                } else {
                    (*b - *a - 1) as f64
                }
            }
            Value::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                let s = start.to_f64();
                let e = end.to_f64();
                let count = e - s + 1.0;
                let adj = if *excl_start { 1.0 } else { 0.0 } + if *excl_end { 1.0 } else { 0.0 };
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
            let start_is_whatever = matches!(r_start, Value::Whatever | Value::HyperWhatever);
            let end_is_whatever = matches!(r_end, Value::Whatever | Value::HyperWhatever);
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
        if let Value::Complex(re, im) = val {
            // Raku converts Complex to Real only if imaginary part is zero
            // (or very close to zero due to floating point)
            if *im == 0.0 || im.abs() < f64::EPSILON {
                let v = *re;
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
