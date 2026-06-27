use super::*;

impl Value {
    /// Create a Match object with positional captures and original string.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn make_match_object_full(
        matched: String,
        from: i64,
        to: i64,
        positional: &[String],
        named: &HashMap<String, Vec<String>>,
        named_subcaps: &HashMap<String, Vec<crate::runtime::RegexCaptures>>,
        positional_subcaps: &[Option<crate::runtime::RegexCaptures>],
        positional_quantified: &[Option<Vec<crate::runtime::QuantifiedCaptureEntry>>],
        positional_nil: &[bool],
        orig: Option<&str>,
    ) -> Self {
        Self::make_match_object_full_q(
            matched,
            from,
            to,
            positional,
            named,
            named_subcaps,
            positional_subcaps,
            positional_quantified,
            positional_nil,
            orig,
            &HashSet::new(),
        )
    }

    /// Like make_match_object_full but with named_quantified tracking.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn make_match_object_full_q(
        matched: String,
        from: i64,
        to: i64,
        positional: &[String],
        named: &HashMap<String, Vec<String>>,
        named_subcaps: &HashMap<String, Vec<crate::runtime::RegexCaptures>>,
        positional_subcaps: &[Option<crate::runtime::RegexCaptures>],
        positional_quantified: &[Option<Vec<crate::runtime::QuantifiedCaptureEntry>>],
        positional_nil: &[bool],
        orig: Option<&str>,
        named_quantified: &HashSet<String>,
    ) -> Self {
        fn make_capture_match(s: &str, orig: Option<&str>, search_from: usize) -> Value {
            let mut attrs = HashMap::new();
            attrs.insert("str".to_string(), Value::str(s.to_string()));
            // Try to find the captured text's position within the original string
            let (cap_from, cap_to) = if let Some(o) = orig {
                // Search for the captured substring starting from search_from
                let haystack: Vec<char> = o.chars().collect();
                let needle: Vec<char> = s.chars().collect();
                let mut found_from = 0i64;
                let mut found = false;
                for start in search_from..=haystack.len().saturating_sub(needle.len()) {
                    if haystack[start..start + needle.len()] == needle[..] {
                        found_from = start as i64;
                        found = true;
                        break;
                    }
                }
                if found {
                    (found_from, found_from + needle.len() as i64)
                } else {
                    (0i64, s.chars().count() as i64)
                }
            } else {
                (0i64, s.chars().count() as i64)
            };
            attrs.insert("from".to_string(), Value::Int(cap_from));
            attrs.insert("to".to_string(), Value::Int(cap_to));
            attrs.insert("list".to_string(), Value::array(Vec::new()));
            attrs.insert("named".to_string(), Value::hash(HashMap::new()));
            if let Some(o) = orig {
                attrs.insert("orig".to_string(), Value::str(o.to_string()));
            }
            Value::make_instance(Symbol::intern("Match"), attrs)
        }

        /// Build a Match object from a RegexCaptures, recursively handling subcaptures.
        fn make_subcap_match(caps: &crate::runtime::RegexCaptures, orig: Option<&str>) -> Value {
            let search_start = caps.from;
            let pos_vals: Vec<Value> = caps
                .positional
                .iter()
                .enumerate()
                .map(|(i, s)| {
                    // An unmatched optional capture (`(x)?` zero match) renders as Nil.
                    if caps.positional_nil.get(i) == Some(&true) {
                        return Value::Nil;
                    }
                    // Check if this positional entry is a quantified list
                    if let Some(Some(qlist)) = caps.positional_quantified.get(i) {
                        let arr: Vec<Value> = qlist
                            .iter()
                            .map(|(text, from, to, subcap)| {
                                if let Some(sc) = subcap {
                                    return make_subcap_match(sc, orig);
                                }
                                let mut a = HashMap::new();
                                a.insert("str".to_string(), Value::str(text.clone()));
                                a.insert("from".to_string(), Value::Int(*from as i64));
                                a.insert("to".to_string(), Value::Int(*to as i64));
                                a.insert("list".to_string(), Value::array(Vec::new()));
                                a.insert("named".to_string(), Value::hash(HashMap::new()));
                                if let Some(o) = orig {
                                    a.insert("orig".to_string(), Value::str(o.to_string()));
                                }
                                Value::make_instance(Symbol::intern("Match"), a)
                            })
                            .collect();
                        return Value::array(arr);
                    }
                    // Recursively build nested Match objects for positional subcaptures
                    if let Some(Some(subcap)) = caps.positional_subcaps.get(i) {
                        return make_subcap_match(subcap, orig);
                    }
                    make_capture_match(s, orig, search_start)
                })
                .collect();
            let mut sub_named: HashMap<String, Value> = HashMap::new();
            for (key, values) in &caps.named {
                let subcaps_for_key = caps.named_subcaps.get(key);
                let vals: Vec<Value> = values
                    .iter()
                    .enumerate()
                    .map(|(i, s)| {
                        if let Some(scs) = subcaps_for_key
                            && let Some(sc) = scs.get(i)
                        {
                            return make_subcap_match(sc, orig);
                        }
                        make_capture_match(s, orig, search_start)
                    })
                    .collect();
                if vals.len() == 1 && !caps.named_quantified.contains(key) {
                    sub_named.insert(key.clone(), vals[0].clone());
                } else {
                    sub_named.insert(key.clone(), Value::real_array(vals));
                }
            }
            // For quantified named captures that matched zero times, insert empty arrays
            for qname in &caps.named_quantified {
                sub_named
                    .entry(qname.clone())
                    .or_insert_with(|| Value::real_array(Vec::new()));
            }
            // Silent-action captures: hidden `<.foo>` subrule matches (stored under
            // a marker key in `named_subcaps`) that carry nested captures. They are
            // absent from `named`/`.hash`, but their action methods must fire, so
            // build them into a `silent_caps` array for the grammar action walk.
            // Each subcap's `action_name` carries the rule name to dispatch on.
            let mut silent_caps_vals: Vec<Value> = Vec::new();
            for (key, scs) in &caps.named_subcaps {
                if key.starts_with(crate::runtime::SILENT_ACTION_MARKER_PREFIX) {
                    for sc in scs {
                        silent_caps_vals.push(make_subcap_match(sc, orig));
                    }
                }
            }
            let mut attrs = HashMap::new();
            attrs.insert("str".to_string(), Value::str(caps.matched.clone()));
            attrs.insert("from".to_string(), Value::Int(caps.from as i64));
            attrs.insert("to".to_string(), Value::Int(caps.to as i64));
            attrs.insert("list".to_string(), Value::array(pos_vals));
            attrs.insert("named".to_string(), Value::hash(sub_named));
            if !silent_caps_vals.is_empty() {
                attrs.insert(
                    "silent_caps".to_string(),
                    Value::real_array(silent_caps_vals),
                );
            }
            if let Some(o) = orig {
                attrs.insert("orig".to_string(), Value::str(o.to_string()));
            }
            // Store :sym<> variant name so action dispatch can find it
            if let Some(ref sym_val) = caps.sym {
                attrs.insert("sym_variant".to_string(), Value::str(sym_val.clone()));
            }
            if let Some(ref action_name) = caps.action_name {
                attrs.insert("action_name".to_string(), Value::str(action_name.clone()));
            }
            if !caps.capture_alias_map.is_empty() {
                let alias_hash: HashMap<String, Value> = caps
                    .capture_alias_map
                    .iter()
                    .map(|(k, v)| (k.clone(), Value::str(v.clone())))
                    .collect();
                attrs.insert("capture_alias_map".to_string(), Value::hash(alias_hash));
            }
            Value::make_instance(Symbol::intern("Match"), attrs)
        }

        let mut attrs = HashMap::new();
        attrs.insert("str".to_string(), Value::str(matched));
        attrs.insert("from".to_string(), Value::Int(from));
        attrs.insert("to".to_string(), Value::Int(to));
        if let Some(o) = orig {
            attrs.insert("orig".to_string(), Value::str(o.to_string()));
        }
        let search_start = from as usize;
        let caps: Vec<Value> = positional
            .iter()
            .enumerate()
            .map(|(i, s)| {
                // An unmatched optional capture (`(x)?` zero match) renders as Nil.
                if positional_nil.get(i) == Some(&true) {
                    return Value::Nil;
                }
                // Check if this positional entry is a quantified list
                if let Some(Some(qlist)) = positional_quantified.get(i) {
                    let arr: Vec<Value> = qlist
                        .iter()
                        .map(|(text, qfrom, qto, subcap)| {
                            if let Some(sc) = subcap {
                                return make_subcap_match(sc, orig);
                            }
                            let mut a = HashMap::new();
                            a.insert("str".to_string(), Value::str(text.clone()));
                            a.insert("from".to_string(), Value::Int(*qfrom as i64));
                            a.insert("to".to_string(), Value::Int(*qto as i64));
                            a.insert("list".to_string(), Value::array(Vec::new()));
                            a.insert("named".to_string(), Value::hash(HashMap::new()));
                            if let Some(o) = orig {
                                a.insert("orig".to_string(), Value::str(o.to_string()));
                            }
                            Value::make_instance(Symbol::intern("Match"), a)
                        })
                        .collect();
                    return Value::array(arr);
                }
                // If there are nested subcaptures for this positional capture,
                // build a full Match object with subcaptures
                if let Some(Some(subcap)) = positional_subcaps.get(i) {
                    return make_subcap_match(subcap, orig);
                }
                make_capture_match(s, orig, search_start)
            })
            .collect();
        attrs.insert("list".to_string(), Value::array(caps));
        let mut named_caps_map: HashMap<String, Value> = HashMap::new();
        for (key, values) in named {
            let subcaps_for_key = named_subcaps.get(key);
            let vals: Vec<Value> = values
                .iter()
                .enumerate()
                .map(|(i, s)| {
                    if let Some(scs) = subcaps_for_key
                        && let Some(sc) = scs.get(i)
                    {
                        return make_subcap_match(sc, orig);
                    }
                    make_capture_match(s, orig, search_start)
                })
                .collect();
            if vals.len() == 1 && !named_quantified.contains(key) {
                named_caps_map.insert(key.clone(), vals[0].clone());
            } else {
                named_caps_map.insert(key.clone(), Value::real_array(vals));
            }
        }
        // For quantified named captures that matched zero times, insert empty arrays
        for qname in named_quantified {
            named_caps_map
                .entry(qname.clone())
                .or_insert_with(|| Value::real_array(Vec::new()));
        }
        attrs.insert("named".to_string(), Value::hash(named_caps_map));
        // Silent-action captures: hidden `<.foo>` subrule matches carrying nested
        // captures (see make_subcap_match). Build them into a `silent_caps` array
        // so the grammar action walk fires their (and descendants') actions; they
        // are deliberately absent from `named`/`.hash`.
        let mut silent_caps_vals: Vec<Value> = Vec::new();
        for (key, scs) in named_subcaps {
            if key.starts_with(crate::runtime::SILENT_ACTION_MARKER_PREFIX) {
                for sc in scs {
                    silent_caps_vals.push(make_subcap_match(sc, orig));
                }
            }
        }
        if !silent_caps_vals.is_empty() {
            attrs.insert(
                "silent_caps".to_string(),
                Value::real_array(silent_caps_vals),
            );
        }
        Value::make_instance(Symbol::intern("Match"), attrs)
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
            self,
            Value::Range(_, _)
                | Value::RangeExcl(_, _)
                | Value::RangeExclStart(_, _)
                | Value::RangeExclBoth(_, _)
                | Value::GenericRange { .. }
        )
    }

    /// Check if this value is a numeric type (Int, Num, Rat, FatRat, BigInt).
    /// Returns the inner items if this value is an Array, Seq, or Slip.
    pub(crate) fn as_list_items(&self) -> Option<&[Value]> {
        match self {
            Value::Array(items, _) => Some(&items.items[..]),
            Value::Seq(items) | Value::Slip(items) => Some(&items[..]),
            _ => None,
        }
    }

    pub(crate) fn is_numeric(&self) -> bool {
        matches!(
            self,
            Value::Int(_)
                | Value::BigInt(_)
                | Value::Num(_)
                | Value::Rat(_, _)
                | Value::FatRat(_, _)
                | Value::BigRat(_, _)
                | Value::Whatever
        )
    }

    /// Convert a numeric value to f64.
    pub(crate) fn to_f64(&self) -> f64 {
        match self {
            // Phase 2 element container: numify the inner value transparently
            // if a `:=`-bound element's cell leaks into a numeric context.
            Value::ContainerRef(cell) => cell.lock().unwrap().to_f64(),
            Value::Int(i) => *i as f64,
            Value::BigInt(n) => n.to_f64().unwrap_or(0.0),
            Value::Num(f) => *f,
            Value::Rat(n, d) => {
                if *d != 0 {
                    *n as f64 / *d as f64
                } else if *n == 0 {
                    f64::NAN
                } else if *n > 0 {
                    f64::INFINITY
                } else {
                    f64::NEG_INFINITY
                }
            }
            Value::FatRat(n, d) => {
                if *d != 0 {
                    *n as f64 / *d as f64
                } else if *n == 0 {
                    f64::NAN
                } else if *n > 0 {
                    f64::INFINITY
                } else {
                    f64::NEG_INFINITY
                }
            }
            Value::BigRat(n, d) => {
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
            Value::Bool(b) => {
                if *b {
                    1.0
                } else {
                    0.0
                }
            }
            Value::Whatever => f64::INFINITY,
            Value::Str(s) => s.trim().parse::<f64>().unwrap_or(0.0),
            Value::Array(items, ..) => items.len() as f64,
            Value::Hash(map) => map.len() as f64,
            Value::Instance {
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
            Value::Instance { attributes, .. } if attributes.contains_key("__mutsu_int_value") => {
                attributes
                    .as_map()
                    .get("__mutsu_int_value")
                    .map(|v| v.to_f64())
                    .unwrap_or(0.0)
            }
            // Match coerces to Numeric via its matched string
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Match" => attributes
                .as_map()
                .get("str")
                .map(|v| v.to_string_value().trim().parse::<f64>().unwrap_or(0.0))
                .unwrap_or(0.0),
            _ => 0.0,
        }
    }

    /// Convert a Value to a num_bigint::BigInt for arbitrary-precision arithmetic.
    pub(crate) fn to_bigint(&self) -> NumBigInt {
        match self {
            Value::Int(i) => NumBigInt::from(*i),
            Value::BigInt(n) => (**n).clone(),
            Value::Num(f) => NumBigInt::from(*f as i64),
            Value::Rat(n, d) => {
                if *d != 0 {
                    NumBigInt::from(n / d)
                } else {
                    NumBigInt::from(0)
                }
            }
            Value::BigRat(n, d) => {
                if !d.is_zero() {
                    n.as_ref() / d.as_ref()
                } else {
                    NumBigInt::from(0)
                }
            }
            Value::Str(s) => s
                .parse::<NumBigInt>()
                .unwrap_or_else(|_| NumBigInt::from(0)),
            // A subclass of native Int (e.g. `class Foo is Int`) carries its
            // integer payload in the reserved `__mutsu_int_value` attribute.
            Value::Instance { attributes, .. } => attributes
                .as_map()
                .get("__mutsu_int_value")
                .map(|v| v.to_bigint())
                .unwrap_or_else(|| NumBigInt::from(0)),
            Value::Mixin(inner, _) => inner.to_bigint(),
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
