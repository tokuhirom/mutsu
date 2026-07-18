use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) fn is_native_buf_constructible(cn: &str) -> bool {
        cn != "utf8" && cn != "utf16" && crate::runtime::utils::is_buf_or_blob_class(cn)
    }

    /// Build a `Buf`/`Blob` instance from `.new` arguments as pure data: flatten
    /// each argument into a byte sequence, mask every value to the element width
    /// inferred from the type name (`uint8`/`16`/`32`/`64`), and tag the
    /// canonical parametric class name (`buf8` -> `Buf[uint8]`, …). Touches no
    /// env / registry, so the VM constructs these directly (see
    /// `is_native_buf_constructible`) without entering generic dispatch, while
    /// `dispatch_new`'s Buf/Blob arm calls the same helper — keeping the two
    /// byte-identical. `utf8`/`utf16` are intentionally NOT handled here.
    pub(crate) fn build_native_buf_value(class_name: Symbol, args: &[Value]) -> Value {
        let cn = class_name.resolve();
        let raw_vals: Vec<Value> = args
            .iter()
            .flat_map(|a| match a.view() {
                ValueView::Int(i) => vec![Value::int(i)],
                ValueView::Array(items, ..) => items.to_vec(),
                ValueView::Seq(items) => items.to_vec(),
                ValueView::Slip(items) => items.to_vec(),
                ValueView::Range(start, end) => (start..=end).map(Value::int).collect(),
                ValueView::RangeExcl(start, end) => (start..end).map(Value::int).collect(),
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Buf"
                    || class_name == "Blob"
                    || class_name == "utf8"
                    || class_name == "utf16"
                    || class_name.resolve().starts_with("Buf[")
                    || class_name.resolve().starts_with("Blob[")
                    || class_name.resolve().starts_with("buf")
                    || class_name.resolve().starts_with("blob") =>
                {
                    if let Some(ValueView::Array(items, ..)) =
                        attributes.as_map().get("bytes").map(Value::view)
                    {
                        items.to_vec()
                    } else {
                        Vec::new()
                    }
                }
                ValueView::BigInt(_) => vec![a.clone()],
                _ => vec![Value::int(to_int(a))],
            })
            .collect();
        // Mask values to unsigned range based on element size. For uint64, use
        // BigInt-aware conversion to preserve values > i64::MAX.
        let byte_vals: Vec<Value> = raw_vals
            .into_iter()
            .map(|v| {
                if cn.contains("64") {
                    let u = match v.view() {
                        ValueView::BigInt(n) => {
                            use num_traits::ToPrimitive;
                            n.as_ref().to_u64().unwrap_or(to_int(&v) as u64)
                        }
                        _ => to_int(&v) as u64,
                    };
                    Value::int(u as i64)
                } else if cn.contains("32") {
                    Value::int(to_int(&v) as u32 as i64)
                } else if cn.contains("16") {
                    Value::int(to_int(&v) as u16 as i64)
                } else {
                    Value::int(to_int(&v) as u8 as i64)
                }
            })
            .collect();
        let mut attrs = HashMap::new();
        attrs.insert("bytes".to_string(), Value::array(byte_vals));
        // Normalize short buf/blob names to canonical forms.
        let canonical_name = match cn.as_str() {
            "buf8" => Symbol::intern("Buf[uint8]"),
            "buf16" => Symbol::intern("Buf[uint16]"),
            "buf32" => Symbol::intern("Buf[uint32]"),
            "buf64" => Symbol::intern("Buf[uint64]"),
            "blob8" => Symbol::intern("Blob[uint8]"),
            "blob16" => Symbol::intern("Blob[uint16]"),
            "blob32" => Symbol::intern("Blob[uint32]"),
            "blob64" => Symbol::intern("Blob[uint64]"),
            _ => class_name,
        };
        Value::make_instance(canonical_name, attrs)
    }

    /// Build a `utf8`/`utf16` instance from `.new` arguments as pure data:
    /// flatten each argument into a code-unit sequence (no masking) and store it
    /// as the `bytes` attribute. Unlike `build_native_buf_value` this keeps the
    /// literal class name and drops arguments it cannot turn into code units
    /// (the interpreter's `utf8`/`utf16` arm did the same), so the two stay
    /// byte-identical.
    pub(crate) fn build_native_utf_value(class_name: Symbol, args: &[Value]) -> Value {
        let elems: Vec<Value> = args
            .iter()
            .flat_map(|a| match a.view() {
                ValueView::Int(i) => vec![Value::int(i)],
                ValueView::Array(items, ..) => items.to_vec(),
                ValueView::Seq(items) | ValueView::Slip(items) => items.to_vec(),
                ValueView::Range(start, end) => (start..=end).map(Value::int).collect(),
                ValueView::RangeExcl(start, end) => (start..end).map(Value::int).collect(),
                ValueView::Instance {
                    class_name: cn,
                    attributes: ia,
                    ..
                } if crate::runtime::utils::is_buf_or_blob_class(&cn.resolve()) => {
                    if let Some(ValueView::Array(items, ..)) =
                        ia.as_map().get("bytes").map(Value::view)
                    {
                        items.to_vec()
                    } else {
                        vec![]
                    }
                }
                _ => vec![],
            })
            .collect();
        let mut attrs = HashMap::new();
        attrs.insert("bytes".to_string(), Value::array(elems));
        Value::make_instance(class_name, attrs)
    }

    /// Build a `Uni` value from `.new` arguments as pure data: flatten each
    /// argument into a sequence of codepoints and collect them as a string.
    /// (Flattening now also covers `Range`s — `Uni.new(65..67)` -> "ABC" — which
    /// the old interpreter arm missed, leaving it to stringify the whole Range;
    /// the shared helper fixes that to match raku and the sibling `utf8` arm.)
    pub(crate) fn build_native_uni_value(args: &[Value]) -> Value {
        let mut flat_args: Vec<Value> = Vec::new();
        for a in args {
            match a.view() {
                ValueView::Array(items, ..) => flat_args.extend(items.iter().cloned()),
                ValueView::Seq(items) | ValueView::Slip(items) => {
                    flat_args.extend(items.iter().cloned());
                }
                ValueView::Range(start, end) => {
                    flat_args.extend((start..=end).map(Value::int));
                }
                ValueView::RangeExcl(start, end) => {
                    flat_args.extend((start..end).map(Value::int));
                }
                _ => flat_args.push(a.clone()),
            }
        }
        let text: String = flat_args
            .iter()
            .filter_map(|a| {
                let cp = match a.view() {
                    ValueView::Int(i) => i as u32,
                    ValueView::Num(f) => f as u32,
                    _ => a.to_string_value().parse::<u32>().unwrap_or(0),
                };
                char::from_u32(cp)
            })
            .collect();
        Value::uni(String::new(), text)
    }

    /// Coerce one `Complex.new` positional argument to its `f64` component
    /// (Int/Num/Rat directly, anything else via `to_float_value`). Mirrors the
    /// interpreter's `Complex` constructor arm exactly.
    fn complex_component(v: Option<&Value>) -> f64 {
        match v.map(Value::view) {
            Some(ValueView::Int(i)) => i as f64,
            Some(ValueView::Num(f)) => f,
            Some(ValueView::Rat(n, d)) if d != 0 => n as f64 / d as f64,
            Some(_) => to_float_value(v.unwrap()).unwrap_or(0.0),
            _ => 0.0,
        }
    }

    /// Build a `Complex` from `.new` arguments as pure data: `Complex.new(re,
    /// im)` -> `re + im*i`, each component coerced via `complex_component`
    /// (missing components default to `0`). Behaviour matches the interpreter
    /// (which is more lenient than raku — raku requires exactly two `Real`
    /// arguments, mutsu accepts 0/1/2 — a pre-existing divergence preserved here).
    pub(crate) fn build_native_complex_value(args: &[Value]) -> Value {
        Value::complex(
            Self::complex_component(args.first()),
            Self::complex_component(args.get(1)),
        )
    }

    /// Build an `Int` from `.new(value)` as pure data: `to_int`-coerce the
    /// argument (default `0`). A type-object argument is an error, matching the
    /// interpreter's basic-type `.new` arm.
    pub(crate) fn build_native_int_value(args: &[Value]) -> Result<Value, RuntimeError> {
        // Int.new accepts at most one positional (`new()` / `new($value)`); named
        // args are ignored. Two or more positionals resolve to no candidate and
        // must throw X::Multi::NoMatch (roast .../multi-no-match.t) rather than
        // silently taking the first.
        let positional: Vec<&Value> = args
            .iter()
            .filter(|a| !matches!(a.view(), ValueView::Pair(..)))
            .collect();
        if positional.len() > 1 {
            return Err(super::methods_signature_errors::make_multi_no_match_error(
                "new",
            ));
        }
        if matches!(
            positional.first().map(|v| v.view()),
            Some(ValueView::Package(_))
        ) {
            return Err(RuntimeError::new("Cannot convert type object to Int"));
        }
        Ok(Value::int(
            positional.first().map_or(0, |v| crate::runtime::to_int(v)),
        ))
    }

    /// Build a `Num` from `.new(value)` as pure data: coerce the argument to
    /// f64 (default `0e0`). A type-object or non-coercible argument is an error,
    /// matching the interpreter's basic-type `.new` arm.
    pub(crate) fn build_native_num_value(args: &[Value]) -> Result<Value, RuntimeError> {
        if let Some(arg) = args.first() {
            if matches!(arg.view(), ValueView::Package(_)) {
                return Err(RuntimeError::new(
                    "Cannot coerce to Num: no .Num method found",
                ));
            }
            match crate::runtime::to_float_value(arg) {
                Some(f) => Ok(Value::num(f)),
                None => Err(RuntimeError::new(
                    "Cannot coerce to Num: no .Num method found",
                )),
            }
        } else {
            Ok(Value::num(0.0))
        }
    }

    /// Build a `Rat` from `.new(numerator, denominator)` as pure data
    /// (defaults `0/1`). A `BigInt` argument routes through `make_big_rat` to
    /// avoid truncation; otherwise the components are `to_int`-coerced.
    pub(crate) fn build_native_rat_value(args: &[Value]) -> Value {
        use num_bigint::BigInt;
        let has_big = args
            .iter()
            .take(2)
            .any(|v| matches!(v.view(), ValueView::BigInt(_)));
        if has_big {
            let a = match args.first().map(Value::view) {
                Some(ValueView::BigInt(bi)) => (**bi).clone(),
                Some(_) => BigInt::from(to_int(args.first().unwrap())),
                None => BigInt::from(0),
            };
            let b = match args.get(1).map(Value::view) {
                Some(ValueView::BigInt(bi)) => (**bi).clone(),
                Some(_) => BigInt::from(to_int(args.get(1).unwrap())),
                None => BigInt::from(1),
            };
            return crate::value::make_big_rat(a, b);
        }
        let a = args.first().map(to_int).unwrap_or(0);
        let b = args.get(1).map(to_int).unwrap_or(1);
        make_rat(a, b)
    }

    /// Build a `FatRat` from `.new(numerator, denominator)` as pure data
    /// (defaults `0/1`), always via the BigInt path (`make_big_fat_rat`).
    pub(crate) fn build_native_fatrat_value(args: &[Value]) -> Value {
        use crate::value::make_big_fat_rat;
        use num_bigint::BigInt;
        let a = match args.first().map(Value::view) {
            Some(ValueView::BigInt(bi)) => (**bi).clone(),
            Some(_) => BigInt::from(to_int(args.first().unwrap())),
            None => BigInt::from(0),
        };
        let b = match args.get(1).map(Value::view) {
            Some(ValueView::BigInt(bi)) => (**bi).clone(),
            Some(_) => BigInt::from(to_int(args.get(1).unwrap())),
            None => BigInt::from(1),
        };
        let r = make_big_fat_rat(a, b);
        if let ValueView::Rat(n, d) = r.view() {
            return Value::fat_rat_raw(n, d);
        }
        r
    }

    /// Build a `Pair` from `.new(:key, :value)` or `.new(key, value)` as pure
    /// data. A `Str` key uses `Pair` (string-keyed); any other key type uses
    /// `ValuePair`, mirroring the `=>` operator and positional `Pair.new`.
    pub(crate) fn build_native_pair_value(args: &[Value]) -> Result<Value, RuntimeError> {
        let mut named_key: Option<Value> = None;
        let mut named_value: Option<Value> = None;
        let mut positional = Vec::new();
        for a in args {
            match a.view() {
                ValueView::Pair(k, v) if k == "key" => named_key = Some(v.clone()),
                ValueView::Pair(k, v) if k == "value" => named_value = Some(v.clone()),
                ValueView::Pair(..) => { /* extra named args are ignored by bless */ }
                _ => positional.push(a.clone()),
            }
        }
        // Pair.new has two candidates: `($key, $value)` (exactly two positionals)
        // and `(:$key, :$value)` (both named, no positionals). Anything else —
        // one/three positionals, a lone :key or :value — resolves to no
        // candidate and must throw X::Multi::NoMatch rather than silently
        // filling in Nil (roast APPENDICES/.../multi-no-match.t).
        let (key, value) = if positional.len() == 2 {
            (positional[0].clone(), positional[1].clone())
        } else if positional.is_empty() && named_key.is_some() && named_value.is_some() {
            (named_key.unwrap(), named_value.unwrap())
        } else {
            return Err(super::methods_signature_errors::make_multi_no_match_error(
                "new",
            ));
        };
        if matches!(key.view(), ValueView::Str(_)) {
            Ok(Value::pair(key.to_string_value(), value))
        } else {
            Ok(Value::value_pair(key, value))
        }
    }

    /// Build an `IterationBuffer` from `.new(...)` as pure data: flatten each
    /// argument (an Array/Seq/Slip is spread, another IterationBuffer's items
    /// are taken, anything else is a single element) into the buffer's
    /// `__mutsu_iterationbuffer_items` array.
    pub(crate) fn build_native_iterationbuffer_value(class_name: Symbol, args: &[Value]) -> Value {
        let mut items = Vec::new();
        for arg in args {
            match arg.view() {
                ValueView::Array(vals, ..) => items.extend(vals.iter().cloned()),
                ValueView::Seq(vals) | ValueView::Slip(vals) => items.extend(vals.iter().cloned()),
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "IterationBuffer" => {
                    match attributes
                        .as_map()
                        .get("__mutsu_iterationbuffer_items")
                        .map(Value::view)
                    {
                        Some(ValueView::Array(vals, ..)) => items.extend(vals.iter().cloned()),
                        Some(ValueView::Seq(vals)) | Some(ValueView::Slip(vals)) => {
                            items.extend(vals.iter().cloned())
                        }
                        _ => {}
                    }
                }
                _ => items.push(arg.clone()),
            }
        }
        let mut attrs = HashMap::new();
        attrs.insert(
            "__mutsu_iterationbuffer_items".to_string(),
            Value::real_array(items),
        );
        Value::make_instance(class_name, attrs)
    }
}
