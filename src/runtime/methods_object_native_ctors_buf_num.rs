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
            .flat_map(|a| match a {
                Value::Int(i) => vec![Value::Int(*i)],
                Value::Array(items, ..) => items.to_vec(),
                Value::Seq(items) => items.to_vec(),
                Value::Slip(items) => items.to_vec(),
                Value::Range(start, end) => (*start..=*end).map(Value::Int).collect(),
                Value::RangeExcl(start, end) => (*start..*end).map(Value::Int).collect(),
                Value::Instance {
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
                    if let Some(Value::Array(items, ..)) = attributes.as_map().get("bytes") {
                        items.to_vec()
                    } else {
                        Vec::new()
                    }
                }
                Value::BigInt(_) => vec![a.clone()],
                other => vec![Value::Int(to_int(other))],
            })
            .collect();
        // Mask values to unsigned range based on element size. For uint64, use
        // BigInt-aware conversion to preserve values > i64::MAX.
        let byte_vals: Vec<Value> = raw_vals
            .into_iter()
            .map(|v| {
                if cn.contains("64") {
                    let u = match &v {
                        Value::BigInt(n) => {
                            use num_traits::ToPrimitive;
                            n.as_ref().to_u64().unwrap_or(to_int(&v) as u64)
                        }
                        _ => to_int(&v) as u64,
                    };
                    Value::Int(u as i64)
                } else if cn.contains("32") {
                    Value::Int(to_int(&v) as u32 as i64)
                } else if cn.contains("16") {
                    Value::Int(to_int(&v) as u16 as i64)
                } else {
                    Value::Int(to_int(&v) as u8 as i64)
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
            .flat_map(|a| match a {
                Value::Int(i) => vec![Value::Int(*i)],
                Value::Array(items, ..) => items.to_vec(),
                Value::Seq(items) | Value::Slip(items) => items.to_vec(),
                Value::Range(start, end) => (*start..=*end).map(Value::Int).collect(),
                Value::RangeExcl(start, end) => (*start..*end).map(Value::Int).collect(),
                Value::Instance {
                    class_name: cn,
                    attributes: ia,
                    ..
                } if crate::runtime::utils::is_buf_or_blob_class(&cn.resolve()) => {
                    if let Some(Value::Array(items, ..)) = ia.as_map().get("bytes") {
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
            match a {
                Value::Array(items, ..) => flat_args.extend(items.iter().cloned()),
                Value::Seq(items) | Value::Slip(items) => {
                    flat_args.extend(items.iter().cloned());
                }
                Value::Range(start, end) => {
                    flat_args.extend((*start..=*end).map(Value::Int));
                }
                Value::RangeExcl(start, end) => {
                    flat_args.extend((*start..*end).map(Value::Int));
                }
                other => flat_args.push(other.clone()),
            }
        }
        let text: String = flat_args
            .iter()
            .filter_map(|a| {
                let cp = match a {
                    Value::Int(i) => *i as u32,
                    Value::Num(f) => *f as u32,
                    other => other.to_string_value().parse::<u32>().unwrap_or(0),
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
        match v {
            Some(Value::Int(i)) => *i as f64,
            Some(Value::Num(f)) => *f,
            Some(Value::Rat(n, d)) if *d != 0 => *n as f64 / *d as f64,
            Some(v) => to_float_value(v).unwrap_or(0.0),
            _ => 0.0,
        }
    }

    /// Build a `Complex` from `.new` arguments as pure data: `Complex.new(re,
    /// im)` -> `re + im*i`, each component coerced via `complex_component`
    /// (missing components default to `0`). Behaviour matches the interpreter
    /// (which is more lenient than raku — raku requires exactly two `Real`
    /// arguments, mutsu accepts 0/1/2 — a pre-existing divergence preserved here).
    pub(crate) fn build_native_complex_value(args: &[Value]) -> Value {
        Value::Complex(
            Self::complex_component(args.first()),
            Self::complex_component(args.get(1)),
        )
    }

    /// Build an `Int` from `.new(value)` as pure data: `to_int`-coerce the
    /// argument (default `0`). A type-object argument is an error, matching the
    /// interpreter's basic-type `.new` arm.
    pub(crate) fn build_native_int_value(args: &[Value]) -> Result<Value, RuntimeError> {
        if matches!(args.first(), Some(Value::Package(_))) {
            return Err(RuntimeError::new("Cannot convert type object to Int"));
        }
        Ok(Value::Int(args.first().map_or(0, crate::runtime::to_int)))
    }

    /// Build a `Num` from `.new(value)` as pure data: coerce the argument to
    /// f64 (default `0e0`). A type-object or non-coercible argument is an error,
    /// matching the interpreter's basic-type `.new` arm.
    pub(crate) fn build_native_num_value(args: &[Value]) -> Result<Value, RuntimeError> {
        if let Some(arg) = args.first() {
            if matches!(arg, Value::Package(_)) {
                return Err(RuntimeError::new(
                    "Cannot coerce to Num: no .Num method found",
                ));
            }
            match crate::runtime::to_float_value(arg) {
                Some(f) => Ok(Value::Num(f)),
                None => Err(RuntimeError::new(
                    "Cannot coerce to Num: no .Num method found",
                )),
            }
        } else {
            Ok(Value::Num(0.0))
        }
    }

    /// Build a `Rat` from `.new(numerator, denominator)` as pure data
    /// (defaults `0/1`). A `BigInt` argument routes through `make_big_rat` to
    /// avoid truncation; otherwise the components are `to_int`-coerced.
    pub(crate) fn build_native_rat_value(args: &[Value]) -> Value {
        use num_bigint::BigInt;
        let has_big = args.iter().take(2).any(|v| matches!(v, Value::BigInt(_)));
        if has_big {
            let a = match args.first() {
                Some(Value::BigInt(bi)) => (**bi).clone(),
                Some(v) => BigInt::from(to_int(v)),
                None => BigInt::from(0),
            };
            let b = match args.get(1) {
                Some(Value::BigInt(bi)) => (**bi).clone(),
                Some(v) => BigInt::from(to_int(v)),
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
        let a = match args.first() {
            Some(Value::BigInt(bi)) => (**bi).clone(),
            Some(v) => BigInt::from(to_int(v)),
            None => BigInt::from(0),
        };
        let b = match args.get(1) {
            Some(Value::BigInt(bi)) => (**bi).clone(),
            Some(v) => BigInt::from(to_int(v)),
            None => BigInt::from(1),
        };
        match make_big_fat_rat(a, b) {
            Value::Rat(n, d) => Value::FatRat(n, d),
            Value::BigRat(n, d) => Value::BigRat(n, d),
            other => other,
        }
    }

    /// Build a `Pair` from `.new(:key, :value)` or `.new(key, value)` as pure
    /// data. A `Str` key uses `Pair` (string-keyed); any other key type uses
    /// `ValuePair`, mirroring the `=>` operator and positional `Pair.new`.
    pub(crate) fn build_native_pair_value(args: &[Value]) -> Value {
        let mut named_key: Option<Value> = None;
        let mut named_value: Option<Value> = None;
        let mut positional = Vec::new();
        for a in args {
            match a {
                Value::Pair(k, v) if k == "key" => named_key = Some((**v).clone()),
                Value::Pair(k, v) if k == "value" => named_value = Some((**v).clone()),
                _ => positional.push(a.clone()),
            }
        }
        let (key, value) = if named_key.is_some() || named_value.is_some() {
            (
                named_key.unwrap_or(Value::Nil),
                named_value.unwrap_or(Value::Nil),
            )
        } else {
            (
                positional.first().cloned().unwrap_or(Value::Nil),
                positional.get(1).cloned().unwrap_or(Value::Nil),
            )
        };
        match &key {
            Value::Str(_) => Value::Pair(key.to_string_value(), Box::new(value)),
            _ => Value::ValuePair(Box::new(key), Box::new(value)),
        }
    }

    /// Build an `IterationBuffer` from `.new(...)` as pure data: flatten each
    /// argument (an Array/Seq/Slip is spread, another IterationBuffer's items
    /// are taken, anything else is a single element) into the buffer's
    /// `__mutsu_iterationbuffer_items` array.
    pub(crate) fn build_native_iterationbuffer_value(class_name: Symbol, args: &[Value]) -> Value {
        let mut items = Vec::new();
        for arg in args {
            match arg {
                Value::Array(vals, ..) => items.extend(vals.iter().cloned()),
                Value::Seq(vals) | Value::Slip(vals) => items.extend(vals.iter().cloned()),
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "IterationBuffer" => {
                    match attributes.as_map().get("__mutsu_iterationbuffer_items") {
                        Some(Value::Array(vals, ..)) => items.extend(vals.iter().cloned()),
                        Some(Value::Seq(vals)) | Some(Value::Slip(vals)) => {
                            items.extend(vals.iter().cloned())
                        }
                        _ => {}
                    }
                }
                other => items.push(other.clone()),
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
