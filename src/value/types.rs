use super::*;

impl Value {
    /// Type-strict structural equivalence (Raku `eqv` operator).
    /// See raku-doc: Language/operators.rakudoc "infix eqv"
    ///
    /// Returns True if two arguments are structurally the same, i.e. from the
    /// same type and (recursively) contain equivalent values.
    /// Unlike PartialEq (used for `==`), this does NOT allow cross-type comparisons:
    ///   1 eqv 1.0  → False  (Int vs Num)
    ///   [1,2] eqv (1,2)  → False  (Array vs List)
    pub(crate) fn eqv(&self, other: &Self) -> bool {
        match (self, other) {
            // Arrays/Lists: must be same container type (Array vs List) and recursively eqv
            // eqv ignores Scalar wrapping — only Array vs List distinction matters
            (Value::Array(a, a_kind), Value::Array(b, b_kind)) => {
                a_kind.is_real_array() == b_kind.is_real_array()
                    && a.len() == b.len()
                    && a.iter().zip(b.iter()).all(|(x, y)| x.eqv(y))
            }
            // Hashes: recursively use eqv for values
            (Value::Hash(a), Value::Hash(b)) => {
                a.len() == b.len() && a.iter().all(|(k, v)| b.get(k).is_some_and(|bv| v.eqv(bv)))
            }
            // Pairs: recursively use eqv for values (Pair and ValuePair are equivalent)
            (Value::Pair(ak, av), Value::Pair(bk, bv)) => ak == bk && av.eqv(bv),
            (Value::ValuePair(ak, av), Value::ValuePair(bk, bv)) => ak.eqv(bk) && av.eqv(bv),
            (Value::Pair(ak, av), Value::ValuePair(bk, bv)) => {
                matches!(bk.as_ref(), Value::Str(s) if s.as_str() == ak) && av.eqv(bv)
            }
            (Value::ValuePair(ak, av), Value::Pair(bk, bv)) => {
                matches!(ak.as_ref(), Value::Str(s) if s.as_str() == bk) && av.eqv(bv)
            }
            // Captures: recursively use eqv for positional and named elements
            (
                Value::Capture {
                    positional: ap,
                    named: an,
                },
                Value::Capture {
                    positional: bp,
                    named: bn,
                },
            ) => {
                ap.len() == bp.len()
                    && ap.iter().zip(bp.iter()).all(|(x, y)| x.eqv(y))
                    && an.len() == bn.len()
                    && an
                        .iter()
                        .all(|(k, v)| bn.get(k).is_some_and(|bv| v.eqv(bv)))
            }
            // Slips: recursively use eqv for elements
            (Value::Slip(a), Value::Slip(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.eqv(y))
            }
            // Seqs: recursively use eqv for elements
            (Value::Seq(a), Value::Seq(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.eqv(y))
            }
            // Num: use bit-exact comparison to distinguish signed zeros,
            // but treat all NaN bit patterns as identical (Raku considers all NaN equal).
            (Value::Num(a), Value::Num(b)) => {
                if a.is_nan() && b.is_nan() {
                    true
                } else {
                    a.to_bits() == b.to_bits()
                }
            }
            // Complex: use bit-exact comparison for both components,
            // treating all NaN bit patterns as identical.
            (Value::Complex(ar, ai), Value::Complex(br, bi)) => {
                let re_eq = if ar.is_nan() && br.is_nan() {
                    true
                } else {
                    ar.to_bits() == br.to_bits()
                };
                let im_eq = if ai.is_nan() && bi.is_nan() {
                    true
                } else {
                    ai.to_bits() == bi.to_bits()
                };
                re_eq && im_eq
            }
            // Same-type scalar comparisons delegate to PartialEq
            // BigInt: both sides BigInt — use PartialEq
            (Value::BigInt(_), Value::BigInt(_)) => self == other,
            // Cross-representation Int/BigInt: compare numerically
            (Value::Int(a), Value::BigInt(b)) => NumBigInt::from(*a) == **b,
            (Value::BigInt(a), Value::Int(b)) => **a == NumBigInt::from(*b),
            // Rat/FatRat: structural equality (n == n, d == d), including NaN (0/0)
            (Value::Rat(n1, d1), Value::Rat(n2, d2))
            | (Value::FatRat(n1, d1), Value::FatRat(n2, d2)) => n1 == n2 && d1 == d2,
            (Value::Int(_), Value::Int(_))
            | (Value::Str(_), Value::Str(_))
            | (Value::Bool(_), Value::Bool(_))
            | (Value::BigRat(_, _), Value::BigRat(_, _))
            | (Value::Set(_, _), Value::Set(_, _))
            | (Value::Bag(_, _), Value::Bag(_, _))
            | (Value::Mix(_, _), Value::Mix(_, _))
            | (Value::Enum { .. }, Value::Enum { .. })
            | (Value::Regex(_), Value::Regex(_))
            | (Value::RegexWithAdverbs { .. }, Value::RegexWithAdverbs { .. })
            | (Value::Routine { .. }, Value::Routine { .. })
            | (Value::Sub(_), Value::Sub(_)) => self == other,
            // Signature instances: compare by .raku string (structural equality)
            (
                Value::Instance {
                    class_name: cn_a, ..
                },
                Value::Instance {
                    class_name: cn_b, ..
                },
            ) if cn_a == "Signature" && cn_b == "Signature" => {
                let raku_a = if let Value::Instance { attributes, .. } = self {
                    attributes.get("raku").map(|v| v.to_string_value())
                } else {
                    None
                };
                let raku_b = if let Value::Instance { attributes, .. } = other {
                    attributes.get("raku").map(|v| v.to_string_value())
                } else {
                    None
                };
                raku_a == raku_b
            }
            (
                Value::Instance {
                    class_name: cn_a,
                    attributes: a_attrs,
                    ..
                },
                Value::Instance {
                    class_name: cn_b,
                    attributes: b_attrs,
                    ..
                },
            ) if cn_a == cn_b
                && a_attrs.contains_key("year")
                && a_attrs.contains_key("month")
                && a_attrs.contains_key("day")
                && a_attrs.contains_key("hour")
                && a_attrs.contains_key("minute")
                && a_attrs.contains_key("second")
                && a_attrs.contains_key("timezone")
                && b_attrs.contains_key("year")
                && b_attrs.contains_key("month")
                && b_attrs.contains_key("day")
                && b_attrs.contains_key("hour")
                && b_attrs.contains_key("minute")
                && b_attrs.contains_key("second")
                && b_attrs.contains_key("timezone") =>
            {
                let (ay, am, ad, ah, amin, asec, atz) =
                    crate::builtins::methods_0arg::temporal::datetime_attrs(a_attrs);
                let (by, bm, bd, bh, bmin, bsec, btz) =
                    crate::builtins::methods_0arg::temporal::datetime_attrs(b_attrs);
                ay == by
                    && am == bm
                    && ad == bd
                    && ah == bh
                    && amin == bmin
                    && atz == btz
                    && (asec - bsec).abs() < 1e-6
            }
            // Date instances: compare only year/month/day (ignore formatter attrs)
            (
                Value::Instance {
                    class_name: cn_a,
                    attributes: a_attrs,
                    ..
                },
                Value::Instance {
                    class_name: cn_b,
                    attributes: b_attrs,
                    ..
                },
            ) if cn_a == cn_b
                && a_attrs.contains_key("year")
                && a_attrs.contains_key("month")
                && a_attrs.contains_key("day")
                && !a_attrs.contains_key("hour")
                && b_attrs.contains_key("year")
                && b_attrs.contains_key("month")
                && b_attrs.contains_key("day")
                && !b_attrs.contains_key("hour") =>
            {
                let (ay, am, ad) = crate::builtins::methods_0arg::temporal::date_attrs(a_attrs);
                let (by, bm, bd) = crate::builtins::methods_0arg::temporal::date_attrs(b_attrs);
                ay == by && am == bm && ad == bd
            }
            // Other Instance types: use identity comparison
            (Value::Instance { .. }, Value::Instance { .. }) => self == other,
            // Nil and Package("Any") are eqv: both represent the undefined type object Any.
            // This matters for containerized contexts (e.g. hash values).
            (Value::Nil, Value::Package(name)) | (Value::Package(name), Value::Nil)
                if name == "Any" =>
            {
                true
            }
            (Value::Range(_, _), Value::Range(_, _))
            | (Value::RangeExcl(_, _), Value::RangeExcl(_, _))
            | (Value::RangeExclStart(_, _), Value::RangeExclStart(_, _))
            | (Value::RangeExclBoth(_, _), Value::RangeExclBoth(_, _))
            | (Value::GenericRange { .. }, Value::GenericRange { .. })
            | (Value::LazyList(_), Value::LazyList(_))
            | (Value::Version { .. }, Value::Version { .. })
            | (Value::Nil, Value::Nil)
            | (Value::Package(_), Value::Package(_))
            | (Value::CompUnitDepSpec { .. }, Value::CompUnitDepSpec { .. })
            | (Value::Junction { .. }, Value::Junction { .. })
            | (Value::Promise(_), Value::Promise(_))
            | (Value::Channel(_), Value::Channel(_))
            | (Value::Uni { .. }, Value::Uni { .. }) => self == other,
            // Mixin (allomorphs): compare both base values and mixin maps with eqv
            (Value::Mixin(a, a_mix), Value::Mixin(b, b_mix)) => {
                if !a.eqv(b) {
                    return false;
                }
                // Compare mixin maps (e.g. Str part of allomorphs)
                if a_mix.len() != b_mix.len() {
                    return false;
                }
                a_mix
                    .iter()
                    .all(|(k, v)| b_mix.get(k).is_some_and(|bv| v.eqv(bv)))
            }
            // Cross-type comparisons always return false for eqv
            _ => false,
        }
    }

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
            attributes.get("handled").is_some_and(|v| v.truthy())
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
                    return match attributes.get("exitcode") {
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
                    return match attributes.get("bytes") {
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
            Value::Seq(items) => !items.is_empty(),
            Value::LazyList(_) => true,
            Value::Promise(p) => p.is_resolved(),
            Value::Channel(_) => true,
            Value::Regex(_) | Value::RegexWithAdverbs { .. } => true,
            Value::Version { .. } => true,
            Value::Nil => false,
            Value::Whatever => true,
            Value::HyperWhatever => true,
            Value::Capture { positional, named } => !positional.is_empty() || !named.is_empty(),
            Value::Uni { text, .. } => !text.is_empty(),
            Value::Mixin(inner, mixins) => {
                if let Some(bool_val) = mixins.get("Bool") {
                    bool_val.truthy()
                } else {
                    inner.truthy()
                }
            }
            Value::Proxy { .. } => true,
            Value::CustomType { .. } => false,
            Value::CustomTypeInstance { .. } => true,
            Value::Scalar(inner) => inner.truthy(),
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
            Value::HashSlotRef { .. } => self.hash_slot_read().truthy(),
        }
    }

    /// Check if this value is an instance of the given type name (Raku `isa` operator).
    pub(crate) fn isa_check(&self, type_name: &str) -> bool {
        // For Instance/Package, extract name as owned String for later comparison
        let owned_name: Option<String> = match self {
            Value::Instance { class_name, .. } => Some(class_name.resolve()),
            Value::Package(name) => Some(name.resolve()),
            _ => None,
        };
        let my_type = match self {
            Value::Int(_) | Value::BigInt(_) => "Int",
            Value::Num(_) => "Num",
            Value::Str(_) => "Str",
            Value::Bool(_) => "Bool",
            Value::Rat(_, _) => "Rat",
            Value::FatRat(_, _) => "FatRat",
            Value::BigRat(_, _) => "Rat",
            Value::Complex(_, _) => "Complex",
            Value::Array(..) | Value::LazyList(_) => "Array",
            Value::Seq(_) => "Seq",
            Value::Hash(_) => "Hash",
            Value::Set(_, is_mutable) => {
                if *is_mutable {
                    "SetHash"
                } else {
                    "Set"
                }
            }
            Value::Bag(_, is_mutable) => {
                if *is_mutable {
                    "BagHash"
                } else {
                    "Bag"
                }
            }
            Value::Mix(_, is_mutable) => {
                if *is_mutable {
                    "MixHash"
                } else {
                    "Mix"
                }
            }
            Value::Pair(_, _) | Value::ValuePair(_, _) => "Pair",
            Value::Range(_, _)
            | Value::RangeExcl(_, _)
            | Value::RangeExclStart(_, _)
            | Value::RangeExclBoth(_, _)
            | Value::GenericRange { .. } => "Range",
            Value::Nil => "Nil",
            Value::Instance { .. } | Value::Package(_) => owned_name.as_deref().unwrap(),
            Value::Enum { enum_type, .. } => {
                return enum_type.resolve() == type_name;
            }
            Value::Sub(data) => match data.env.get("__mutsu_callable_type") {
                Some(Value::Str(kind)) if kind.as_str() == "Method" => "Method",
                Some(Value::Str(kind)) if kind.as_str() == "WhateverCode" => "WhateverCode",
                _ => "Sub",
            },
            Value::WeakSub(_) => "Sub",
            Value::Routine {
                is_regex: false, ..
            } => "Sub",
            Value::Regex(_)
            | Value::RegexWithAdverbs { .. }
            | Value::Routine { is_regex: true, .. } => "Regex",
            Value::Junction { .. } => "Junction",
            Value::Version { .. } => "Version",
            Value::Slip(_) => "Slip",
            Value::Promise(p) => {
                let cn = p.class_name();
                if cn != "Promise" && cn == type_name {
                    return true;
                }
                // Also check if type_name is "Promise" (parent)
                if type_name == "Promise" {
                    return true;
                }
                "Promise"
            }
            Value::Channel(_) => "Channel",
            Value::CompUnitDepSpec { .. } => "CompUnit::DependencySpecification",
            Value::Whatever => "Whatever",
            Value::HyperWhatever => "HyperWhatever",
            Value::Capture { .. } => "Capture",
            Value::Uni { form, .. } => form.as_str(),
            Value::Mixin(inner, mixins) => {
                // Check allomorphic type names (IntStr, NumStr, RatStr, ComplexStr, Allomorph)
                if matches!(
                    type_name,
                    "IntStr" | "NumStr" | "RatStr" | "ComplexStr" | "Allomorph"
                ) {
                    if let Some(allo_name) = allomorph_type_name(inner, mixins)
                        && (type_name == "Allomorph" || type_name == allo_name)
                    {
                        return true;
                    }
                    return false;
                }
                if inner.isa_check(type_name) {
                    return true;
                }
                // Also check mixin type keys (e.g., allomorphic "Str" mixin)
                return mixins.contains_key(type_name);
            }
            Value::Proxy { .. } => "Proxy",
            Value::ParametricRole { base_name, .. } => {
                return base_name.resolve() == type_name;
            }
            Value::CustomType { name, .. } => {
                return name.resolve() == type_name;
            }
            Value::CustomTypeInstance { type_name: tn, .. } => {
                return tn.resolve() == type_name;
            }
            Value::Scalar(inner) => return inner.isa_check(type_name),
            Value::LazyThunk(thunk_data) => {
                let cache = thunk_data.cache.lock().unwrap();
                if let Some(ref cached) = *cache {
                    return cached.isa_check(type_name);
                }
                "Scalar" // unforced lazy thunk
            }
            Value::LazyIoLines { .. } => "Seq",
            Value::HashSlotRef { .. } => return self.hash_slot_read().isa_check(type_name),
        };
        if my_type == type_name {
            return true;
        }
        // Perl6::Metamodel:: and Metamodel:: are equivalent namespaces
        if let Some(short) = my_type.strip_prefix("Perl6::")
            && short == type_name
        {
            return true;
        }
        if let Some(short) = type_name.strip_prefix("Perl6::")
            && short == my_type
        {
            return true;
        }
        // Check type hierarchy
        match type_name {
            "Any" => true,
            "Mu" => true,
            "SetHash" => matches!(self, Value::Set(_, true)),
            "BagHash" => matches!(self, Value::Bag(_, true)),
            "MixHash" => matches!(self, Value::Mix(_, true)),
            "Cool" => matches!(
                self,
                Value::Int(_)
                    | Value::BigInt(_)
                    | Value::Num(_)
                    | Value::Str(_)
                    | Value::Bool(_)
                    | Value::Rat(_, _)
                    | Value::FatRat(_, _)
                    | Value::BigRat(_, _)
                    | Value::Complex(_, _)
                    | Value::Array(..)
                    | Value::Hash(_)
            ),
            "Numeric" => matches!(
                self,
                Value::Int(_)
                    | Value::BigInt(_)
                    | Value::Num(_)
                    | Value::Rat(_, _)
                    | Value::FatRat(_, _)
                    | Value::BigRat(_, _)
                    | Value::Complex(_, _)
            ),
            "Real" => matches!(
                self,
                Value::Int(_)
                    | Value::BigInt(_)
                    | Value::Num(_)
                    | Value::Rat(_, _)
                    | Value::FatRat(_, _)
                    | Value::BigRat(_, _)
            ),
            "Dateish" => matches!(
                self,
                Value::Instance { class_name, .. } if class_name == "Date" || class_name == "DateTime"
            ),
            "FatRat" => matches!(self, Value::FatRat(_, _) | Value::BigRat(_, _)),
            "Int" => matches!(self, Value::Bool(_)),
            "Stringy" => matches!(self, Value::Str(_)),
            "Block" | "Routine" | "Code" | "Callable" => {
                matches!(
                    self,
                    Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
                ) || matches!(
                    self,
                    Value::Package(name)
                        if matches!(name.resolve().as_str(), "Sub" | "Routine" | "Method" | "Block" | "Code")
                )
            }
            "Method" => {
                matches!(
                    self,
                    Value::Sub(data)
                        if matches!(
                            data.env.get("__mutsu_callable_type"),
                            Some(Value::Str(kind)) if kind.as_str() == "Method"
                        )
                ) || matches!(
                    self,
                    Value::Instance { class_name, .. } if class_name == "Method"
                ) || matches!(self, Value::Package(name) if name == "Method")
            }
            "Exception" => {
                if let Value::Instance { class_name, .. } = self {
                    class_name.resolve().starts_with("X::") || class_name == "Exception"
                } else {
                    false
                }
            }
            "X::AdHoc" | "CX::Warn" | "CX::Return" | "X::OS" => {
                if let Value::Instance { class_name, .. } = self {
                    class_name == type_name
                } else {
                    false
                }
            }
            "Seq" | "List" => {
                matches!(self, Value::Array(..) | Value::LazyList(_) | Value::Slip(_))
            }
            "Positional" => {
                matches!(
                    self,
                    Value::Array(..)
                        | Value::LazyList(_)
                        | Value::Range(_, _)
                        | Value::RangeExcl(_, _)
                        | Value::RangeExclStart(_, _)
                        | Value::RangeExclBoth(_, _)
                        | Value::GenericRange { .. }
                        | Value::Capture { .. }
                ) || matches!(
                    self,
                    Value::Package(name)
                        if matches!(
                            name.resolve().as_str(),
                            "Array" | "List" | "Range" | "Buf" | "Blob" | "Capture"
                        )
                )
            }
            "Map" | "Associative" => {
                matches!(
                    self,
                    Value::Hash(_)
                        | Value::Pair(_, _)
                        | Value::ValuePair(_, _)
                        | Value::Set(_, _)
                        | Value::Bag(_, _)
                        | Value::Mix(_, _)
                        | Value::Capture { .. }
                ) || matches!(
                    self,
                    Value::Package(name)
                        if matches!(
                            name.resolve().as_str(),
                            "Hash" | "Map" | "Pair" | "Set" | "Bag" | "Mix" | "QuantHash" | "Capture"
                        )
                )
            }
            "Iterable" => matches!(
                self,
                Value::Array(..) | Value::LazyList(_) | Value::Hash(_) | Value::Seq(_)
            ),
            "ObjAt" => {
                // ValueObjAt is a subclass of ObjAt
                matches!(
                    self,
                    Value::Instance { class_name, .. }
                        if class_name == "ObjAt" || class_name == "ValueObjAt"
                )
            }
            "Pod::Block" => matches!(
                self,
                Value::Instance { class_name, .. }
                    if class_name == "Pod::Block"
                        || class_name == "Pod::Block::Comment"
                        || class_name == "Pod::Block::Para"
                        || class_name == "Pod::Block::Named"
                        || class_name == "Pod::Heading"
                        || class_name == "Pod::Block::Table"
                        || class_name == "Pod::Item"
            ),
            _ => false,
        }
    }

    /// Check if this value does (composes) the given role name.
    pub(crate) fn does_check(&self, role_name: &str) -> bool {
        if let Value::Mixin(inner, mixins) = self {
            let key = format!("__mutsu_role__{}", role_name);
            if mixins.contains_key(&key) {
                return true;
            }
            return inner.does_check(role_name);
        }
        // Check built-in role compositions
        if role_name == "Encoding" {
            if let Value::Instance { class_name, .. } = self
                && class_name == "Encoding::Builtin"
            {
                return true;
            }
            if let Value::Package(name) = self
                && name == "Encoding::Builtin"
            {
                return true;
            }
        }
        // Delegate to isa_check for other cases (roles are stored as parents)
        self.isa_check(role_name)
    }
}

/// Returns the Raku type name for a value (used in error messages).
pub(crate) fn what_type_name(val: &Value) -> String {
    match val {
        Value::Int(_) | Value::BigInt(_) => "Int".to_string(),
        Value::Num(_) => "Num".to_string(),
        Value::Str(_) => "Str".to_string(),
        Value::Bool(_) => "Bool".to_string(),
        Value::Rat(_, _) | Value::BigRat(_, _) => "Rat".to_string(),
        Value::FatRat(_, _) => "FatRat".to_string(),
        Value::Complex(_, _) => "Complex".to_string(),
        Value::Array(..) | Value::LazyList(_) => "Array".to_string(),
        Value::Seq(_) => "Seq".to_string(),
        Value::Hash(_) => "Hash".to_string(),
        Value::Set(_, is_mutable) => {
            if *is_mutable {
                "SetHash".to_string()
            } else {
                "Set".to_string()
            }
        }
        Value::Bag(_, is_mutable) => {
            if *is_mutable {
                "BagHash".to_string()
            } else {
                "Bag".to_string()
            }
        }
        Value::Mix(_, is_mutable) => {
            if *is_mutable {
                "MixHash".to_string()
            } else {
                "Mix".to_string()
            }
        }
        Value::Pair(_, _) | Value::ValuePair(_, _) => "Pair".to_string(),
        Value::Range(_, _)
        | Value::RangeExcl(_, _)
        | Value::RangeExclStart(_, _)
        | Value::RangeExclBoth(_, _)
        | Value::GenericRange { .. } => "Range".to_string(),
        Value::Nil => "Nil".to_string(),
        Value::Instance { class_name, .. } => class_name.resolve(),
        Value::Package(name) => name.resolve(),
        Value::Enum { enum_type, .. } => enum_type.resolve(),
        Value::Sub(_) | Value::WeakSub(_) => "Sub".to_string(),
        Value::Routine { .. } => "Sub".to_string(),
        Value::Regex(_) => "Regex".to_string(),
        Value::Junction { .. } => "Junction".to_string(),
        Value::Slip(_) => "Slip".to_string(),
        Value::Uni { form, .. } if !form.is_empty() => form.clone(),
        Value::Uni { .. } => "Uni".to_string(),
        Value::Mixin(inner, mixins) => {
            allomorph_type_name(inner, mixins).unwrap_or_else(|| what_type_name(inner))
        }
        _ => "Any".to_string(),
    }
}

/// Return the allomorphic type name for a Mixin value, if it is allomorphic.
/// An allomorphic Mixin has a "Str" key and a numeric inner value.
pub(crate) fn allomorph_type_name(
    inner: &Value,
    mixins: &std::collections::HashMap<String, Value>,
) -> Option<String> {
    if !mixins.contains_key("Str") {
        return None;
    }
    match inner {
        Value::Int(_) | Value::BigInt(_) => Some("IntStr".to_string()),
        Value::Num(_) => Some("NumStr".to_string()),
        Value::Rat(_, _) | Value::FatRat(_, _) | Value::BigRat(_, _) => Some("RatStr".to_string()),
        Value::Complex(_, _) => Some("ComplexStr".to_string()),
        _ => None,
    }
}
