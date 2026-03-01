use super::*;

impl Value {
    /// Bit-exact identity (Raku `===` operator).
    /// Unlike PartialEq, this distinguishes 0e0 from -0e0 and different NaN payloads.
    pub(crate) fn strict_identical(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Num(a), Value::Num(b)) => a.to_bits() == b.to_bits(),
            (Value::Complex(ar, ai), Value::Complex(br, bi)) => {
                ar.to_bits() == br.to_bits() && ai.to_bits() == bi.to_bits()
            }
            _ => self == other,
        }
    }

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
            (Value::Array(a, a_mut), Value::Array(b, b_mut)) => {
                a_mut == b_mut
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
                matches!(bk.as_ref(), Value::Str(s) if s == ak) && av.eqv(bv)
            }
            (Value::ValuePair(ak, av), Value::Pair(bk, bv)) => {
                matches!(ak.as_ref(), Value::Str(s) if s == bk) && av.eqv(bv)
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
            // Num: use bit-exact comparison to distinguish signed zeros
            (Value::Num(a), Value::Num(b)) => a.to_bits() == b.to_bits(),
            // Complex: use bit-exact comparison for both components
            (Value::Complex(ar, ai), Value::Complex(br, bi)) => {
                ar.to_bits() == br.to_bits() && ai.to_bits() == bi.to_bits()
            }
            // Same-type scalar comparisons delegate to PartialEq
            (Value::Int(_), Value::Int(_))
            | (Value::Str(_), Value::Str(_))
            | (Value::Bool(_), Value::Bool(_))
            | (Value::Rat(_, _), Value::Rat(_, _))
            | (Value::FatRat(_, _), Value::FatRat(_, _))
            | (Value::BigRat(_, _), Value::BigRat(_, _))
            | (Value::Set(_), Value::Set(_))
            | (Value::Bag(_), Value::Bag(_))
            | (Value::Mix(_), Value::Mix(_))
            | (Value::Enum { .. }, Value::Enum { .. })
            | (Value::Regex(_), Value::Regex(_))
            | (Value::RegexWithAdverbs { .. }, Value::RegexWithAdverbs { .. })
            | (Value::Routine { .. }, Value::Routine { .. })
            | (Value::Sub(_), Value::Sub(_))
            | (Value::Instance { .. }, Value::Instance { .. })
            | (Value::Range(_, _), Value::Range(_, _))
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
            // Cross-type comparisons always return false for eqv
            _ => false,
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
            Value::Set(s) => !s.is_empty(),
            Value::Bag(b) => !b.is_empty(),
            Value::Mix(m) => !m.is_empty(),
            Value::Pair(_, _) | Value::ValuePair(_, _) => true,
            Value::Enum { .. } => true,
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
            Value::Capture { positional, .. } => !positional.is_empty(),
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
        }
    }

    /// Check if this value is an instance of the given type name (Raku `isa` operator).
    pub(crate) fn isa_check(&self, type_name: &str) -> bool {
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
            Value::Set(_) => "Set",
            Value::Bag(_) => "Bag",
            Value::Mix(_) => "Mix",
            Value::Pair(_, _) | Value::ValuePair(_, _) => "Pair",
            Value::Range(_, _)
            | Value::RangeExcl(_, _)
            | Value::RangeExclStart(_, _)
            | Value::RangeExclBoth(_, _)
            | Value::GenericRange { .. } => "Range",
            Value::Nil => "Nil",
            Value::Instance { class_name, .. } => class_name.as_str(),
            Value::Package(name) => name.as_str(),
            Value::Enum { enum_type, .. } => enum_type.as_str(),
            Value::Sub(data) => match data.env.get("__mutsu_callable_type") {
                Some(Value::Str(kind)) if kind == "Method" => "Method",
                Some(Value::Str(kind)) if kind == "WhateverCode" => "WhateverCode",
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
                if cn != "Promise" && type_name == cn {
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
                if inner.isa_check(type_name) {
                    return true;
                }
                // Also check mixin type keys (e.g., allomorphic "Str" mixin)
                return mixins.contains_key(type_name);
            }
            Value::Proxy { .. } => "Proxy",
            Value::ParametricRole { base_name, .. } => base_name.as_str(),
            Value::CustomType { name, .. } => name.as_str(),
            Value::CustomTypeInstance { type_name: tn, .. } => tn.as_str(),
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
            "Int" => matches!(self, Value::Bool(_)),
            "Stringy" => matches!(self, Value::Str(_)),
            "Block" | "Routine" | "Code" | "Callable" => {
                matches!(
                    self,
                    Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
                ) || matches!(
                    self,
                    Value::Package(name)
                        if matches!(name.as_str(), "Sub" | "Routine" | "Method" | "Block" | "Code")
                )
            }
            "Method" => {
                matches!(
                    self,
                    Value::Sub(data)
                        if matches!(
                            data.env.get("__mutsu_callable_type"),
                            Some(Value::Str(kind)) if kind == "Method"
                        )
                ) || matches!(
                    self,
                    Value::Instance { class_name, .. } if class_name == "Method"
                ) || matches!(self, Value::Package(name) if name == "Method")
            }
            "Exception" => {
                if let Value::Instance { class_name, .. } = self {
                    class_name.starts_with("X::") || class_name == "Exception"
                } else {
                    false
                }
            }
            "X::AdHoc" | "CX::Warn" | "X::OS" => {
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
                            name.as_str(),
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
                        | Value::Set(_)
                        | Value::Bag(_)
                        | Value::Mix(_)
                        | Value::Capture { .. }
                ) || matches!(
                    self,
                    Value::Package(name)
                        if matches!(
                            name.as_str(),
                            "Hash" | "Map" | "Pair" | "Set" | "Bag" | "Mix" | "QuantHash" | "Capture"
                        )
                )
            }
            "Iterable" => matches!(self, Value::Array(..) | Value::LazyList(_) | Value::Hash(_)),
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
pub(crate) fn what_type_name(val: &Value) -> &str {
    match val {
        Value::Int(_) | Value::BigInt(_) => "Int",
        Value::Num(_) => "Num",
        Value::Str(_) => "Str",
        Value::Bool(_) => "Bool",
        Value::Rat(_, _) | Value::BigRat(_, _) => "Rat",
        Value::FatRat(_, _) => "FatRat",
        Value::Complex(_, _) => "Complex",
        Value::Array(..) | Value::LazyList(_) => "Array",
        Value::Seq(_) => "Seq",
        Value::Hash(_) => "Hash",
        Value::Set(_) => "Set",
        Value::Bag(_) => "Bag",
        Value::Mix(_) => "Mix",
        Value::Pair(_, _) | Value::ValuePair(_, _) => "Pair",
        Value::Range(_, _)
        | Value::RangeExcl(_, _)
        | Value::RangeExclStart(_, _)
        | Value::RangeExclBoth(_, _)
        | Value::GenericRange { .. } => "Range",
        Value::Nil => "Nil",
        Value::Instance { class_name, .. } => class_name.as_str(),
        Value::Package(name) => name.as_str(),
        Value::Enum { enum_type, .. } => enum_type.as_str(),
        Value::Sub(_) | Value::WeakSub(_) => "Sub",
        Value::Routine { .. } => "Sub",
        Value::Regex(_) => "Regex",
        Value::Junction { .. } => "Junction",
        Value::Slip(_) => "Slip",
        Value::Mixin(_, _) => "Mixin",
        _ => "Any",
    }
}
