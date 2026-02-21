use super::*;

impl Value {
    /// Type-strict value equivalence (Raku `eqv` operator).
    /// Unlike PartialEq (used for `==`), this does NOT allow cross-type comparisons.
    /// Two values are eqv only if they are the same type AND have the same value.
    pub(crate) fn eqv(&self, other: &Self) -> bool {
        match (self, other) {
            // Arrays: recursively use eqv for elements
            (Value::Array(a), Value::Array(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.eqv(y))
            }
            // Hashes: recursively use eqv for values
            (Value::Hash(a), Value::Hash(b)) => {
                a.len() == b.len() && a.iter().all(|(k, v)| b.get(k).is_some_and(|bv| v.eqv(bv)))
            }
            // Pairs: recursively use eqv for values
            (Value::Pair(ak, av), Value::Pair(bk, bv)) => ak == bk && av.eqv(bv),
            // Slips: recursively use eqv for elements
            (Value::Slip(a), Value::Slip(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.eqv(y))
            }
            // Same-type scalar comparisons delegate to PartialEq
            (Value::Int(_), Value::Int(_))
            | (Value::Num(_), Value::Num(_))
            | (Value::Str(_), Value::Str(_))
            | (Value::Bool(_), Value::Bool(_))
            | (Value::Rat(_, _), Value::Rat(_, _))
            | (Value::FatRat(_, _), Value::FatRat(_, _))
            | (Value::Complex(_, _), Value::Complex(_, _))
            | (Value::Set(_), Value::Set(_))
            | (Value::Bag(_), Value::Bag(_))
            | (Value::Mix(_), Value::Mix(_))
            | (Value::Enum { .. }, Value::Enum { .. })
            | (Value::Regex(_), Value::Regex(_))
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
            | (Value::Channel(_), Value::Channel(_)) => self == other,
            // Cross-type comparisons always return false for eqv
            _ => false,
        }
    }

    pub(crate) fn truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Int(i) => *i != 0,
            Value::BigInt(n) => !n.is_zero(),
            Value::Num(f) => *f != 0.0 && !f.is_nan(),
            Value::Str(s) => !s.is_empty(),
            Value::Range(_, _) => true,
            Value::RangeExcl(_, _) => true,
            Value::RangeExclStart(_, _) => true,
            Value::RangeExclBoth(_, _) => true,
            Value::GenericRange { .. } => true,
            Value::Array(items) => !items.is_empty(),
            Value::Hash(items) => !items.is_empty(),
            Value::Rat(n, _) => *n != 0,
            Value::FatRat(n, _) => !n.is_zero(),
            Value::Complex(r, i) => *r != 0.0 || *i != 0.0,
            Value::Set(s) => !s.is_empty(),
            Value::Bag(b) => !b.is_empty(),
            Value::Mix(m) => !m.is_empty(),
            Value::Pair(_, _) => true,
            Value::Enum { .. } => true,
            Value::CompUnitDepSpec { .. } => true,
            Value::Package(_) => false,
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
                true
            }
            Value::Junction { kind, values } => match kind {
                JunctionKind::Any => values.iter().any(|v| v.truthy()),
                JunctionKind::All => values.iter().all(|v| v.truthy()),
                JunctionKind::One => values.iter().filter(|v| v.truthy()).count() == 1,
                JunctionKind::None => values.iter().all(|v| !v.truthy()),
            },
            Value::Slip(items) => !items.is_empty(),
            Value::LazyList(_) => true,
            Value::Promise(p) => p.is_resolved(),
            Value::Channel(_) => true,
            Value::Regex(_) => true,
            Value::Version { .. } => true,
            Value::Nil => false,
            Value::HyperWhatever => true,
            Value::Mixin(inner, mixins) => {
                if let Some(bool_val) = mixins.get("Bool") {
                    bool_val.truthy()
                } else {
                    inner.truthy()
                }
            }
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
            Value::Complex(_, _) => "Complex",
            Value::Array(_) | Value::LazyList(_) => "Array",
            Value::Hash(_) => "Hash",
            Value::Set(_) => "Set",
            Value::Bag(_) => "Bag",
            Value::Mix(_) => "Mix",
            Value::Pair(_, _) => "Pair",
            Value::Range(_, _)
            | Value::RangeExcl(_, _)
            | Value::RangeExclStart(_, _)
            | Value::RangeExclBoth(_, _)
            | Value::GenericRange { .. } => "Range",
            Value::Nil => "Nil",
            Value::Instance { class_name, .. } => class_name.as_str(),
            Value::Package(name) => name.as_str(),
            Value::Enum { enum_type, .. } => enum_type.as_str(),
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. } => "Sub",
            Value::Regex(_) => "Regex",
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
            Value::HyperWhatever => "HyperWhatever",
            Value::Mixin(inner, _) => return inner.isa_check(type_name),
        };
        if my_type == type_name {
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
                    | Value::Complex(_, _)
                    | Value::Array(_)
                    | Value::Hash(_)
            ),
            "Numeric" => matches!(
                self,
                Value::Int(_)
                    | Value::BigInt(_)
                    | Value::Num(_)
                    | Value::Rat(_, _)
                    | Value::FatRat(_, _)
                    | Value::Complex(_, _)
            ),
            "Real" => matches!(
                self,
                Value::Int(_)
                    | Value::BigInt(_)
                    | Value::Num(_)
                    | Value::Rat(_, _)
                    | Value::FatRat(_, _)
            ),
            "Int" => matches!(self, Value::Bool(_)),
            "Stringy" => matches!(self, Value::Str(_)),
            "Block" | "Routine" | "Code" | "Callable" => {
                matches!(
                    self,
                    Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
                )
            }
            "Exception" => {
                if let Value::Instance { class_name, .. } = self {
                    class_name.starts_with("X::") || class_name == "Exception"
                } else {
                    false
                }
            }
            "X::AdHoc" | "CX::Warn" => {
                if let Value::Instance { class_name, .. } = self {
                    class_name == type_name
                } else {
                    false
                }
            }
            "Seq" | "List" => matches!(self, Value::Array(_) | Value::LazyList(_) | Value::Slip(_)),
            "Positional" => matches!(self, Value::Array(_) | Value::LazyList(_)),
            "Map" | "Associative" => matches!(self, Value::Hash(_)),
            "Iterable" => matches!(self, Value::Array(_) | Value::LazyList(_) | Value::Hash(_)),
            _ => false,
        }
    }

    /// Check if this value does (composes) the given role name.
    pub(crate) fn does_check(&self, role_name: &str) -> bool {
        // For now, delegate to isa_check since we don't track roles separately
        self.isa_check(role_name)
    }
}
