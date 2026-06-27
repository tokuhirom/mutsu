use super::types::allomorph_type_name;
use super::*;

impl Value {
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
            Value::HyperSeq(_) => "HyperSeq",
            Value::RaceSeq(_) => "RaceSeq",
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
                Some(Value::Str(kind)) if kind.as_str() == "Submethod" => "Submethod",
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
            Value::Uni(u) => u.form.as_str(),
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
            Value::CustomType(c) => {
                return c.name.resolve() == type_name;
            }
            Value::CustomTypeInstance(d) => {
                return d.type_name.resolve() == type_name;
            }
            Value::Scalar(inner) => return inner.isa_check(type_name),
            Value::ContainerRef(_) => return self.with_deref(|inner| inner.isa_check(type_name)),
            Value::LazyThunk(thunk_data) => {
                let cache = thunk_data.cache.lock().unwrap();
                if let Some(ref cached) = *cache {
                    return cached.isa_check(type_name);
                }
                "Scalar" // unforced lazy thunk
            }
            Value::LazyIoLines { .. } => "Seq",
            Value::HashEntryRef { .. } => return self.hash_entry_read().isa_check(type_name),
        };
        if my_type == type_name {
            return true;
        }
        // The X::Await::Died role is mixed into the original exception when
        // `await` observes a broken Promise (see `await_died_error`): the cause
        // keeps its own class but also does X::Await::Died.
        if type_name == "X::Await::Died"
            && let Value::Instance { attributes, .. } = self
            && matches!(
                attributes.as_map().get("__mutsu_does_await_died"),
                Some(Value::Bool(true))
            )
        {
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
            "Cool" => {
                matches!(
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
                ) || matches!(
                    self,
                    Value::Instance { class_name, .. }
                        if class_name == "Match" || class_name == "Capture"
                )
            }
            "Capture" => {
                matches!(self, Value::Capture { .. })
                    || matches!(
                        self,
                        Value::Instance { class_name, .. }
                            if class_name == "Match" || class_name == "Capture"
                    )
            }
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
            "Rational" => matches!(
                self,
                Value::Rat(_, _) | Value::FatRat(_, _) | Value::BigRat(_, _)
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
            "HyperSeq" => {
                matches!(self, Value::HyperSeq(_))
            }
            "RaceSeq" => {
                matches!(self, Value::RaceSeq(_))
            }
            "Seq" | "List" => {
                matches!(
                    self,
                    Value::Array(..)
                        | Value::LazyList(_)
                        | Value::Slip(_)
                        | Value::HyperSeq(_)
                        | Value::RaceSeq(_)
                )
            }
            "Positional" => {
                matches!(
                    self,
                    Value::Array(..)
                        | Value::LazyList(_)
                        | Value::HyperSeq(_)
                        | Value::RaceSeq(_)
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
                ) || matches!(
                    self,
                    Value::Instance { attributes, .. }
                        if attributes.contains_key("__mutsu_array_storage")
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
            "Pod::Config" => matches!(
                self,
                Value::Instance { class_name, .. }
                    if class_name == "Pod::Config"
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
