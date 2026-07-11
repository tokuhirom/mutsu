use super::types::allomorph_type_name;
use super::*;

impl Value {
    /// Check if this value is an instance of the given type name (Raku `isa` operator).
    pub(crate) fn isa_check(&self, type_name: &str) -> bool {
        // For Instance/Package, extract name as owned String for later comparison
        let owned_name: Option<String> = match self {
            Value(ValueRepr::Instance { class_name, .. }) => Some(class_name.resolve()),
            Value(ValueRepr::Package(name)) => Some(name.resolve()),
            _ => None,
        };
        let my_type = match self {
            Value(ValueRepr::Int(_)) | Value(ValueRepr::BigInt(_)) => "Int",
            Value(ValueRepr::Num(_)) => "Num",
            Value(ValueRepr::Str(_)) => "Str",
            Value(ValueRepr::Bool(_)) => "Bool",
            Value(ValueRepr::Rat(_, _)) => "Rat",
            Value(ValueRepr::FatRat(_, _)) => "FatRat",
            Value(ValueRepr::BigRat(_, _)) => "Rat",
            Value(ValueRepr::Complex(_, _)) => "Complex",
            Value(ValueRepr::Array(..)) | Value(ValueRepr::LazyList(_)) => "Array",
            Value(ValueRepr::Seq(_)) => "Seq",
            Value(ValueRepr::HyperSeq(_)) => "HyperSeq",
            Value(ValueRepr::RaceSeq(_)) => "RaceSeq",
            Value(ValueRepr::Hash(..)) => "Hash",
            Value(ValueRepr::Set(_, is_mutable)) => {
                if *is_mutable {
                    "SetHash"
                } else {
                    "Set"
                }
            }
            Value(ValueRepr::Bag(_, is_mutable)) => {
                if *is_mutable {
                    "BagHash"
                } else {
                    "Bag"
                }
            }
            Value(ValueRepr::Mix(_, is_mutable)) => {
                if *is_mutable {
                    "MixHash"
                } else {
                    "Mix"
                }
            }
            Value(ValueRepr::Pair(_, _)) | Value(ValueRepr::ValuePair(_, _)) => "Pair",
            Value(ValueRepr::Range(_, _))
            | Value(ValueRepr::RangeExcl(_, _))
            | Value(ValueRepr::RangeExclStart(_, _))
            | Value(ValueRepr::RangeExclBoth(_, _))
            | Value(ValueRepr::GenericRange { .. }) => "Range",
            Value(ValueRepr::Nil) => "Nil",
            Value(ValueRepr::Instance { .. }) | Value(ValueRepr::Package(_)) => {
                owned_name.as_deref().unwrap()
            }
            Value(ValueRepr::Enum { enum_type, .. }) => {
                return enum_type.resolve() == type_name;
            }
            Value(ValueRepr::Sub(data)) => match data.env.get("__mutsu_callable_type") {
                Some(Value(ValueRepr::Str(kind))) if kind.as_str() == "Method" => "Method",
                Some(Value(ValueRepr::Str(kind))) if kind.as_str() == "Submethod" => "Submethod",
                Some(Value(ValueRepr::Str(kind))) if kind.as_str() == "WhateverCode" => {
                    "WhateverCode"
                }
                _ => "Sub",
            },
            Value(ValueRepr::WeakSub(_)) => "Sub",
            Value(ValueRepr::Routine {
                is_regex: false, ..
            }) => "Sub",
            Value(ValueRepr::Regex(_))
            | Value(ValueRepr::RegexWithAdverbs { .. })
            | Value(ValueRepr::Routine { is_regex: true, .. }) => "Regex",
            Value(ValueRepr::Junction { .. }) => "Junction",
            Value(ValueRepr::Version { .. }) => "Version",
            Value(ValueRepr::Slip(_)) => "Slip",
            Value(ValueRepr::Promise(p)) => {
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
            Value(ValueRepr::Channel(_)) => "Channel",
            Value(ValueRepr::CompUnitDepSpec { .. }) => "CompUnit::DependencySpecification",
            Value(ValueRepr::Whatever) => "Whatever",
            Value(ValueRepr::HyperWhatever) => "HyperWhatever",
            Value(ValueRepr::Capture { .. }) => "Capture",
            Value(ValueRepr::Uni(u)) => u.form.as_str(),
            Value(ValueRepr::Mixin(inner, mixins)) => {
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
            Value(ValueRepr::Proxy { .. }) => "Proxy",
            Value(ValueRepr::ParametricRole { base_name, .. }) => {
                return base_name.resolve() == type_name;
            }
            Value(ValueRepr::CustomType(c)) => {
                return c.name.resolve() == type_name;
            }
            Value(ValueRepr::CustomTypeInstance(d)) => {
                return d.type_name.resolve() == type_name;
            }
            Value(ValueRepr::Scalar(inner)) => return inner.isa_check(type_name),
            Value(ValueRepr::ContainerRef(_)) => {
                return self.with_deref(|inner| inner.isa_check(type_name));
            }
            Value(ValueRepr::LazyThunk(thunk_data)) => {
                let cache = thunk_data.cache.lock().unwrap();
                if let Some(ref cached) = *cache {
                    return cached.isa_check(type_name);
                }
                "Scalar" // unforced lazy thunk
            }
            Value(ValueRepr::LazyIoLines { .. }) => "Seq",
            Value(ValueRepr::HashEntryRef { .. }) => {
                return self.hash_entry_read().isa_check(type_name);
            }
        };
        if my_type == type_name {
            return true;
        }
        // The X::Await::Died role is mixed into the original exception when
        // `await` observes a broken Promise (see `await_died_error`): the cause
        // keeps its own class but also does X::Await::Died.
        if type_name == "X::Await::Died"
            && let Value(ValueRepr::Instance { attributes, .. }) = self
            && matches!(
                attributes.as_map().get("__mutsu_does_await_died"),
                Some(Value(ValueRepr::Bool(true)))
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
            "SetHash" => matches!(self, Value(ValueRepr::Set(_, true))),
            "BagHash" => matches!(self, Value(ValueRepr::Bag(_, true))),
            "MixHash" => matches!(self, Value(ValueRepr::Mix(_, true))),
            "Cool" => {
                matches!(
                    self,
                    Value(ValueRepr::Int(_))
                        | Value(ValueRepr::BigInt(_))
                        | Value(ValueRepr::Num(_))
                        | Value(ValueRepr::Str(_))
                        | Value(ValueRepr::Bool(_))
                        | Value(ValueRepr::Rat(_, _))
                        | Value(ValueRepr::FatRat(_, _))
                        | Value(ValueRepr::BigRat(_, _))
                        | Value(ValueRepr::Complex(_, _))
                        | Value(ValueRepr::Array(..))
                        | Value(ValueRepr::Hash(..))
                ) || matches!(
                    self,
                    Value(ValueRepr::Instance { class_name, .. })
                        if class_name == "Match" || class_name == "Capture"
                )
            }
            "Capture" => {
                matches!(self, Value(ValueRepr::Capture { .. }))
                    || matches!(
                        self,
                        Value(ValueRepr::Instance { class_name, .. })
                            if class_name == "Match" || class_name == "Capture"
                    )
            }
            "Numeric" => matches!(
                self,
                Value(ValueRepr::Int(_))
                    | Value(ValueRepr::BigInt(_))
                    | Value(ValueRepr::Num(_))
                    | Value(ValueRepr::Rat(_, _))
                    | Value(ValueRepr::FatRat(_, _))
                    | Value(ValueRepr::BigRat(_, _))
                    | Value(ValueRepr::Complex(_, _))
            ),
            "Real" => matches!(
                self,
                Value(ValueRepr::Int(_))
                    | Value(ValueRepr::BigInt(_))
                    | Value(ValueRepr::Num(_))
                    | Value(ValueRepr::Rat(_, _))
                    | Value(ValueRepr::FatRat(_, _))
                    | Value(ValueRepr::BigRat(_, _))
            ),
            "Rational" => matches!(
                self,
                Value(ValueRepr::Rat(_, _))
                    | Value(ValueRepr::FatRat(_, _))
                    | Value(ValueRepr::BigRat(_, _))
            ),
            "Dateish" => matches!(
                self,
                Value(ValueRepr::Instance { class_name, .. }) if class_name == "Date" || class_name == "DateTime"
            ),
            "FatRat" => matches!(
                self,
                Value(ValueRepr::FatRat(_, _)) | Value(ValueRepr::BigRat(_, _))
            ),
            "Int" => matches!(self, Value(ValueRepr::Bool(_))),
            "Stringy" => matches!(self, Value(ValueRepr::Str(_))),
            "Block" | "Routine" | "Code" | "Callable" => {
                matches!(
                    self,
                    Value(ValueRepr::Sub(_))
                        | Value(ValueRepr::WeakSub(_))
                        | Value(ValueRepr::Routine { .. })
                ) || matches!(
                    self,
                    Value(ValueRepr::Package(name))
                        if matches!(name.resolve().as_str(), "Sub" | "Routine" | "Method" | "Block" | "Code")
                )
            }
            "Method" => {
                matches!(
                    self,
                    Value(ValueRepr::Sub(data))
                        if matches!(
                            data.env.get("__mutsu_callable_type"),
                            Some(Value(ValueRepr::Str(kind))) if kind.as_str() == "Method"
                        )
                ) || matches!(
                    self,
                    Value(ValueRepr::Instance { class_name, .. }) if class_name == "Method"
                ) || matches!(self, Value(ValueRepr::Package(name)) if name == "Method")
            }
            "Exception" => {
                if let Value(ValueRepr::Instance { class_name, .. }) = self {
                    class_name.resolve().starts_with("X::") || class_name == "Exception"
                } else {
                    false
                }
            }
            "X::AdHoc" | "CX::Warn" | "CX::Return" | "X::OS" => {
                if let Value(ValueRepr::Instance { class_name, .. }) = self {
                    class_name == type_name
                } else {
                    false
                }
            }
            "HyperSeq" => {
                matches!(self, Value(ValueRepr::HyperSeq(_)))
            }
            "RaceSeq" => {
                matches!(self, Value(ValueRepr::RaceSeq(_)))
            }
            "Seq" | "List" => {
                matches!(
                    self,
                    Value(ValueRepr::Array(..))
                        | Value(ValueRepr::LazyList(_))
                        | Value(ValueRepr::Slip(_))
                        | Value(ValueRepr::HyperSeq(_))
                        | Value(ValueRepr::RaceSeq(_))
                )
            }
            "Positional" => {
                matches!(
                    self,
                    Value(ValueRepr::Array(..))
                        | Value(ValueRepr::LazyList(_))
                        | Value(ValueRepr::HyperSeq(_))
                        | Value(ValueRepr::RaceSeq(_))
                        | Value(ValueRepr::Range(_, _))
                        | Value(ValueRepr::RangeExcl(_, _))
                        | Value(ValueRepr::RangeExclStart(_, _))
                        | Value(ValueRepr::RangeExclBoth(_, _))
                        | Value(ValueRepr::GenericRange { .. })
                        | Value(ValueRepr::Capture { .. })
                ) || matches!(
                    self,
                    Value(ValueRepr::Package(name))
                        if matches!(
                            name.resolve().as_str(),
                            "Array" | "List" | "Range" | "Buf" | "Blob" | "Capture"
                        )
                ) || matches!(
                    self,
                    Value(ValueRepr::Instance { attributes, .. })
                        if attributes.contains_key("__mutsu_array_storage")
                )
            }
            "Map" | "Associative" => {
                matches!(
                    self,
                    Value(ValueRepr::Hash(..))
                        | Value(ValueRepr::Pair(_, _))
                        | Value(ValueRepr::ValuePair(_, _))
                        | Value(ValueRepr::Set(_, _))
                        | Value(ValueRepr::Bag(_, _))
                        | Value(ValueRepr::Mix(_, _))
                        | Value(ValueRepr::Capture { .. })
                ) || matches!(
                    self,
                    Value(ValueRepr::Package(name))
                        if matches!(
                            name.resolve().as_str(),
                            "Hash" | "Map" | "Pair" | "Set" | "Bag" | "Mix" | "QuantHash" | "Capture"
                        )
                )
            }
            "Iterable" => matches!(
                self,
                Value(ValueRepr::Array(..))
                    | Value(ValueRepr::LazyList(_))
                    | Value(ValueRepr::Hash(..))
                    | Value(ValueRepr::Seq(_))
            ),
            "ObjAt" => {
                // ValueObjAt is a subclass of ObjAt
                matches!(
                    self,
                    Value(ValueRepr::Instance { class_name, .. })
                        if class_name == "ObjAt" || class_name == "ValueObjAt"
                )
            }
            "Pod::Block" => matches!(
                self,
                Value(ValueRepr::Instance { class_name, .. })
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
                Value(ValueRepr::Instance { class_name, .. })
                    if class_name == "Pod::Config"
            ),
            _ => false,
        }
    }

    /// Check if this value does (composes) the given role name.
    pub(crate) fn does_check(&self, role_name: &str) -> bool {
        if let Value(ValueRepr::Mixin(inner, mixins)) = self {
            let key = format!("__mutsu_role__{}", role_name);
            if mixins.contains_key(&key) {
                return true;
            }
            return inner.does_check(role_name);
        }
        // Check built-in role compositions
        if role_name == "Encoding" {
            if let Value(ValueRepr::Instance { class_name, .. }) = self
                && class_name == "Encoding::Builtin"
            {
                return true;
            }
            if let Value(ValueRepr::Package(name)) = self
                && name == "Encoding::Builtin"
            {
                return true;
            }
        }
        // Delegate to isa_check for other cases (roles are stored as parents)
        self.isa_check(role_name)
    }
}
