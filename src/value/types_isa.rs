use super::types::allomorph_type_name;
use super::*;

impl Value {
    /// Check if this value is an instance of the given type name (Raku `isa` operator).
    pub(crate) fn isa_check(&self, type_name: &str) -> bool {
        // For Instance/Package, extract name as owned String for later comparison
        let owned_name: Option<String> = match self.view() {
            ValueView::Instance { class_name, .. } => Some(class_name.resolve()),
            ValueView::Package(name) => Some(name.resolve()),
            _ => None,
        };
        let my_type = match self.view() {
            ValueView::Int(_) | ValueView::BigInt(_) => "Int",
            ValueView::Num(_) => "Num",
            ValueView::Str(_) => "Str",
            ValueView::Bool(_) => "Bool",
            ValueView::Rat(_, _) => "Rat",
            ValueView::FatRat(_, _) => "FatRat",
            ValueView::BigRat(_, _) => "Rat",
            ValueView::Complex(_, _) => "Complex",
            ValueView::Array(..) | ValueView::LazyList(_) => "Array",
            ValueView::Seq(_) => "Seq",
            ValueView::HyperSeq(_) => "HyperSeq",
            ValueView::RaceSeq(_) => "RaceSeq",
            ValueView::Hash(..) => "Hash",
            ValueView::Set(_, is_mutable) => {
                if is_mutable {
                    "SetHash"
                } else {
                    "Set"
                }
            }
            ValueView::Bag(_, is_mutable) => {
                if is_mutable {
                    "BagHash"
                } else {
                    "Bag"
                }
            }
            ValueView::Mix(_, is_mutable) => {
                if is_mutable {
                    "MixHash"
                } else {
                    "Mix"
                }
            }
            ValueView::Pair(_, _) | ValueView::ValuePair(_, _) => "Pair",
            ValueView::Range(_, _)
            | ValueView::RangeExcl(_, _)
            | ValueView::RangeExclStart(_, _)
            | ValueView::RangeExclBoth(_, _)
            | ValueView::GenericRange { .. } => "Range",
            ValueView::Nil => "Nil",
            ValueView::Instance { .. } | ValueView::Package(_) => owned_name.as_deref().unwrap(),
            ValueView::Enum { enum_type, .. } => {
                return enum_type.resolve() == type_name;
            }
            ValueView::Sub(data) => match data.env.get("__mutsu_callable_type").map(Value::view) {
                Some(ValueView::Str(kind)) if kind.as_str() == "Method" => "Method",
                Some(ValueView::Str(kind)) if kind.as_str() == "Submethod" => "Submethod",
                Some(ValueView::Str(kind)) if kind.as_str() == "WhateverCode" => "WhateverCode",
                _ => "Sub",
            },
            ValueView::WeakSub(_) => "Sub",
            ValueView::Routine {
                is_regex: false, ..
            } => "Sub",
            ValueView::Regex(_)
            | ValueView::RegexWithAdverbs { .. }
            | ValueView::Routine { is_regex: true, .. } => "Regex",
            ValueView::Junction { .. } => "Junction",
            ValueView::Version { .. } => "Version",
            ValueView::Slip(_) => "Slip",
            ValueView::Promise(p) => {
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
            ValueView::Channel(_) => "Channel",
            ValueView::CompUnitDepSpec { .. } => "CompUnit::DependencySpecification",
            ValueView::Whatever => "Whatever",
            ValueView::HyperWhatever => "HyperWhatever",
            ValueView::Capture { .. } => "Capture",
            ValueView::Uni(u) => u.form.as_str(),
            ValueView::Mixin(inner, mixins) => {
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
            ValueView::Proxy { .. } => "Proxy",
            ValueView::ParametricRole { base_name, .. } => {
                return base_name.resolve() == type_name;
            }
            ValueView::CustomType(c) => {
                return c.name.resolve() == type_name;
            }
            ValueView::CustomTypeInstance(d) => {
                return d.type_name.resolve() == type_name;
            }
            ValueView::Scalar(inner) => return inner.isa_check(type_name),
            ValueView::ContainerRef(_) => {
                return self.with_deref(|inner| inner.isa_check(type_name));
            }
            ValueView::LazyThunk(thunk_data) => {
                let cache = thunk_data.cache.lock().unwrap();
                if let Some(ref cached) = *cache {
                    return cached.isa_check(type_name);
                }
                "Scalar" // unforced lazy thunk
            }
            ValueView::LazyIoLines { .. } => "Seq",
            ValueView::HashEntryRef { .. } => {
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
            && let ValueView::Instance { attributes, .. } = self.view()
            && matches!(
                attributes
                    .as_map()
                    .get("__mutsu_does_await_died")
                    .map(Value::view),
                Some(ValueView::Bool(true))
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
            "SetHash" => matches!(self.view(), ValueView::Set(_, true)),
            "BagHash" => matches!(self.view(), ValueView::Bag(_, true)),
            "MixHash" => matches!(self.view(), ValueView::Mix(_, true)),
            "Cool" => {
                matches!(
                    self.view(),
                    ValueView::Int(_)
                        | ValueView::BigInt(_)
                        | ValueView::Num(_)
                        | ValueView::Str(_)
                        | ValueView::Bool(_)
                        | ValueView::Rat(_, _)
                        | ValueView::FatRat(_, _)
                        | ValueView::BigRat(_, _)
                        | ValueView::Complex(_, _)
                        | ValueView::Array(..)
                        | ValueView::Hash(..)
                ) || matches!(
                    self.view(),
                    ValueView::Instance { class_name, .. }
                        if class_name == "Match" || class_name == "Capture"
                )
            }
            "Capture" => {
                matches!(self.view(), ValueView::Capture { .. })
                    || matches!(
                        self.view(),
                        ValueView::Instance { class_name, .. }
                            if class_name == "Match" || class_name == "Capture"
                    )
            }
            "Numeric" => matches!(
                self.view(),
                ValueView::Int(_)
                    | ValueView::BigInt(_)
                    | ValueView::Num(_)
                    | ValueView::Rat(_, _)
                    | ValueView::FatRat(_, _)
                    | ValueView::BigRat(_, _)
                    | ValueView::Complex(_, _)
            ),
            "Real" => matches!(
                self.view(),
                ValueView::Int(_)
                    | ValueView::BigInt(_)
                    | ValueView::Num(_)
                    | ValueView::Rat(_, _)
                    | ValueView::FatRat(_, _)
                    | ValueView::BigRat(_, _)
            ),
            "Rational" => matches!(
                self.view(),
                ValueView::Rat(_, _) | ValueView::FatRat(_, _) | ValueView::BigRat(_, _)
            ),
            "Dateish" => matches!(
                self.view(),
                ValueView::Instance { class_name, .. } if class_name == "Date" || class_name == "DateTime"
            ),
            "FatRat" => matches!(
                self.view(),
                ValueView::FatRat(_, _) | ValueView::BigRat(_, _)
            ),
            "Int" => matches!(self.view(), ValueView::Bool(_)),
            "Stringy" => matches!(self.view(), ValueView::Str(_)),
            "Block" | "Routine" | "Code" | "Callable" => {
                matches!(
                    self.view(),
                    ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
                ) || matches!(
                    self.view(),
                    ValueView::Package(name)
                        if matches!(name.resolve().as_str(), "Sub" | "Routine" | "Method" | "Block" | "Code")
                )
            }
            "Method" => {
                matches!(
                    self.view(),
                    ValueView::Sub(data)
                        if matches!(
                            data.env.get("__mutsu_callable_type").map(Value::view),
                            Some(ValueView::Str(kind)) if kind.as_str() == "Method"
                        )
                ) || matches!(
                    self.view(),
                    ValueView::Instance { class_name, .. } if class_name == "Method"
                ) || matches!(self.view(), ValueView::Package(name) if name == "Method")
            }
            "Exception" => {
                if let ValueView::Instance { class_name, .. } = self.view() {
                    class_name.resolve().starts_with("X::") || class_name == "Exception"
                } else {
                    false
                }
            }
            "X::AdHoc" | "CX::Warn" | "CX::Return" | "X::OS" => {
                if let ValueView::Instance { class_name, .. } = self.view() {
                    class_name == type_name
                } else {
                    false
                }
            }
            "HyperSeq" => {
                matches!(self.view(), ValueView::HyperSeq(_))
            }
            "RaceSeq" => {
                matches!(self.view(), ValueView::RaceSeq(_))
            }
            "Seq" | "List" => {
                matches!(
                    self.view(),
                    ValueView::Array(..)
                        | ValueView::LazyList(_)
                        | ValueView::Slip(_)
                        | ValueView::HyperSeq(_)
                        | ValueView::RaceSeq(_)
                )
            }
            "Positional" => {
                matches!(
                    self.view(),
                    ValueView::Array(..)
                        | ValueView::LazyList(_)
                        | ValueView::HyperSeq(_)
                        | ValueView::RaceSeq(_)
                        | ValueView::Range(_, _)
                        | ValueView::RangeExcl(_, _)
                        | ValueView::RangeExclStart(_, _)
                        | ValueView::RangeExclBoth(_, _)
                        | ValueView::GenericRange { .. }
                        | ValueView::Capture { .. }
                ) || matches!(
                    self.view(),
                    ValueView::Package(name)
                        if matches!(
                            name.resolve().as_str(),
                            "Array" | "List" | "Range" | "Buf" | "Blob" | "Capture"
                        )
                ) || matches!(
                    self.view(),
                    ValueView::Instance { attributes, .. }
                        if attributes.contains_key("__mutsu_array_storage")
                )
            }
            "Map" | "Associative" => {
                matches!(
                    self.view(),
                    ValueView::Hash(..)
                        | ValueView::Pair(_, _)
                        | ValueView::ValuePair(_, _)
                        | ValueView::Set(_, _)
                        | ValueView::Bag(_, _)
                        | ValueView::Mix(_, _)
                        | ValueView::Capture { .. }
                ) || matches!(
                    self.view(),
                    ValueView::Package(name)
                        if matches!(
                            name.resolve().as_str(),
                            "Hash" | "Map" | "Pair" | "Set" | "Bag" | "Mix" | "QuantHash" | "Capture"
                        )
                )
            }
            "Iterable" => matches!(
                self.view(),
                ValueView::Array(..)
                    | ValueView::LazyList(_)
                    | ValueView::Hash(..)
                    | ValueView::Seq(_)
            ),
            "ObjAt" => {
                // ValueObjAt is a subclass of ObjAt
                matches!(
                    self.view(),
                    ValueView::Instance { class_name, .. }
                        if class_name == "ObjAt" || class_name == "ValueObjAt"
                )
            }
            "Pod::Block" => matches!(
                self.view(),
                ValueView::Instance { class_name, .. }
                    if class_name == "Pod::Block"
                        || class_name == "Pod::Block::Comment"
                        || class_name == "Pod::Block::Para"
                        || class_name == "Pod::Block::Named"
                        || class_name == "Pod::Heading"
                        || class_name == "Pod::Block::Table"
                        || class_name == "Pod::Item"
            ),
            "Pod::Config" => matches!(
                self.view(),
                ValueView::Instance { class_name, .. }
                    if class_name == "Pod::Config"
            ),
            _ => false,
        }
    }

    /// Check if this value does (composes) the given role name.
    pub(crate) fn does_check(&self, role_name: &str) -> bool {
        if let ValueView::Mixin(inner, mixins) = self.view() {
            let key = format!("__mutsu_role__{}", role_name);
            if mixins.contains_key(&key) {
                return true;
            }
            return inner.does_check(role_name);
        }
        // Check built-in role compositions
        if role_name == "Encoding" {
            if let ValueView::Instance { class_name, .. } = self.view()
                && class_name == "Encoding::Builtin"
            {
                return true;
            }
            if let ValueView::Package(name) = self.view()
                && name == "Encoding::Builtin"
            {
                return true;
            }
        }
        // Delegate to isa_check for other cases (roles are stored as parents)
        self.isa_check(role_name)
    }
}
