use super::types::allomorph_type_name;
use super::*;

impl Value {
    /// Check if this value is an instance of the given type name (Raku `isa` operator).
    ///
    /// `.isa()` checks only the **nominal class hierarchy** (`.^mro`): it is
    /// `False` for a role the value merely *does* (composes), even when an
    /// ancestor class composes that role. `.does()` / `~~` / role-aware
    /// smart-match are role-aware and must stay `True` for those roles — see
    /// `does_check` below, which additionally consults `does_role_hierarchy`.
    pub(crate) fn isa_check(&self, type_name: &str) -> bool {
        self.isa_or_does_check(type_name, false)
    }

    /// Shared implementation behind both `isa_check` (nominal-only,
    /// `allow_roles = false`) and `does_check` (nominal-or-role,
    /// `allow_roles = true`). Every wrapper-unwrapping early return threads
    /// `allow_roles` through the recursive call so a wrapped value (Scalar,
    /// ContainerRef, a forced LazyThunk, a HashEntryRef, a VarRef, or a Mixin's
    /// inner value) resolves under the same semantics as the outer call.
    fn isa_or_does_check(&self, type_name: &str, allow_roles: bool) -> bool {
        // For Instance/Package, extract name as owned String for later comparison
        let owned_name: Option<String> = match self.view() {
            ValueView::Instance { class_name, .. } => Some(class_name.resolve()),
            ValueView::Package(name) => Some(name.resolve()),
            _ => None,
        };
        // A `VarRef` is a transient binder wrapper, not a type of its own: it
        // answers as the variable's value.
        if let ValueView::VarRef { value, .. } = self.view() {
            return value.isa_or_does_check(type_name, allow_roles);
        }
        let my_type = match self.view() {
            ValueView::VarRef { .. } => unreachable!("unwrapped above"),
            // `Buf`/`Blob` element storage never surfaces as a Raku-level value:
            // it lives in the buffer instance's attribute cell and only
            // `value::value_buf` reads it. Answer as the buffer it backs.
            ValueView::BufStorage(_) => "Buf",
            ValueView::RakuAst(node) => node.class.printed_name(),
            ValueView::Int(_) | ValueView::BigInt(_) => "Int",
            ValueView::Num(_) => "Num",
            ValueView::Str(_) => "Str",
            ValueView::Bool(_) => "Bool",
            ValueView::Rat(_, _) => "Rat",
            ValueView::FatRat(_, _) => "FatRat",
            ValueView::BigRat(_, _) => {
                if self.is_bigfatrat() {
                    "FatRat"
                } else {
                    "Rat"
                }
            }
            ValueView::Complex(_, _) => "Complex",
            // A native `array[T]` is a *distinct* type from `Array`: its MRO is
            // `array, Cool, Any, Mu`, so `.isa(array)` is True and
            // `.isa(Array)` is False (raku parity). Mirrors the `Map` arm below,
            // which reads the same embedded `declared_type`.
            ValueView::Array(ref items, ..)
                if items
                    .declared_type
                    .as_deref()
                    .is_some_and(|d| d == "array" || d.starts_with("array[")) =>
            {
                "array"
            }
            ValueView::Array(..) | ValueView::LazyList(_) => "Array",
            ValueView::Seq(_) => "Seq",
            ValueView::HyperSeq(_) => "HyperSeq",
            ValueView::RaceSeq(_) => "RaceSeq",
            ValueView::Hash(ref h) if h.declared_type.as_deref() == Some("Map") => "Map",
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
                // Every enum value does the `Enumeration` role.
                return type_name == "Enumeration" || enum_type.resolve() == type_name;
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
                if inner.isa_or_does_check(type_name, allow_roles) {
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
            ValueView::Scalar(inner) => return inner.isa_or_does_check(type_name, allow_roles),
            ValueView::ContainerRef(_) => {
                return self.with_deref(|inner| inner.isa_or_does_check(type_name, allow_roles));
            }
            ValueView::LazyThunk(thunk_data) => {
                let cache = thunk_data.cache.lock().unwrap();
                if let Some(ref cached) = *cache {
                    return cached.isa_or_does_check(type_name, allow_roles);
                }
                "Scalar" // unforced lazy thunk
            }
            ValueView::HashEntryRef { .. } => {
                return self
                    .hash_entry_read()
                    .isa_or_does_check(type_name, allow_roles);
            }
        };
        if my_type == type_name {
            return true;
        }
        if let ValueView::Package(name) = self.view() {
            let actual = name.resolve();
            if actual.starts_with("RakuAST::") && type_name.starts_with("RakuAST::") {
                return crate::rakuast::type_object_isa(&actual, type_name);
            }
        }
        // RakuAST node hierarchy (Phase 3): every node isa `RakuAST::Node`, a node
        // isa any `::`-namespace ancestor of its printed class name (e.g.
        // `Statement::If` isa `RakuAST::Statement`; the `::` boundary avoids a
        // false `StatementList isa Statement`), and a node isa its semantic
        // ancestors (`RakuAST::Term`/`RakuAST::Expression`) whose names are not
        // part of the printed class name.
        if let ValueView::RakuAst(node) = self.view() {
            if type_name == "RakuAST::Node" {
                return true;
            }
            if let Some(rest) = my_type.strip_prefix(type_name)
                && rest.starts_with("::")
            {
                return true;
            }
            if node.class.semantic_ancestors().contains(&type_name) {
                return true;
            }
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
        if allow_roles && self.does_role_hierarchy(type_name) {
            return true;
        }
        self.isa_nominal_hierarchy(type_name)
    }

    /// Nominal class-hierarchy table for built-in/primitive value types —
    /// real `.^mro` ancestors only, no roles. Consulted by `isa_check`
    /// (directly) and by `does_check` (as the "or is a real ancestor" half of
    /// `does = isa OR does-role`). Every entry here was verified against real
    /// `raku -e 'say TYPE.^mro'` output; a role name that merely reads like a
    /// supertype (e.g. `Numeric`, `Positional`, `Callable`) belongs in
    /// `does_role_hierarchy` instead, not here.
    fn isa_nominal_hierarchy(&self, type_name: &str) -> bool {
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
                    // Match.^mro is (Match Capture Cool Any Mu) — Cool is a real
                    // ancestor of Match. Capture itself is NOT (Capture.^mro is
                    // (Capture Any Mu), no Cool): `Capture.new.isa(Cool)` is
                    // False in real raku, verified 2026-08-22.
                    ValueView::Instance { class_name, .. }
                        if class_name == "Match"
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
            "FatRat" => {
                matches!(self.view(), ValueView::FatRat(_, _))
                    || (matches!(self.view(), ValueView::BigRat(_, _)) && self.is_bigfatrat())
            }
            // Bool.^mro is (Bool Int Cool Any Mu) — Bool really does nominally
            // extend Int in raku (`True.isa(Int)` is True).
            "Int" => matches!(self.view(), ValueView::Bool(_)),
            // Block.^mro/Routine.^mro/Code.^mro are real class chains
            // (Sub < Routine < Block < Code). `Callable` is the role they all
            // compose (`Sub.isa(Callable)` is False, `.does(Callable)` is
            // True) — see `does_role_hierarchy`.
            "Block" | "Routine" | "Code" => {
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
            // List.^mro is (List Cool Any Mu); Array < List. A genuine forced
            // `Seq` is its own `ValueView::Seq` and already answered by the
            // `my_type == type_name` fast path above — HyperSeq/RaceSeq do
            // NOT nominally descend from List or Seq (their `.^mro` is just
            // (HyperSeq/RaceSeq Any Mu)), verified 2026-08-22.
            "List" => {
                matches!(
                    self.view(),
                    ValueView::Array(..) | ValueView::LazyList(_) | ValueView::Slip(_)
                )
            }
            // Hash.^mro is (Hash Map Cool Any Mu) — Map is a real ancestor of
            // Hash. Pair/Set/Bag/Mix/Capture do NOT nominally descend from Map
            // (they only compose the Associative role): `(1=>2).isa(Map)` and
            // `Set.new.isa(Map)` are both False in real raku, verified
            // 2026-08-22 (`Associative` lives in `does_role_hierarchy`).
            "Map" => {
                matches!(self.view(), ValueView::Hash(..))
                    || matches!(
                        self.view(),
                        ValueView::Instance { attributes, .. }
                            if attributes.contains_key("__mutsu_hash_storage")
                    )
                    || matches!(
                        self.view(),
                        ValueView::Package(name)
                            if matches!(name.resolve().as_str(), "Hash" | "Map")
                    )
            }
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

    /// Role table for built-in/primitive value types — role names a value
    /// merely *does* (composes), NOT real `.^mro` ancestors. Consulted only by
    /// `does_check` (`.does()` / `~~` / role-aware smart-match), never by
    /// `isa_check`. The caller (`isa_or_does_check`) has already unwrapped
    /// wrapper views (Scalar, ContainerRef, forced LazyThunk, HashEntryRef,
    /// VarRef), so these arms match `self.view()` directly.
    fn does_role_hierarchy(&self, role_name: &str) -> bool {
        match role_name {
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
            "Stringy" => matches!(self.view(), ValueView::Str(_)),
            "Callable" => {
                matches!(
                    self.view(),
                    ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
                ) || matches!(
                    self.view(),
                    ValueView::Package(name)
                        if matches!(name.resolve().as_str(), "Sub" | "Routine" | "Method" | "Block" | "Code")
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
            // `Map` is NOT here — it is a real class (nominal, `isa_nominal_hierarchy`
            // handles it), unlike `Associative` which is the role Hash/Pair/Set/
            // Bag/Mix/Capture all compose. `(1=>2).does(Map)` is False in real
            // raku (verified 2026-08-22) even though `.does(Associative)` is True.
            "Associative" => {
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
                    ValueView::Instance { attributes, .. }
                        if attributes.contains_key("__mutsu_hash_storage")
                ) || matches!(
                    self.view(),
                    ValueView::Package(name)
                        if matches!(
                            name.resolve().as_str(),
                            "Hash" | "Map" | "Pair" | "Set" | "Bag" | "Mix" | "QuantHash" | "Capture"
                        )
                )
            }
            // A `Range` composes `Iterable` in Raku (`(0..2) ~~ Iterable` and
            // `Range ~~ Iterable` are both True), and so does a `Slip` (a List
            // subclass). Both were missing here even though the *type-name*
            // table in `signature.rs` already lists them, so a role-aware
            // smart-match disagreed with signature binding. `Set`/`Bag`/`Mix`
            // are deliberately NOT here: they do `Associative`, and
            // `(1,2).Set ~~ Iterable` is False in Raku.
            "Iterable" => {
                matches!(
                    self.view(),
                    ValueView::Array(..)
                        | ValueView::LazyList(_)
                        | ValueView::Hash(..)
                        | ValueView::Seq(_)
                        | ValueView::Slip(_)
                        | ValueView::HyperSeq(_)
                        | ValueView::RaceSeq(_)
                        | ValueView::Range(_, _)
                        | ValueView::RangeExcl(_, _)
                        | ValueView::RangeExclStart(_, _)
                        | ValueView::RangeExclBoth(_, _)
                        | ValueView::GenericRange { .. }
                ) || matches!(
                    self.view(),
                    ValueView::Package(name)
                        if matches!(
                            name.resolve().as_str(),
                            "Array" | "List" | "Range" | "Seq" | "Slip" | "Hash" | "Map"
                        )
                )
            }
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
        // `does` is `isa OR does-role`: consult the nominal hierarchy too
        // (e.g. `42.does(Cool)` is True in real raku), not just role names.
        self.isa_or_does_check(role_name, true)
    }
}
