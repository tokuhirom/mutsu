use super::*;


impl Interpreter {
    /// `my Array enum E (:a[1, 2])`: a member is also an instance of the base
    /// type, so the base's collection methods answer. The key/value views are
    /// not among them: `Any(Enumeration)`'s candidates list the enumeration
    /// (`a.keys` is `(a)`, as in Rakudo). The base value when `target`
    /// is such a member and `method` is one of those methods.
    // Cost: O(1).
    pub(crate) fn enum_aggregate_base(target: &Value, method: &str) -> Option<Value> {
        if !matches!(
            method,
            "elems" | "end" | "list" | "List" | "Array" | "max" | "min" | "map" | "grep" | "first"
                | "flat" | "join" | "reverse" | "sort" | "head" | "tail" | "sum" | "iterator"
                | "AT-POS" | "EXISTS-POS" | "Seq" | "rotate" | "batch" | "produce" | "reduce"
                | "unique" | "squish" | "pick" | "roll"
        ) {
            return None;
        }
        let ValueView::Enum {
            value: EnumValue::Generic(inner),
            ..
        } = target.view()
        else {
            return None;
        };
        matches!(inner.view(), ValueView::Array(..) | ValueView::Hash(_))
            .then(|| inner.as_ref().clone())
    }

    /// The methods of an enum member: its base type's collection methods for
    /// an `Array`/`Hash`-valued member ([`Self::enum_aggregate_base`]), then
    /// the rows of the roles Rakudo composes into every enum type
    /// (`Enumeration`, `NumericEnumeration`, `StringyEnumeration`; ADR-11276
    /// §9.125), found by their owner. `None` when neither answers.
    // Cost: O(o) to find the row, o = the member's roles, plus the row's own.
    pub(super) fn dispatch_enum_method(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let owners = crate::builtins::method_table::enumeration_owners_of(target)?;
        // `Mu.perl` is `self.raku`, so the row of `raku` answers it.
        let method = if method == "perl" { "raku" } else { method };
        if let Some(base) = Self::enum_aggregate_base(target, method) {
            return Some(self.call_method_with_values(base, method, args.to_vec()));
        }
        if let Some(result) =
            crate::builtins::method_table::invoke_owner(self, owners, method, args, || {
                target.clone()
            })
        {
            return Some(result);
        }
        // A member is a value of its base type (Rakudo's enum type inherits from
        // it): a method the enumeration roles do not declare is the base type's
        // row, answered on the member's value (`y.Num`, `y.is-prime`, `p.uc`).
        let ValueView::Enum { value, .. } = target.view() else {
            return None;
        };
        let base: &[&str] = match value {
            EnumValue::Int(_) => &["Int"],
            EnumValue::Str(_) => &["Str"],
            EnumValue::Generic(_) => &[],
        };
        if !base.is_empty()
            && !crate::builtins::native_method_row::native_method_declared("Enumeration", method)
            && let Some(result) =
                crate::builtins::method_table::invoke_owner(self, base, method, args, || {
                    value.to_value()
                })
        {
            return Some(result);
        }
        // Then `Any(Enumeration)`'s key/value views of the whole enumeration
        // (`red.pairs`, ADR-11276 §9.145).
        crate::builtins::method_table::any_enum_views::invoke(self, target, method, args)
    }

    /// The `enums` of an enumeration, in declaration order: of an enum's type
    /// object or one of its values, `Bool` included. `None` for anything else.
    // Cost: O(v), v = values of the enumeration (one registry probe and a copy).
    pub(crate) fn enumeration_variants(&self, target: &Value) -> Option<Vec<(String, EnumValue)>> {
        let type_name = match target.view() {
            ValueView::Package(name) => name.resolve(),
            // An enum value (`red` of `enum Color`) carries its enum type.
            ValueView::Enum { enum_type, .. } => enum_type.resolve(),
            // `True` and `False` are the values of the core's `Bool` enum.
            ValueView::Bool(_) => "Bool".to_string(),
            _ => return None,
        };
        if type_name == "Bool" {
            return Some(crate::builtins::method_table::any_enum_views::bool_variants().to_vec());
        }
        self.registry().enum_types.get(&type_name).cloned()
    }

    /// `Enumeration.enums`: a `Map` of each value's name to its value.
    // Cost: O(v), v = values of the enumeration.
    pub(crate) fn dispatch_enums(&mut self, target: &Value) -> Option<Result<Value, RuntimeError>> {
        let variants = self.enumeration_variants(target)?;
        let mut map = ValueMap::default();
        for (k, v) in variants {
            map.insert(k, v.to_value());
        }
        Some(Ok(self.tag_container_metadata(
            Value::hash(map),
            ContainerTypeInfo {
                value_type: String::new(),
                key_type: None,
                declared_type: Some("Map".to_string()),
            },
        )))
    }
}
