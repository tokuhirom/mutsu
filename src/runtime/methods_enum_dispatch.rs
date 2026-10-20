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

    /// What an enum member answers beyond its shape's rows (the roles Rakudo
    /// composes into every enum type, found by the opaque shape of the member
    /// by its payload kind): a method the enumeration roles do not declare is
    /// the **base type's** row, answered on the member's value (`y.Num`,
    /// `y.is-prime`, `p.uc`), then `Any(Enumeration)`'s key/value views of the
    /// whole enumeration (`red.pairs`, ADR-11276 §9.145). `None` when neither
    /// answers.
    ///
    /// This runs late, not in the receiver step: a member's `WHICH` and the
    /// `Cool` text methods read through its *key* are answered before it, and
    /// a redirect to the payload would take them over.
    // Cost: O(o) to find the row, o = the member's base types, plus the row's own.
    pub(super) fn dispatch_enum_method(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Enum { value, .. } = target.view() else {
            return None;
        };
        let base: &[&str] = match value {
            EnumValue::Int(_) => &["Int", "Cool"],
            EnumValue::Str(_) => &["Str", "Cool"],
            EnumValue::Generic(_) => &[],
        };
        if !base.is_empty()
            && !crate::builtins::method_introspection::native_method_declared("Enumeration", method)
            && let Some(result) =
                crate::builtins::method_table::invoke_owner(self, base, method, args, || {
                    value.to_value()
                })
        {
            return Some(result);
        }
        crate::builtins::method_table::any_enum_views::invoke(self, target, method, args)
    }

    /// Whether an enum member has `method` for `.^can`: a row of its shape's
    /// roles, its base type's row, or one of the views of the enumeration.
    // Cost: a few set probes.
    pub(crate) fn enum_member_has_method(target: &Value, method: &str) -> bool {
        let method = Symbol::intern(method);
        let ValueView::Enum { value, .. } = target.view() else {
            return false;
        };
        let owners = target.receiver_shape().and_then(|shape| shape.opaque_owners());
        owners
            .into_iter()
            .flatten()
            .chain(match value {
                EnumValue::Int(_) => ["Int", "Cool"].as_slice(),
                EnumValue::Str(_) => ["Str", "Cool"].as_slice(),
                EnumValue::Generic(_) => [].as_slice(),
            })
            .any(|owner| {
                (0..4).any(|arity| {
                    crate::builtins::method_table::owner_row(Symbol::intern(owner), method, arity)
                        .is_some()
                })
            })
            || crate::builtins::method_table::any_enum_views::declares(method.as_str())
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
