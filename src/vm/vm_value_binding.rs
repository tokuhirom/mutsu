use super::*;

impl Interpreter {
    /// True when a `$`-sigil `:=` bind SOURCE has no assignable container
    /// behind it, so a later `$x = v` through the bound name is rakudo's
    /// X::AdHoc "Cannot assign to an immutable value".
    ///
    /// An actual Scalar, Proxy or deferred entry keeps its assignable home.
    /// Every other scalar value binds as the value itself, including code,
    /// buffers and objects with mutable attributes. Whole aggregate containers
    /// and type objects have their separate declaration/error rules below.
    // Cost: O(1).
    pub(super) fn bind_source_has_no_container(v: &Value) -> bool {
        match v.view() {
            ValueView::ContainerRef(_)
            | ValueView::ContainerView(_)
            | ValueView::Scalar(_)
            | ValueView::Proxy { .. }
            | ValueView::HashEntryRef { .. }
            | ValueView::VarRef { .. } => false,
            ValueView::Array(_, kind) => kind.is_immutable_list(),
            // These have declaration-only aggregate rules or the separate
            // type-object error kind below, rather than a Scalar container.
            ValueView::Hash(_)
            | ValueView::Pair(..)
            | ValueView::ValuePair(..)
            | ValueView::Package(_) => false,
            _ => true,
        }
    }

    /// True when a `$`-sigil `:=` bind SOURCE is a container that is not a
    /// **Scalar** container, so a later whole-value `$x = v` through the bound
    /// name is rakudo's X::AdHoc "Cannot assign to an immutable value".
    ///
    /// rakudo's rule is sharper than "immutable": `$x = v` needs `$x` bound to a
    /// Scalar, and no other container qualifies — a real `Array`, a `Hash`, a
    /// `Map` and a `Pair` all refuse it, though each is mutable through its own
    /// interface. `my $x := @a; $x = 5` is a hard error there and used to
    /// overwrite `@a` here.
    ///
    /// Separate from [`Self::bind_source_has_no_container`] because the two say
    /// different things and are gated differently. That one is about
    /// immutability and applies to any `:=`; this one is about *which kind of
    /// container the name owns*, and its caller restricts it to a DECLARATION —
    /// a parameter bind reaches the same store, and an `is raw` / `\x`
    /// parameter bound to an array must stay assignable.
    ///
    /// The aliasing these binds exist for is untouched: only the whole-value
    /// `=` is refused, so `$x.push(9)`, `$x<k> = 2` and `$x[0]` keep working.
    /// True when a `$`-sigil `:=` bind SOURCE is an itemized aggregate
    /// (`$(%h)`, `$(@a)`, `(1, 2).item`): the value sits in a `Scalar`, so the
    /// name binds that Scalar -- `.VAR.^name` is `Scalar` and `=:=` sees a
    /// container -- but the Scalar is readonly, and an assignment is rakudo's
    /// "Cannot assign to a readonly variable or a value" (#11129).
    // Cost: O(1).
    pub(super) fn bind_source_is_itemized_aggregate(v: &Value) -> bool {
        match v.view() {
            ValueView::Hash(_) => v.hash_is_itemized(),
            ValueView::Array(_, kind) => kind.is_itemized(),
            ValueView::Scalar(inner) => {
                matches!(inner.view(), ValueView::Array(..) | ValueView::Hash(_))
            }
            _ => false,
        }
    }

    // Cost: O(1).
    pub(super) fn bind_source_is_non_scalar_container(v: &Value) -> bool {
        matches!(
            v.view(),
            ValueView::Array(..)
                | ValueView::Hash(_)
                | ValueView::Pair(..)
                | ValueView::ValuePair(..)
        )
    }

    /// True when a `$`-sigil `:=` bind SOURCE is a bare TYPE OBJECT (`$s :=
    /// IB`, `$s := Int`) — no Scalar container at all, so a later `$s = v` is
    /// rakudo's own "assign requires a concrete object (got a IB type object
    /// instead)", not the generic immutable-value wording.
    ///
    /// Kept apart from [`Self::bind_source_has_no_container`] —
    /// which deliberately excludes `Package` — because the two shapes raise
    /// DIFFERENT exceptions ([`crate::ast::ReadonlyKind::TypeObject`] vs.
    /// `::Immutable`) even though both leave the name with no container.
    // Cost: O(1).
    pub(super) fn bind_source_is_type_object(v: &Value) -> bool {
        matches!(v.view(), ValueView::Package(_))
    }

    /// True when `v` reads the same through a binding cell as it does bare.
    /// A cell itemizes what it holds, so a list-like value (`my $r := 1..3`,
    /// a `Seq`, an immutable `List`) iterates as one item behind one, while
    /// rakudo iterates the value a container-less `$r` is bound to. Those
    /// keep a bare slot and only the readonly registry's mark.
    // Cost: O(1).
    pub(super) fn binding_cell_keeps_value_semantics(v: &Value) -> bool {
        !matches!(
            v.view(),
            ValueView::Array(..)
                | ValueView::Hash(_)
                | ValueView::Pair(..)
                | ValueView::ValuePair(..)
                | ValueView::Range(..)
                | ValueView::RangeExcl(..)
                | ValueView::RangeExclStart(..)
                | ValueView::RangeExclBoth(..)
                | ValueView::GenericRange { .. }
                | ValueView::Seq(_)
                | ValueView::HyperSeq(_)
                | ValueView::RaceSeq(_)
                | ValueView::LazyList(_)
                | ValueView::Slip(_)
                | ValueView::Mixin(..)
                | ValueView::Scalar(_)
                | ValueView::Proxy { .. }
                | ValueView::ContainerRef(_)
                | ValueView::ContainerView(_)
                | ValueView::HashEntryRef { .. }
                | ValueView::VarRef { .. }
        )
    }

    /// Seat a containerless value in the binding's own immutable home when
    /// doing so preserves its iteration and itemization semantics.
    // Cost: O(1).
    pub(crate) fn readonly_value_binding(value: Value, kind: crate::ast::ReadonlyKind) -> Value {
        if Self::binding_cell_keeps_value_semantics(&value) {
            Value::container_ref(crate::gc::Gc::new(
                crate::value::ContainerCell::new_readonly_binding(value, kind),
            ))
        } else {
            value
        }
    }

    /// The readonly kind a `$` variable `:=`-bound straight to the value `v`
    /// has: `TypeObject` for a type object, `Immutable` for a value with no
    /// container of its own (the shapes `bind_marks_type_object` and
    /// `bind_marks_immutable` mark at a declaration), else none.
    // Cost: O(1).
    pub(super) fn rebound_value_kind(v: &Value) -> Option<crate::ast::ReadonlyKind> {
        if Self::bind_source_is_type_object(v) {
            Some(crate::ast::ReadonlyKind::TypeObject)
        } else if Self::bind_source_has_no_container(v) {
            Some(crate::ast::ReadonlyKind::Immutable)
        } else {
            None
        }
    }

    /// Classify a declaration's source without consulting a name marker.
    // Cost: O(1).
    pub(super) fn value_declaration_readonly_kind(
        value: &Value,
        source: Option<&str>,
    ) -> Option<crate::ast::ReadonlyKind> {
        match source {
            None if Self::bind_source_is_itemized_aggregate(value) => {
                Some(crate::ast::ReadonlyKind::Alias)
            }
            None => Self::rebound_value_kind(value).or_else(|| {
                Self::bind_source_is_non_scalar_container(value)
                    .then_some(crate::ast::ReadonlyKind::Immutable)
            }),
            Some(name) if name.starts_with(['@', '%']) => Some(crate::ast::ReadonlyKind::Immutable),
            Some(_) => None,
        }
    }
}
