//! What a scalar attribute store of a class has to check before it may write
//! the attribute's slot directly (the `$!x = <scalar>` lane of
//! `Interpreter::exec_set_local_attr_fast`).
//!
//! The answer is a function of the attribute's declaration alone, so it is
//! settled once per class layout ([`super::ClassLayout::store_kind`]) instead of
//! being re-derived from the registry on every store.

use super::{Value, ValueView};

/// The declared-type shape of one scalar attribute, as far as a store of an
/// inert scalar value (`Int`, `Num`, `Str`, `Bool`, ...) is concerned.
///
/// A kind never claims more than the general store would conclude: every
/// declaration it cannot describe exactly (a `where` clause, a smiley, an
/// `is Type` trait, any class or role constraint, an `@`/`%` attribute) is
/// [`Self::Decline`], and a store into it takes the general path.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AttrStoreKind {
    /// The general store decides.
    Decline,
    /// No constraint (or `Mu`/`Any`): every inert scalar is accepted as is.
    Untyped,
    /// `int` / `int64`: an `Int` is stored as is.
    NativeInt,
    /// `str`: a `Str` is stored as is.
    NativeStr,
    /// `num` / `num64`: a `Num` is stored as is.
    NativeNum,
    /// `Int`: an `Int` or `BigInt`.
    CoreInt,
    /// `Str`.
    CoreStr,
    /// `Num`.
    CoreNum,
    /// `Bool`.
    CoreBool,
}

impl AttrStoreKind {
    /// The kind of a scalar attribute whose declared constraint is `constraint`
    /// (as `attribute_type_constraint` reports it, smiley excluded).
    // Cost: O(1), a short string compare.
    pub(crate) fn classify(constraint: Option<&str>) -> Self {
        match constraint.map(str::trim) {
            None | Some("Mu" | "Any") => Self::Untyped,
            Some("int" | "int64") => Self::NativeInt,
            Some("str") => Self::NativeStr,
            Some("num" | "num64") => Self::NativeNum,
            Some("Int") => Self::CoreInt,
            Some("Str") => Self::CoreStr,
            Some("Num") => Self::CoreNum,
            Some("Bool") => Self::CoreBool,
            Some(_) => Self::Decline,
        }
    }

    /// Whether a constraint is declared at all, so that a user `subset` may
    /// have redirected its name.
    // Cost: O(1).
    pub(crate) fn is_typed(self) -> bool {
        !matches!(self, Self::Decline | Self::Untyped)
    }

    /// Whether storing `value` (an inert scalar payload) leaves it unchanged
    /// and satisfies the declaration.
    // Cost: O(1).
    pub(crate) fn accepts(self, value: &Value) -> bool {
        match self {
            Self::Decline => false,
            Self::Untyped => true,
            Self::NativeInt => matches!(value.view(), ValueView::Int(_)),
            Self::NativeStr | Self::CoreStr => value.is_str_value(),
            Self::NativeNum | Self::CoreNum => matches!(value.view(), ValueView::Num(_)),
            Self::CoreInt => matches!(value.view(), ValueView::Int(_) | ValueView::BigInt(_)),
            Self::CoreBool => matches!(value.view(), ValueView::Bool(_)),
        }
    }
}
