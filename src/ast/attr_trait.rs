//! The order an attribute's traits were written in.
//!
//! `Stmt::HasDecl` keeps each trait in a field of its own (`is_rw`,
//! `is_required`, `unknown_traits`, `handles_terms`, ...), which says what the
//! traits are but not in which order they came: `has $.x is rw is required` and
//! `has $.x is required is rw` parse to the same fields. Rakudo's
//! `VarDeclaration::Simple.traits` is in written order, so the parser also
//! records the order as a list of [`AttrTrait`] kinds. A kind that can occur
//! more than once ([`AttrTrait::Custom`], [`AttrTrait::Handles`]) takes the next
//! entry of its own list (`unknown_traits`, `handles_terms`). An attribute built
//! without written traits (`my $.x`, whose `is_rw` is implied) has an empty
//! list, and so renders no trait.

/// One written attribute trait, by kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum AttrTrait {
    /// `is rw`.
    Rw,
    /// `is readonly`.
    Readonly,
    /// `is required` / `is required("reason")`.
    Required,
    /// `is default(EXPR)`.
    Default,
    /// `is built` / `is built(False)`.
    Built,
    /// `is DEPRECATED` / `is DEPRECATED("message")`.
    Deprecated,
    /// `is TYPE`, a container type of an `@` / `%` attribute (`is Buf`).
    Type,
    /// The next entry of `unknown_traits`: any other `is NAME`, a `will` or a
    /// `does`.
    Custom,
    /// The next `handles` clause, an entry of `handles_terms`.
    Handles,
}
