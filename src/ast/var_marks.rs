//! The facts about a `VarDecl` that are neither its traits nor its type: how
//! the parser desugared it, and what the program wrote that the canonical tree
//! drops (ADR-10723 section 6.125). They used to be `__`-prefixed entries of
//! `custom_traits`, which every consumer had to filter back out.

use super::Expr;

/// See the module documentation. `Default` is the plain declaration.
#[derive(Debug, Clone, Default, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct VarMarks {
    /// The initializer reads the binding the declaration introduces
    /// (`my $x = do { $x }`, `my $*X = $*X + 1`, #9770), found by the scope
    /// pass after parsing or lowering: the read must see a fresh `Any`, not
    /// the outer binding the declaration shadows.
    pub(crate) init_sees_self: bool,
    /// An initialized shaped array, `my @a[3] = ...`: its initializer is
    /// `Array.new(shape => ..., data => ...)`, and the value-copy of a plain
    /// `my @u = @shaped` must not strip the shape.
    pub(crate) shaped: bool,
    /// A key-typed hash (`my %h{Str}`) whose `Any` value type the parser
    /// supplied; `my Any %h{Str}` writes it, and RakuAST tells them apart.
    pub(crate) implicit_value_type: bool,
    /// In a `.AST` parse, the colonpairs of the declared name as written
    /// (`:foo<g h>` of `my $today:foo<g h>`); the name itself is canonical.
    pub(crate) name_colonpairs: Option<Box<str>>,
    /// The argument of `is dynamic(ARG)`, kept for the RakuAST view; the trait
    /// itself is the declaration's `is_dynamic` flag.
    pub(crate) dynamic_argument: Option<Box<Expr>>,
}

impl VarMarks {
    /// True for a declaration the parser built without any of these marks,
    /// which every "plain `my` declaration" test needs (a guard spelled
    /// `custom_traits.is_empty()` once excluded the marked ones by accident).
    // Cost: O(1).
    pub(crate) fn is_plain(&self) -> bool {
        !self.init_sees_self
            && !self.shaped
            && !self.implicit_value_type
            && self.name_colonpairs.is_none()
            && self.dynamic_argument.is_none()
    }
}
