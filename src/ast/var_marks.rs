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
    /// The parameter declaration of a pointy `given` / `with` block
    /// (`given $x -> $v is copy { ... }`) that takes a fresh, writable copy of the topic
    /// with no write-back to the source.
    pub(crate) pointy_copy: bool,
    /// The same declaration for a native-typed parameter, which cannot use `:=`; the
    /// compiler recognizes it as the pointy parameter so `is rw` writes back.
    pub(crate) pointy_native_param: bool,
    /// The declaration reads the new topic exactly once; after it the block's `$_` is the
    /// enclosing topic again, so the compiler emits the matching `ExitPointyTopic`.
    pub(crate) restore_pointy_given_topic: bool,
    /// Built from `my \name = ...` / `my \name := ...`: the declaration is the sigilless term,
    /// which binds its initializer whichever spelling was written.
    pub(crate) sigilless_decl: bool,
    /// An expression-position `my Foo $x .= new` whose initializer is the dotty `.=` marker.
    pub(crate) method_assign_decl: bool,
    /// A declaration a lifted BEGIN gave a static cell (ADR-0134 slice 2): it is already its
    /// static half and its initializer reads the static value.
    pub(crate) begin_static: bool,
    /// The static half of a `:D` scalar, which holds its nominal type object until the
    /// run-time assignment.
    pub(crate) begin_static_definite: bool,
    /// A `:=` declaration that aliases the container its initializer names
    /// (`my @a := @b`, `my $x := $y`, a pointy parameter): the compiler binds
    /// instead of storing a copy, so the bound container keeps its type (a
    /// List stays a List). Not part of [`VarMarks::is_plain`]: the declaration
    /// sat in a bind group before the mark existed and every "plain `my`"
    /// test still saw it as plain.
    pub(crate) bound: bool,
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
            && !self.pointy_copy
            && !self.pointy_native_param
            && !self.restore_pointy_given_topic
            && !self.sigilless_decl
            && !self.method_assign_decl
            && !self.begin_static
            && !self.begin_static_definite
    }
}

/// `decl` with its [`VarMarks::bound`] mark set; anything but a `VarDecl` is
/// returned as is.
// Cost: O(1).
pub(crate) fn with_bound_mark(mut decl: super::Stmt) -> super::Stmt {
    if let super::Stmt::VarDecl { marks, .. } = &mut decl {
        marks.bound = true;
    }
    decl
}

/// Whether `stmts` is a bind group: a parser or compiler expansion with a
/// declaration marked [`VarMarks::bound`].
// Cost: O(s), s = statements.
pub(crate) fn is_bind_group(stmts: &[super::Stmt]) -> bool {
    stmts
        .iter()
        .any(|s| matches!(s, super::Stmt::VarDecl { marks, .. } if marks.bound))
}
