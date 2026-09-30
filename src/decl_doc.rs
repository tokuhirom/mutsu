//! Declarator documentation (`#|` leading, `#=` trailing) as parser output.
//!
//! The parser attaches every declarator doc comment to the declaration it
//! documents (`parser::decl_doc`, ADR-0134). What it hands the rest of the
//! interpreter is described here:
//!
//! - [`DocSlot`] — the doc text on an anonymous code node (`anon sub`,
//!   `sub { }`, a block used as a value). The node itself carries it, the
//!   compiler copies it onto the closure's `CompiledCode`, and `.WHY` on the
//!   resulting code object reads it from there.
//! - [`DocComment`] — one documented declaration of a compilation unit, in
//!   source order, keyed by the structural name `.WHY` and the `$=pod`
//!   builder look a named declaration up by (`&name`, `Class::method`,
//!   `Class::$!attr`, `&routine::$param`, `Name/role.1`, `&name/multi.0`).

/// The text of one declaration's doc comments.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct DeclDoc {
    /// The `#|` comments written before the declaration, joined by a space.
    pub(crate) leading: Option<String>,
    /// The `#=` comments written after it, joined by a space.
    pub(crate) trailing: Option<String>,
}

impl DeclDoc {
    pub(crate) fn is_empty(&self) -> bool {
        self.leading.is_none() && self.trailing.is_none()
    }

    /// `Pod::Block::Declarator.contents`: leading and trailing joined by a
    /// newline.
    pub(crate) fn contents(&self) -> String {
        match (&self.leading, &self.trailing) {
            (Some(l), Some(t)) => format!("{l}\n{t}"),
            (Some(l), None) => l.clone(),
            (None, Some(t)) => t.clone(),
            (None, None) => String::new(),
        }
    }
}

/// The declarator documentation of an anonymous code node.
///
/// Which comments document a declaration is only known once the whole
/// compilation unit is parsed (a `#|` documents the *next* declaration,
/// wherever it starts), but the node is built -- and memoized, and cloned --
/// long before that. So the parser gives the node an empty slot, keeps a
/// handle to it, and fills it exactly once when the unit's parse completes
/// (`parser::decl_doc::finish_unit`); every clone of the node shares the one
/// slot. Nothing reads a slot before then: the unit is compiled after it is
/// parsed. It serializes, hashes and compares as its content.
#[derive(Clone, Debug, Default)]
pub(crate) struct DocSlot(Option<std::sync::Arc<std::sync::OnceLock<DeclDoc>>>);

impl DocSlot {
    /// A slot the parser fills in later.
    pub(crate) fn pending() -> Self {
        Self(Some(std::sync::Arc::default()))
    }

    /// Fill the slot; a second fill is ignored.
    pub(crate) fn fill(&self, doc: DeclDoc) {
        if let Some(cell) = &self.0
            && !doc.is_empty()
        {
            let _ = cell.set(doc);
        }
    }

    pub(crate) fn get(&self) -> Option<&DeclDoc> {
        self.0.as_ref().and_then(|cell| cell.get())
    }
}

impl PartialEq for DocSlot {
    fn eq(&self, other: &Self) -> bool {
        self.get() == other.get()
    }
}

impl std::hash::Hash for DocSlot {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.get().hash(state);
    }
}

impl serde::Serialize for DocSlot {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.get().serialize(serializer)
    }
}

impl<'de> serde::Deserialize<'de> for DocSlot {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let doc = Option::<DeclDoc>::deserialize(deserializer)?;
        Ok(match doc {
            Some(doc) => {
                let slot = Self::pending();
                slot.fill(doc);
                slot
            }
            None => Self::default(),
        })
    }
}

/// Kind of declaration a doc comment is attached to.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub(crate) enum DocDeclKind {
    /// class, module, package, grammar, role, enum, subset
    #[default]
    Package,
    /// sub, method, submethod, proto, and an anonymous routine
    Sub,
    /// token, rule, regex
    GrammarRule,
    /// has $.attr
    Attr,
    /// a documented parameter
    Param,
    /// a block used as a value (`my $b = {; ... }`)
    Block,
}

/// One documented declaration of a compilation unit.
#[derive(Clone, Debug, Default, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub(crate) struct DocComment {
    pub(crate) doc: DeclDoc,
    /// The declaration's own name (a package's qualified name, `&sub`,
    /// `Class::method`, `Class::$!attr`, a parameter's sigiled name).
    pub(crate) wherefore_name: String,
    /// The key this comment is filed under: `wherefore_name`, uniquified where
    /// one name covers several declarations (`&mm/multi.1`, `R/role.1`) and
    /// scoped to its owner for a parameter (`&doc-sub::$a`). The `$=pod`
    /// builder files the concrete declarant values under the same keys.
    pub(crate) key: String,
    pub(crate) kind: DocDeclKind,
    /// A `proto` declaration.
    pub(crate) is_proto: bool,
    /// The routine's return type (`anon Str sub {}` has `Str`).
    pub(crate) return_type: Option<String>,
    /// For a routine: `Method` or `Submethod` when it is one.
    pub(crate) callable_type_override: Option<String>,
    /// True for an anonymous routine or block: it has no name to be found
    /// by, so `.WHY` reaches it through the code object instead (see
    /// [`DeclDoc`]), and only `$=pod` lists it here.
    pub(crate) is_anonymous: bool,
}
