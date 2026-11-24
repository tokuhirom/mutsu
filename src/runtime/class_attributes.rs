//! A class's declared attribute list (`ClassDef::attributes`), which a
//! built-in exception class fills from `builtin_exception_attrs` on first use
//! (#18956).
//!
//! Every built-in `X::` class declares the attributes rakudo's does (#18654),
//! but declaring all ~2,100 of them for 337 classes cost every `mutsu`
//! process about as much as the rest of its startup. A class marked with
//! [`ClassAttributes::defer_builtin_exception`] keeps the attributes it
//! already has and adds the table's rows the first time anything reads or
//! changes the list, so a program that never touches an exception class never
//! pays for its metadata. A read and a write see the same list as the former
//! eager declaration: the rows come after the attributes declared before the
//! mark, and before anything pushed later.

use super::ClassAttributeDef;
use crate::symbol::Symbol;
use std::sync::OnceLock;

/// `ClassDef::attributes`: a `Vec<ClassAttributeDef>` through `Deref`, filled
/// on first access for a built-in exception class.
#[derive(Debug, Clone, Default)]
pub(crate) struct ClassAttributes {
    /// The declaration; for a deferred class, only the attributes declared
    /// before it was marked.
    attrs: Vec<ClassAttributeDef>,
    /// Table attributes a built-in method row already reads: built from their
    /// named argument (`is built`) although they are declared private.
    built: Vec<&'static str>,
    /// The built-in exception class whose table rows `attrs` still lacks.
    /// A read declares them into its cell; the first write moves the cell's
    /// declaration into `attrs` and drops this.
    deferred: Option<Deferred>,
}

#[derive(Debug, Clone)]
struct Deferred {
    class: Symbol,
    declared: OnceLock<Declared>,
}

#[derive(Debug, Clone, Default)]
struct Declared {
    attrs: Vec<ClassAttributeDef>,
    built: Vec<&'static str>,
}

impl ClassAttributes {
    /// Declares built-in exception class `class`'s table attributes on first
    /// access, after the ones the list holds now.
    // Cost: O(1).
    pub(crate) fn defer_builtin_exception(&mut self, class: Symbol) {
        self.deferred = Some(Deferred {
            class,
            declared: OnceLock::new(),
        });
    }

    /// Whether the table rows are still undeclared: the class's accessor rows
    /// are not in the method table yet, and `Registry::accessor_is_public_sym`
    /// answers from this list instead.
    // Cost: O(1).
    pub(crate) fn is_deferred(&self) -> bool {
        self.deferred
            .as_ref()
            .is_some_and(|deferred| deferred.declared.get().is_none())
    }

    // Cost: O(r), r = the deferred class's table rows (plus the one-time
    // table parse).
    #[cold]
    fn materialize(&self, class: Symbol) -> Declared {
        let mut attrs = self.attrs.clone();
        let mut built = self.built.clone();
        built.extend(super::builtin_exception_attrs::declare(class.as_str(), &mut attrs));
        Declared { attrs, built }
    }

    // Cost: O(r) on the first call for a deferred class, r = its table rows;
    // O(1) after that and for any other class.
    #[inline]
    fn declared(&self) -> (&Vec<ClassAttributeDef>, &[&'static str]) {
        match &self.deferred {
            None => (&self.attrs, &self.built),
            Some(deferred) => {
                let declared = deferred
                    .declared
                    .get_or_init(|| self.materialize(deferred.class));
                (&declared.attrs, &declared.built)
            }
        }
    }

    /// The attributes declared outside the table, for a write that only
    /// concerns those: a deferred class answers its pre-table list without
    /// declaring the table rows, and any other class its whole list.
    ///
    /// The one caller hands a role's captured lexical scope to the composed
    /// copies of the role's attributes (`give_role_captures_to_composed_
    /// classes`). A table row's default is a literal that needs no scope, so
    /// leaving the rows undeclared there changes nothing, and declaring them
    /// would undo the deferral for every `X::Comp` class each time a module
    /// that declares the role loads.
    // Cost: O(1).
    pub(crate) fn without_table_mut(&mut self) -> &mut Vec<ClassAttributeDef> {
        if self.is_deferred() {
            &mut self.attrs
        } else {
            self
        }
    }

    /// Whether `name` is a table attribute built from its named argument
    /// although declared private (`X::AdHoc`'s `$!payload`); what an explicit
    /// `is built` records in `ClassDef::attribute_built`.
    // Cost: O(b), b = such attributes of this class (0 or 1 in practice).
    pub(crate) fn builtin_is_built(&self, name: &str) -> bool {
        self.declared().1.contains(&name)
    }
}

impl std::ops::Deref for ClassAttributes {
    type Target = Vec<ClassAttributeDef>;

    // Cost: O(1) after the first access (see `declared`).
    #[inline]
    fn deref(&self) -> &Vec<ClassAttributeDef> {
        self.declared().0
    }
}

impl std::ops::DerefMut for ClassAttributes {
    // Cost: O(1) after the first access (see `declared`).
    #[inline]
    fn deref_mut(&mut self) -> &mut Vec<ClassAttributeDef> {
        if let Some(deferred) = self.deferred.take() {
            let declared = deferred
                .declared
                .into_inner()
                .unwrap_or_else(|| self.materialize(deferred.class));
            self.attrs = declared.attrs;
            self.built = declared.built;
        }
        &mut self.attrs
    }
}

impl From<Vec<ClassAttributeDef>> for ClassAttributes {
    // Cost: O(1).
    fn from(attrs: Vec<ClassAttributeDef>) -> Self {
        Self {
            attrs,
            built: Vec::new(),
            deferred: None,
        }
    }
}

impl FromIterator<ClassAttributeDef> for ClassAttributes {
    // Cost: O(n), n = the attributes.
    fn from_iter<I: IntoIterator<Item = ClassAttributeDef>>(iter: I) -> Self {
        Self::from(iter.into_iter().collect::<Vec<_>>())
    }
}

impl<'a> IntoIterator for &'a ClassAttributes {
    type Item = &'a ClassAttributeDef;
    type IntoIter = std::slice::Iter<'a, ClassAttributeDef>;

    // Cost: O(1).
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a> IntoIterator for &'a mut ClassAttributes {
    type Item = &'a mut ClassAttributeDef;
    type IntoIter = std::slice::IterMut<'a, ClassAttributeDef>;

    // Cost: O(1).
    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

#[cfg(test)]
mod tests {
    use super::ClassAttributes;
    use crate::symbol::Symbol;

    /// Writing through `without_table_mut` leaves a deferred class deferred,
    /// so the table rows are still declared on the next real access
    /// (#18956: role registration must not force every `X::Comp` class).
    #[test]
    fn a_write_outside_the_table_keeps_the_class_deferred() {
        let mut attrs = ClassAttributes::default();
        attrs.defer_builtin_exception(Symbol::intern("X::NYI"));
        assert!(attrs.is_deferred());
        assert!(attrs.without_table_mut().is_empty());
        assert!(attrs.is_deferred(), "the pre-table write declared nothing");
        assert!(attrs.iter().any(|a| a.name == "feature"));
        assert!(!attrs.is_deferred());
        // Once declared, the write sees the whole list.
        assert!(attrs.without_table_mut().iter().any(|a| a.name == "feature"));
    }
}
