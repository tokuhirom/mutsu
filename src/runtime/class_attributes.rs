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
    /// The built-in exception class whose table rows `list` still lacks, with
    /// the attributes declared before it was marked. `None` once `list` holds
    /// the full declaration, and for every other class.
    deferred: Option<Box<Deferred>>,
    list: OnceLock<Declared>,
}

#[derive(Debug, Clone)]
struct Deferred {
    class: Symbol,
    base: Vec<ClassAttributeDef>,
}

#[derive(Debug, Clone, Default)]
struct Declared {
    attrs: Vec<ClassAttributeDef>,
    /// Table attributes a built-in method row already reads: built from their
    /// named argument (`is built`) although they are declared private.
    built: Vec<&'static str>,
}

impl ClassAttributes {
    /// Declares built-in exception class `class`'s table attributes on first
    /// access, after the ones the list holds now.
    // Cost: O(1).
    pub(crate) fn defer_builtin_exception(&mut self, class: Symbol) {
        let base = self.list.take().map(|d| d.attrs).unwrap_or_default();
        self.deferred = Some(Box::new(Deferred { class, base }));
    }

    // Cost: O(r) on the first call for a deferred class, r = its table rows
    // (plus the one-time table parse); O(1) after that and for any other class.
    #[inline]
    fn declared(&self) -> &Declared {
        self.list.get_or_init(|| self.materialize())
    }

    // Cost: O(r), r = the deferred class's table rows.
    #[cold]
    fn materialize(&self) -> Declared {
        let Some(deferred) = &self.deferred else {
            return Declared::default();
        };
        let mut attrs = deferred.base.clone();
        let built = super::builtin_exception_attrs::declare(deferred.class.as_str(), &mut attrs);
        Declared { attrs, built }
    }

    /// Whether `name` is a table attribute built from its named argument
    /// although declared private (`X::AdHoc`'s `$!payload`); what an explicit
    /// `is built` records in `ClassDef::attribute_built`.
    // Cost: O(b), b = such attributes of this class (0 or 1 in practice).
    pub(crate) fn builtin_is_built(&self, name: &str) -> bool {
        self.declared().built.contains(&name)
    }
}

impl std::ops::Deref for ClassAttributes {
    type Target = Vec<ClassAttributeDef>;

    // Cost: O(1) after the first access (see `declared`).
    #[inline]
    fn deref(&self) -> &Vec<ClassAttributeDef> {
        &self.declared().attrs
    }
}

impl std::ops::DerefMut for ClassAttributes {
    // Cost: O(1) after the first access (see `declared`).
    #[inline]
    fn deref_mut(&mut self) -> &mut Vec<ClassAttributeDef> {
        self.declared();
        self.deferred = None;
        &mut self
            .list
            .get_mut()
            .expect("`declared` initialized the list")
            .attrs
    }
}

impl From<Vec<ClassAttributeDef>> for ClassAttributes {
    // Cost: O(1).
    fn from(attrs: Vec<ClassAttributeDef>) -> Self {
        Self {
            deferred: None,
            list: OnceLock::from(Declared {
                attrs,
                built: Vec::new(),
            }),
        }
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
