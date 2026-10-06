//! A registry table shared copy-on-write (ADR-12026 §2.3).
//!
//! Every interpreter starts from a shared builtin [`super::registry::Registry`],
//! and the first registry write of a process copies it. With the large tables
//! held as [`CowTable`]s that copy is a reference-count bump per table, and a
//! table is copied only when it is itself written. Loading `use Test` writes
//! routines, not classes or roles, so the builtin class and role tables were
//! copied for nothing (1.6M instructions per load).
//!
//! Reads go through `Deref`, so call sites read the table exactly as before;
//! a write goes through `DerefMut`, which copies the table if it is shared.

use std::ops::{Deref, DerefMut};
use std::sync::Arc;

/// See the module docs.
pub(crate) struct CowTable<T>(Arc<T>);

impl<T: Default> Default for CowTable<T> {
    fn default() -> Self {
        CowTable(Arc::new(T::default()))
    }
}

impl<T> Clone for CowTable<T> {
    // Cost: O(1).
    fn clone(&self) -> Self {
        CowTable(Arc::clone(&self.0))
    }
}

impl<T> Deref for CowTable<T> {
    type Target = T;
    // Cost: O(1).
    #[inline]
    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T: Clone> DerefMut for CowTable<T> {
    // Cost: O(1), or O(n) once when the table is still shared (the copy).
    #[inline]
    fn deref_mut(&mut self) -> &mut T {
        Arc::make_mut(&mut self.0)
    }
}

impl<'a, T> IntoIterator for &'a CowTable<T>
where
    &'a T: IntoIterator,
{
    type Item = <&'a T as IntoIterator>::Item;
    type IntoIter = <&'a T as IntoIterator>::IntoIter;
    // Cost: O(1) (the iterator; walking it is the table's own cost).
    fn into_iter(self) -> Self::IntoIter {
        (*self.0).into_iter()
    }
}

impl<T: IntoIterator + Clone> IntoIterator for CowTable<T> {
    type Item = T::Item;
    type IntoIter = T::IntoIter;
    // Cost: O(1) when this is the table's only holder, else O(n) (a copy).
    fn into_iter(self) -> Self::IntoIter {
        Arc::unwrap_or_clone(self.0).into_iter()
    }
}

impl<T> From<T> for CowTable<T> {
    fn from(table: T) -> Self {
        CowTable(Arc::new(table))
    }
}

#[cfg(test)]
mod tests {
    use super::CowTable;
    use std::collections::HashMap;

    #[test]
    fn a_write_copies_only_a_shared_table() {
        let mut a: CowTable<HashMap<&str, u32>> = CowTable::default();
        a.insert("x", 1);
        let b = a.clone();
        a.insert("y", 2);
        assert_eq!(b.len(), 1);
        assert_eq!(a.len(), 2);
        assert_eq!(a.get("x"), Some(&1));
    }
}
