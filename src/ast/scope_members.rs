//! The statements of one scope, looking through `Stmt::SyntheticBlock`.
//!
//! The parser lowers many single source statements into a scopeless
//! `SyntheticBlock` (a grouped `my ($a, $b)`, `my $x will leave {...} = 1`,
//! `has ($a, $b)`, `sub f {...}(...)`, a declaration with a statement
//! modifier, ...), and a lowering may wrap a statement that is itself such a
//! group, so the wrappers nest. A declaration inside one belongs to the
//! enclosing scope. Every question of the form "what does this scope declare
//! at its own level?" therefore walks the statement list through those
//! wrappers, and nothing else: a real `Stmt::Block` or any other construct
//! with a body is a scope of its own and is not entered.

use super::Stmt;
use std::slice;

/// The members of the scope whose statement list is `stmts`, in source order,
/// with every `Stmt::SyntheticBlock` wrapper (at any depth) replaced by its
/// statements. The wrappers themselves are not yielded.
// Cost: O(n), n = number of statements in `stmts` and the wrappers it holds; no
// allocation until a wrapper nests inside another.
pub(crate) fn scope_members(stmts: &[Stmt]) -> ScopeMembers<'_> {
    ScopeMembers {
        cur: stmts.iter(),
        outer: Vec::new(),
        through_unit_package: false,
        keep_whole: None,
    }
}

/// Iterator returned by [`scope_members`].
pub(crate) struct ScopeMembers<'a> {
    cur: slice::Iter<'a, Stmt>,
    outer: Vec<slice::Iter<'a, Stmt>>,
    through_unit_package: bool,
    keep_whole: Option<fn(&[Stmt]) -> bool>,
}

impl<'a> ScopeMembers<'a> {
    /// Also look through a `unit module`/`unit package` (`Stmt::Package` with
    /// `is_unit`): it wraps the rest of the file, but its declarations are
    /// still the compunit's own unit-scope ones. Only an analysis of a
    /// compunit's top level wants this.
    pub(crate) fn through_unit_package(mut self) -> Self {
        self.through_unit_package = true;
        self
    }

    /// Yield a `SyntheticBlock` whose statements satisfy `keep` as one
    /// member instead of looking through it — for a caller that must handle
    /// such a group as a unit (a bind group compiled as one statement).
    pub(crate) fn keep_whole(mut self, keep: fn(&[Stmt]) -> bool) -> Self {
        self.keep_whole = Some(keep);
        self
    }

    fn enter(&mut self, inner: &'a [Stmt]) {
        let parent = std::mem::replace(&mut self.cur, inner.iter());
        self.outer.push(parent);
    }
}

impl<'a> Iterator for ScopeMembers<'a> {
    type Item = &'a Stmt;

    fn next(&mut self) -> Option<&'a Stmt> {
        loop {
            match self.cur.next() {
                Some(Stmt::SyntheticBlock(inner))
                    if !self.keep_whole.is_some_and(|keep| keep(inner)) =>
                {
                    self.enter(inner)
                }
                Some(Stmt::Package {
                    body,
                    is_unit: true,
                    ..
                }) if self.through_unit_package => self.enter(body),
                Some(stmt) => return Some(stmt),
                None => self.cur = self.outer.pop()?,
            }
        }
    }
}

/// The mutable twin of [`scope_members`]: every statement of the scope,
/// looking through `Stmt::SyntheticBlock` wrappers at any depth.
// Cost: O(n), n = number of statements in `stmts` and the wrappers it holds.
pub(crate) fn scope_members_mut(stmts: &mut [Stmt]) -> ScopeMembersMut<'_> {
    ScopeMembersMut {
        cur: stmts.iter_mut(),
        outer: Vec::new(),
    }
}

/// Iterator returned by [`scope_members_mut`].
pub(crate) struct ScopeMembersMut<'a> {
    cur: slice::IterMut<'a, Stmt>,
    outer: Vec<slice::IterMut<'a, Stmt>>,
}

impl<'a> Iterator for ScopeMembersMut<'a> {
    type Item = &'a mut Stmt;

    fn next(&mut self) -> Option<&'a mut Stmt> {
        loop {
            match self.cur.next() {
                Some(Stmt::SyntheticBlock(inner)) => {
                    let parent = std::mem::replace(&mut self.cur, inner.iter_mut());
                    self.outer.push(parent);
                }
                Some(stmt) => return Some(stmt),
                None => self.cur = self.outer.pop()?,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Expr;
    use crate::value::Value;

    fn lit(n: i64) -> Stmt {
        Stmt::Expr(Expr::Literal(Value::int(n)))
    }

    fn ints(members: Vec<&Stmt>) -> Vec<i64> {
        members
            .into_iter()
            .map(|s| match s {
                Stmt::Expr(Expr::Literal(v)) => v.to_string_value().parse().unwrap(),
                other => panic!("unexpected member {other:?}"),
            })
            .collect()
    }

    #[test]
    fn nested_wrappers_are_flattened_in_order() {
        let stmts = vec![
            lit(1),
            Stmt::SyntheticBlock(vec![
                lit(2),
                Stmt::SyntheticBlock(vec![lit(3), Stmt::SyntheticBlock(vec![])]),
                lit(4),
            ]),
            lit(5),
        ];
        assert_eq!(ints(scope_members(&stmts).collect()), vec![1, 2, 3, 4, 5]);
        let mut stmts = stmts;
        assert_eq!(scope_members_mut(&mut stmts).count(), 5);
    }

    #[test]
    fn real_blocks_are_members_not_entered() {
        let stmts = vec![Stmt::Block(vec![lit(1)]), lit(2)];
        let members: Vec<&Stmt> = scope_members(&stmts).collect();
        assert!(matches!(members[0], Stmt::Block(_)));
        assert_eq!(members.len(), 2);
    }

    #[test]
    fn keep_whole_yields_the_wrapper() {
        let stmts = vec![
            Stmt::SyntheticBlock(vec![Stmt::MarkBind, lit(1)]),
            Stmt::SyntheticBlock(vec![lit(2)]),
        ];
        let members: Vec<&Stmt> = scope_members(&stmts)
            .keep_whole(|inner| inner.iter().any(|s| matches!(s, Stmt::MarkBind)))
            .collect();
        assert_eq!(members.len(), 2);
        assert!(matches!(members[0], Stmt::SyntheticBlock(_)));
    }
}
