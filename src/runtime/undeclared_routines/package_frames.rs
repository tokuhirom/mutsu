//! Which scan frame holds a package declaration's body.
//!
//! The BEGIN prologue moves a `class`/`package` body's run-time statements
//! out of the declaration into a [`Stmt::PackageRuntimeBody`] (ADR-0134).
//! Those statements still run in the declaration's lexical scope: a `sub`
//! declared in the class body is callable from them. The scan therefore
//! remembers the frame it opened for each declaration's body and walks the
//! run-time half as a scope nested in it.

use crate::ast::{PackageRuntimeDecl, Stmt};
use crate::symbol::Symbol;
use std::collections::HashMap;

/// A package declaration, as its run-time half names it.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Key {
    Class(u64),
    Package(Symbol),
}

#[derive(Default)]
pub(super) struct PackageFrames {
    /// The frame each walked declaration's body opened.
    frames: HashMap<Key, usize>,
    /// The declaration being walked, with the address of its body's first
    /// statement, until the walk opens the frame for that body.
    pending: Option<(usize, Key)>,
}

impl PackageFrames {
    /// Note a declaration whose body the walk is about to reach.
    // Cost: O(1).
    pub(super) fn enter_decl(&mut self, stmt: &Stmt) {
        let (key, body) = match stmt {
            Stmt::ClassDecl { decl_id, body, .. } => (Key::Class(*decl_id), body),
            Stmt::Package { name, body, .. } => (Key::Package(*name), body),
            _ => return,
        };
        self.pending = Some((body.as_ptr() as usize, key));
    }

    /// The walk opened `frame` for the statement list `body`: record it when
    /// it is the pending declaration's body.
    // Cost: O(1) amortized.
    pub(super) fn frame_opened(&mut self, body: &[Stmt], frame: usize) {
        if let Some((addr, key)) = self.pending
            && addr == body.as_ptr() as usize
        {
            self.pending = None;
            self.frames.insert(key, frame);
        }
    }

    /// The frame of the declaration a run-time body belongs to, once the walk
    /// has seen that declaration.
    // Cost: O(1) amortized.
    pub(super) fn body_frame(&self, decl: PackageRuntimeDecl, name: Symbol) -> Option<usize> {
        let key = match decl {
            PackageRuntimeDecl::Class { decl_id, .. } => Key::Class(decl_id),
            PackageRuntimeDecl::Package => Key::Package(name),
        };
        self.frames.get(&key).copied()
    }
}
