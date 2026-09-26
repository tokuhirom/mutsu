//! Named methods declared in expression position, hoisted into their package.
//!
//! `has @.x = method TOP ($/) { ... }` is still a *declaration* in Raku: the
//! routine declarator installs `TOP` as a method of the enclosing class, and
//! the expression evaluates to that method object. The expression itself is
//! parsed as a `my method` (so its value is the named `Method`), and a copy of
//! the declaration is recorded here; [`package_body_block`](super::package_body_block)
//! opens a frame around the class body and appends every recorded declaration
//! to it as an ordinary method statement once the body has parsed.
//!
//! The parser backtracks, so the same source position can be parsed more than
//! once; entries are keyed by the address of the text they were parsed from and
//! a repeat is dropped. Outside any package body (the mainline) no frame is
//! open and nothing is recorded: there the declaration only yields its value,
//! which is what rakudo does (with a "useless declaration" worry).

use crate::ast::Stmt;
use std::cell::RefCell;

thread_local! {
    /// One frame per package body being parsed, innermost last. Each entry is
    /// the source address the declaration was parsed from, and the declaration.
    static FRAMES: RefCell<Vec<Vec<(usize, Stmt)>>> = const { RefCell::new(Vec::new()) };
}

/// Open a frame for a package body about to be parsed.
pub(super) fn push_frame() {
    FRAMES.with(|f| f.borrow_mut().push(Vec::new()));
}

/// Close the innermost frame, returning the declarations recorded in it in
/// source order.
pub(super) fn pop_frame() -> Vec<Stmt> {
    let mut entries = FRAMES.with(|f| f.borrow_mut().pop()).unwrap_or_default();
    // A higher address is later in the source.
    entries.sort_by_key(|(addr, _)| *addr);
    entries.into_iter().map(|(_, stmt)| stmt).collect()
}

/// Record `decl` (parsed from the text starting at `src`) for the innermost
/// open package body. A no-op in the mainline, and for a position already
/// recorded by an earlier (backtracked) attempt.
pub(crate) fn record(src: &str, decl: &Stmt) {
    let addr = src.as_ptr() as usize;
    FRAMES.with(|f| {
        let mut frames = f.borrow_mut();
        let Some(frame) = frames.last_mut() else {
            return;
        };
        if frame.iter().any(|(a, _)| *a == addr) {
            return;
        }
        let mut decl = decl.clone();
        // The value is a lexical `my method`; the hoisted copy is the class's
        // ordinary method.
        if let Stmt::MethodDecl { is_my, .. } = &mut decl {
            *is_my = false;
        }
        frame.push((addr, decl));
    });
}
