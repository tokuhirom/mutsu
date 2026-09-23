//! Folding a CORE type name in an `nqp::` operand to its type object at parse
//! time (ADR-0115).
//!
//! A bareword such as `List` in `nqp::bindattr(@r, List, '$!reified', $b)` was
//! resolved at run time, every time, through the untyped path's full bareword
//! chain (`push_bare_word_value`): import-alias scans, enum probes, env probes,
//! the type registry. JSON::Fast names `List`, `Array`, `IterationBuffer`,
//! `Map`, `Hash`, `Uni` and `NFD` this way once per container or string, which
//! was 10% of a whole SPDX decode (#9122).
//!
//! In Raku such a name is a lexical lookup settled at compile time: it means
//! the CORE type unless something in the compunit binds the same name. The
//! parser is where that is known, and it already folds the CORE term `Any` to
//! its type object on the same terms (`term_literals::keyword_literal`, #9047).
//! This extends the fold to a fixed list of CORE types, in one position only,
//! an `nqp::` operand, where the type object is plain data: no method call,
//! smartmatch, coercion or trait reads it as a name there.
//!
//! The name is folded only when the compunit binds it nowhere. Every scope the
//! parse leaves hands its bound names to this module ([`note_bound`]), and so
//! does every scope still open at the end. That covers subs, types (including
//! imported ones), enum values, sigilless and constant terms, imported
//! functions and imported value terms. The fold is unit-wide on purpose: a
//! name bound in ANY scope of the unit is not folded anywhere in it. A unit that
//! imports through a run-time `sub EXPORT` hook, whose names no scan can see,
//! folds nothing, exactly as the `Any` fold gives way there.

use std::cell::RefCell;
use std::collections::HashSet;

use crate::ast::{CallArg, Expr};
use crate::symbol::Symbol;
use crate::value::Value;

/// The CORE types folded. Each resolves to its own type object in a fresh
/// interpreter, as in rakudo (`::(N).^name eq N`, checked against both).
const FOLDABLE: &[&str] = &[
    "Any",
    "Array",
    "Blob",
    "Buf",
    "Capture",
    "Cool",
    "Exception",
    "Failure",
    "Hash",
    "Int",
    "IterationBuffer",
    "List",
    "Map",
    "Match",
    "Mu",
    "NFC",
    "NFD",
    "NFKC",
    "NFKD",
    "Num",
    "Pair",
    "Range",
    "Rat",
    "Scalar",
    "Seq",
    "Slip",
    "Str",
    "Uni",
];

fn foldable(name: &str) -> Option<&'static str> {
    FOLDABLE.iter().copied().find(|n| *n == name)
}

/// What the unit being parsed binds, and whether it may fold at all.
#[derive(Default)]
pub(crate) struct UnitFold {
    bound: HashSet<&'static str>,
    /// Folding is on for the unit being parsed (set by [`begin_unit`]).
    active: bool,
}

thread_local! {
    static UNIT: RefCell<UnitFold> = RefCell::new(UnitFold::default());
}

/// The enclosing unit's record, put back when the unit's parse ends. Parses
/// nest (a module scan parses from inside a `use`).
pub(crate) struct UnitGuard(Option<UnitFold>);

impl Drop for UnitGuard {
    fn drop(&mut self) {
        if let Some(saved) = self.0.take() {
            UNIT.with(|u| *u.borrow_mut() = saved);
        }
    }
}

/// Start a unit.
pub(crate) fn begin_unit() -> UnitGuard {
    UnitGuard(Some(UNIT.with(|u| {
        std::mem::replace(
            &mut *u.borrow_mut(),
            UnitFold {
                bound: HashSet::new(),
                active: true,
            },
        )
    })))
}

/// Record a name some scope of the unit binds.
pub(crate) fn note_bound(name: &str) {
    if let Some(n) = foldable(name) {
        UNIT.with(|u| {
            u.borrow_mut().bound.insert(n);
        });
    }
}

/// Stop folding for the rest of the unit (a `sub EXPORT` import).
pub(crate) fn disable_unit() {
    UNIT.with(|u| u.borrow_mut().active = false);
}

fn fold(e: &mut Expr) {
    let Expr::BareWord(name) = e else {
        return;
    };
    let Some(n) = foldable(name) else {
        return;
    };
    let ok = UNIT.with(|u| {
        let u = u.borrow();
        u.active && !u.bound.contains(n)
    });
    if ok {
        *e = Expr::Literal(Value::package(Symbol::intern(n)));
    }
}

/// Fold the CORE type barewords among an `nqp::` call's operands.
pub(crate) fn fold_nqp_operands(name: Symbol, args: &mut [Expr]) {
    if name.with_str(|n| n.starts_with("nqp::")) {
        args.iter_mut().for_each(fold);
    }
}

/// [`fold_nqp_operands`] for a statement-level call.
pub(crate) fn fold_nqp_call_args(name: Symbol, args: &mut [CallArg]) {
    if name.with_str(|n| n.starts_with("nqp::")) {
        for a in args {
            if let CallArg::Positional(e) = a {
                fold(e);
            }
        }
    }
}
