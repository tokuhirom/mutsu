//! The source text of the compilation unit being compiled, for `$*CU`.
//!
//! Rakudo's `$*CU` is the `RakuAST::CompUnit` of the unit under compilation,
//! visible to BEGIN blocks and to what `use` runs at compile time (an `EXPORT`
//! sub reaches the *importing* unit through it); at run time it is a Failure.
//! mutsu has no persistent AST of the in-flight unit, so the unit publishes its
//! source text here for its compile phase and `$*CU` is built from it on first
//! read, once per unit, with the same conversion `Str.AST` uses. Shaped like
//! [`crate::unit_source_file`]: a thread-local stack, a guard per unit.

use crate::value::Value;
use std::cell::RefCell;
use std::rc::Rc;

struct Unit {
    source: Rc<str>,
    /// The unit's compile phase is over (its BEGIN prologue has run).
    compiled: bool,
    cu: Option<Value>,
}

thread_local! {
    static UNITS: RefCell<Vec<Unit>> = const { RefCell::new(Vec::new()) };
}

/// Publishes a unit's source for the lifetime of the guard.
pub(crate) struct UnitSourceTextGuard;

impl UnitSourceTextGuard {
    // Cost: O(n) to copy the source, n = source length.
    pub(crate) fn enter(source: &str) -> Self {
        UNITS.with(|units| {
            units.borrow_mut().push(Unit {
                source: Rc::from(source),
                compiled: false,
                cu: None,
            });
        });
        Self
    }
}

impl Drop for UnitSourceTextGuard {
    fn drop(&mut self) {
        UNITS.with(|units| {
            units.borrow_mut().pop();
        });
    }
}

/// The innermost unit's compile phase is over: `$*CU` is a Failure from here on.
// Cost: O(1).
pub(crate) fn end_compile_phase() {
    UNITS.with(|units| {
        if let Some(unit) = units.borrow_mut().last_mut() {
            unit.compiled = true;
        }
    });
}

/// `$*CU`: the innermost unit still in its compile phase as a
/// `RakuAST::CompUnit`, built on first use. `None` outside a compile phase or
/// when the source does not convert.
// Cost: O(n) on the first read of a unit (parse + conversion), O(1) after;
// n = source length.
pub(crate) fn compile_time_cu() -> Option<Value> {
    enum Step {
        Built(Value),
        Build(Rc<str>),
    }
    let step = UNITS.with(|units| {
        let units = units.borrow();
        let unit = units.last().filter(|unit| !unit.compiled)?;
        Some(match &unit.cu {
            Some(cu) => Step::Built(cu.clone()),
            None => Step::Build(unit.source.clone()),
        })
    })?;
    match step {
        Step::Built(cu) => Some(cu),
        Step::Build(source) => {
            let cu = crate::rakuast::str_dot_ast_compunit(&source).ok()?;
            UNITS.with(|units| {
                if let Some(unit) = units.borrow_mut().last_mut() {
                    unit.cu = Some(cu.clone());
                }
            });
            Some(cu)
        }
    }
}
