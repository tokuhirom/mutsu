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
    /// Closures `$*CU.add-check-code` queued, each with the `CompUnit` it was
    /// called on, for the unit's CHECK phase.
    check_code: Vec<(Value, Value)>,
    /// A module's `sub EXPORT` is running for this unit: `$*CU` is the
    /// importing unit below it, not this one.
    in_export: bool,
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
                check_code: Vec::new(),
                in_export: false,
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

/// The unit `$*CU` names: the innermost one not running a `sub EXPORT` of its
/// own (that one runs on behalf of the unit importing it).
fn active_index(units: &[Unit]) -> Option<usize> {
    units.iter().rposition(|unit| !unit.in_export)
}

/// While alive, the innermost unit is running its `sub EXPORT`, so `$*CU` and
/// `add-check-code` address the unit that imports it.
pub(crate) struct ExportPhaseGuard(Option<usize>);

impl ExportPhaseGuard {
    // Cost: O(1).
    pub(crate) fn enter() -> Self {
        UNITS.with(|units| {
            let mut units = units.borrow_mut();
            let index = units.len().checked_sub(1);
            if let Some(unit) = index.and_then(|i| units.get_mut(i)) {
                unit.in_export = true;
            }
            Self(index)
        })
    }
}

impl Drop for ExportPhaseGuard {
    fn drop(&mut self) {
        UNITS.with(|units| {
            let mut units = units.borrow_mut();
            if let Some(unit) = self.0.and_then(|i| units.get_mut(i)) {
                unit.in_export = false;
            }
        });
    }
}

/// `$cu.add-check-code($code)`: queue `code` on the innermost unit still in
/// its compile phase, to run (with no arguments) at its CHECK phase.
// Cost: O(1) amortized.
pub(crate) fn add_check_code(cu: Value, code: Value) {
    UNITS.with(|units| {
        let mut units = units.borrow_mut();
        let index = active_index(&units);
        // A unit that finished compiling never runs it, and rakudo does not
        // refuse the call either.
        if let Some(unit) = index
            .and_then(|i| units.get_mut(i))
            .filter(|unit| !unit.compiled)
        {
            unit.check_code.push((code, cu));
        }
    });
}

/// The check code queued on the innermost unit, in the order it runs (last
/// queued first, as CHECK phasers do); the queue is emptied.
// Cost: O(c), c = queued closures.
pub(crate) fn take_check_code() -> Vec<(Value, Value)> {
    UNITS.with(|units| {
        let mut units = units.borrow_mut();
        let mut queued = units
            .last_mut()
            .map(|unit| std::mem::take(&mut unit.check_code))
            .unwrap_or_default();
        queued.reverse();
        queued
    })
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
        let unit = active_index(&units)
            .and_then(|i| units.get(i))
            .filter(|unit| !unit.compiled)?;
        Some(match &unit.cu {
            Some(cu) => Step::Built(cu.clone()),
            None => Step::Build(unit.source.clone()),
        })
    })?;
    match step {
        Step::Built(cu) => Some(cu),
        Step::Build(source) => {
            let cu = crate::rakuast::compile_time_compunit(&source).ok()?;
            UNITS.with(|units| {
                let mut units = units.borrow_mut();
                if let Some(unit) = active_index(&units).and_then(|i| units.get_mut(i)) {
                    unit.cu = Some(cu.clone());
                }
            });
            Some(cu)
        }
    }
}
